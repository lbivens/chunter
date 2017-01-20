-module(fwadm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(FWADM, "/usr/sbin/fwadm").
-define(OPTS, [{line, 512}, binary, exit_status]).

-export([convert/2, add/3, delete/1,
         list/0, list/1,
         list_fifo/0, list_fifo/1,
         start/1, stop/1]).

-ignore_xref([list_fifo/0]).
%%%===================================================================
%%% fwadm API
%%%===================================================================

-spec add(fifo:org_id(), fifo:vm_id(), fifo:smartos_fw_rule()) ->
                 {error, Reason::term()} |
                 {ok, UUID :: fifo:uuid()}.
add(Owner, VM, Rule) ->
    RuleB = list_to_binary(build(Rule)),
    Desc = base64:encode(term_to_binary({VM, Rule})),
    File = os:cmd("mktemp") -- "\n",
    JSON = #{
      description => <<"fifo:", Desc/binary>>,
      enabled => true,
      owner_uuid => Owner,
      rule => RuleB
     },
    case file:write_file(File, jsone:encode(JSON)) of
        ok ->
            extract_uuid(fwadm("add", [{file, File}]));
        E ->
            E
    end.


delete(UUID) ->
    extract_uuid(fwadm(["delete", UUID])).

-spec list() ->
                  {ok, [{binary(), binary() | fifo:smartos_fw_rule()}]}|
                  {error, Reason::term()}.
list() ->
    case fwadm("list", [json]) of
        {ok, JSON} ->
            {ok, [convert_to_fifo(E) || E <- jsone:decode(JSON)]};
        E ->
            E
    end.

-spec list(UUID :: fifo:vm_id()) ->
                  {ok, [{binary(), binary() | fifo:smartos_fw_rule()}]}|
                  {error, Reason::term()}.
list(VM) ->
    case list() of
        {ok, L} ->
            {ok, lists:filter(is_vm(VM), L)};
        E ->
            E
    end.

-spec list_fifo() ->
                       {ok, [{binary(), binary() | fifo:smartos_fw_rule()}]}|
                       {error, Reason::term()}.
list_fifo() ->
    case fwadm:list() of
        {ok, L} ->
            {ok, lists:filter(fun is_fifo/1, L)};
        E ->
            E
    end.

-spec list_fifo(UUID :: fifo:vm_id()) ->
                       {ok, [{binary(), binary() | fifo:smartos_fw_rule()}]}|
                       {error, Reason::term()}.
list_fifo(VM) ->
    case list_fifo() of
        {ok, L} ->
            {ok, lists:filter(is_vm(VM), L)};
        E ->
            E
    end.


start(VM) ->
    fwadm("start", [VM]).

stop(VM) ->
    fwadm("stop", [VM]).
%%%===================================================================
%%% fifo conversion
%%%===================================================================

-spec convert(fifo:vm_id(), fifi:firewall_rule()) ->
                     [fifo:smartos_fw_rule()].

%% TODO: We should group that up to 20 later on.
convert(VM, {Action, inbound, Src, {Proto, Filter}}) ->
    [{Action, [S], [{vm, VM}], Proto, Filter} || S <- convert_target(Src)];

convert(VM, {Action, outbound, Dst, {Proto, Filter}}) ->
    [{Action, [{vm, VM}], [D], Proto, Filter} || D <- convert_target(Dst)];

convert(VM, {Action, Dst, inbound, Src, {Proto, Filter}}) ->
    %% Inbound is the reverse of outboutl, go figure!
    convert(VM, {Action, Src, outbound, Dst, {Proto, Filter}});

convert(VM, {Action, Src, outbound, Dst, {Proto, Filter}}) ->
    [{Action, [S], [D], Proto, Filter} ||
        S <- convert_vm(VM, Src), D <- convert_target(Dst)].

build({Action, Src, Dst, icmp, Tags}) ->
    [build1(Action, Src, Dst), "icmp ", build_filter(Tags)];

build({Action, Src, Dst, Protocol, all})
  when Protocol =:= udp; Protocol =:= tcp ->
    [build1(Action, Src, Dst), atom_to_list(Protocol), " PORT all"];

build({Action, Src, Dst, Protocol, Ports})
  when Protocol =:= udp; Protocol =:= tcp ->
    [build1(Action, Src, Dst), atom_to_list(Protocol),
     " ", build_filter(Ports)].

%%%===================================================================
%%% Internal
%%%===================================================================

extract_uuid({ok, <<"Added rules:", Data/binary>>}) ->
    {ok, extract_uuid(Data, <<>>)};
extract_uuid({ok, <<"Deleted rules:", Data/binary>>}) ->
    extract_uuid(Data, <<>>);
extract_uuid(Res) ->
    Res.

extract_uuid(<<C, R/binary>>, Acc) when C =/= $\s ->
    extract_uuid(R, <<Acc/binary, C>>);
extract_uuid(_, Acc) ->
    Acc.

is_vm(VM) ->
    fun(JSX) ->
            case jsxd:get(<<"vm">>, JSX) of
                {ok, VM} ->
                    true;
                _ ->
                    false
            end
    end.

is_fifo(JSX) ->
    case jsxd:get(<<"vm">>, JSX) of
        {ok, _} ->
            true;
        _ ->
            false
    end.


convert_to_fifo(JSXD = #{<<"description">> := <<"fifo:", Base64/binary>>}) ->
    {VM, Rule} = decode_fifo(Base64),
    jsxd:thread([{delete, <<"description">>},
                 {set, <<"vm">>, VM},
                 {set, <<"rule">>, Rule}],
                JSXD);
convert_to_fifo(JSXD) ->
    JSXD.

-spec decode_fifo(binary()) ->
                         {fifo:vm_id(), fifo:smartos_fw_rule()}.
decode_fifo(Base64) ->
    binary_to_term(base64:decode(Base64)).


fwadm(Cmd, Args) ->
    fwadm([Cmd] ++ fwadm_opts(Args, [])).

fwadm(Args) ->
    lager:info("[fwadm] ~s args: ~p", [?FWADM, Args]),
    P = erlang:open_port({spawn_executable, ?FWADM}, [{args, Args} | ?OPTS]),
    read_result(P).


fwadm_opts([{desc, Desc} | R] , Args) ->
    fwadm_opts(R, ["--desc", Desc | Args]);
fwadm_opts([enable | R] , Args) ->
    fwadm_opts(R, ["--enable" | Args]);
fwadm_opts([global | R] , Args) ->
    fwadm_opts(R, ["--global" | Args]);
fwadm_opts([dryrun | R] , Args) ->
    fwadm_opts(R, ["--dryrun" | Args]);
fwadm_opts([{owner, UUID} | R] , Args) ->
    fwadm_opts(R, ["--owner_uuid", UUID | Args]);
fwadm_opts([{file, File} | R] , Args) ->
    fwadm_opts(R, ["-f", File | Args]);
fwadm_opts([json | R] , Args) ->
    fwadm_opts(R, ["--json" | Args]);
fwadm_opts([B | R] , Args) when is_binary(B) ->
    fwadm_opts(R,  [B | Args]);
fwadm_opts([_ | R] , Args) ->
    fwadm_opts(R,  Args);
fwadm_opts([] , Args) ->
    Args.

read_result(P) ->
    read_result(P, <<>>).

read_result(P, Acc) ->
    receive
        {P, {data, {_, Data}}} ->
            read_result(P, <<Acc/binary, Data/binary>>);
        {P, {exit_status, 0}} -> {ok, Acc};
        {P, {exit_status, N}} -> {error, N, Acc}
    end.
-spec convert_target(fifo:fw_target()) ->
                            fifo:smartos_fw_targets().
convert_target({vm, _UUID}) ->
    %% TODO: get the VM's interfaces
    [any];
convert_target({cluster, _UUID}) ->
    %% TODO: get the VM's and the interfaces in the cluster
    [any];
convert_target({stack, _UUID}) ->
    %% TODO: get the VM's and the interfaces in the stack
    [any];
convert_target({network, _UUID}) ->
    %% TODO: get the subnets in the stack
    [any];
%% Those are the supported targets so far
convert_target({ip, IP}) ->
    [{ip, IP}];

convert_target({subnet, Network, Mask}) ->
    [{subnet, Network, Mask}];

convert_target(all) ->
    [any].

-spec convert_vm(UUID::fifo:vm_id(), fifo:fw_iface()) ->
                        [fifo:smartos_fw_targets()].
convert_vm(VM, {nic, _IFance}) ->
    %% {ip, ...};
    [{vm, VM}];
convert_vm(VM, {network, _UUID}) ->
    %% {subnset, ...};
    [{vm, VM}];
convert_vm(VM, all) ->
    [{vm, VM}].

build1(allow, Src, Dst) ->
    ["FROM ", build_targets(Src), " TO ", build_targets(Dst), " ALLOW "];
build1(block, Src, Dst) ->
    ["FROM ", build_targets(Src), " TO ", build_targets(Dst), " BLOCK "].

build_filter([F]) ->
    build_filter_element(F);
build_filter(Fs) ->
    [$(, build_filter1(Fs), $)].

build_filter1([F]) ->
    build_filter_element(F);
build_filter1([F | R]) ->
    [build_filter_element(F), " AND ", build_filter1(R)].

build_filter_element(P) when is_integer(P) ->
    ["PORT ", integer_to_list(P)];
build_filter_element({icmp, Type}) ->
    ["TYPE ", integer_to_list(Type)];
build_filter_element({icmp, Type, Code}) ->
    ["TYPE ", integer_to_list(Type), " CODE ", integer_to_list(Code)].

build_targets([T]) ->
    build_target(T);
build_targets(Ts) ->
    [$(, build_targets1(Ts), $)].

build_targets1([T]) ->
    build_target(T);
build_targets1([T | R]) ->
    [build_target(T), " OR ", build_targets(R)].

build_target(any) ->
    "any";
build_target({vm, all}) ->
    "all vms";
build_target({vm, UUID}) ->
    ["vm ", UUID];
build_target({ip, IP}) ->
    ["ip ", ft_iprange:to_bin(IP)];
build_target({subnet, Base, Mask}) ->
    ["subnet ", ft_iprange:to_bin(Base), $/, integer_to_list(Mask)];
build_target({tag, T}) ->
    ["tag ", encode_tag(T)];
build_target({tag, T, V}) when is_binary(V) ->
    ["tag ", encode_tag(T), " = ", encode_tag(V)];
build_target({tag, T, V}) when is_integer(V) ->
    ["tag ", encode_tag(T), " = ", integer_to_list(V)].


encode_tag(Tag) ->
    <<$", (encode_tag(Tag, <<>>))/binary, $">>.

encode_tag(<<$\\, R/binary>>, Acc) ->
    encode_tag(R, <<Acc/binary, $\\, $\\>>);
encode_tag(<<$", R/binary>>, Acc) ->
    encode_tag(R, <<Acc/binary, $\\, $">>);
encode_tag(<<C, R/binary>>, Acc) ->
    encode_tag(R, <<Acc/binary, C>>);
encode_tag(<<>>, Acc) ->
    Acc.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

tag_test() ->
    ?assertEqual(<<"\"tag\"">>, encode_tag(<<"tag">>)),
    ?assertEqual(<<"\"a tag\"">>, encode_tag(<<"a tag">>)),
    ?assertEqual(<<"\"a \\\\ tag\"">>, encode_tag(<<"a \\ tag">>)),
    ?assertEqual(<<"\"a \\\" tag\"">>, encode_tag(<<"a \" tag">>)),
    ?assertEqual(<<"\"a \\\\\\\" tag\"">>, encode_tag(<<"a \\\" tag">>)),
    ok.

mkfilter(P) ->
    binary_to_list(list_to_binary(build_filter(P))).

filter_test() ->
    ?assertEqual("PORT 5", mkfilter([5])),
    ?assertEqual("(PORT 5 AND PORT 6)", mkfilter([5, 6])),
    ?assertEqual("(PORT 5 AND PORT 6 AND PORT 7)", mkfilter([5, 6, 7])),
    ok.

bt(T) ->
    binary_to_list(list_to_binary(build_target(T))).

build_target_test() ->
    ?assertEqual("vm abc", bt({vm, "abc"})),
    ?assertEqual("ip 1.2.3.4", bt({ip, 16#01020304})),
    ?assertEqual("subnet 10.0.0.0/24", bt({subnet, 16#0A000000, 24})),
    ?assertEqual("tag \"test\"", bt({tag, <<"test">>})),
    ?assertEqual("tag \"test\" = \"toast\"",
                 bt({tag, <<"test">>, <<"toast">>})),
    ok.


b(T) ->
    binary_to_list(list_to_binary(build(T))).

build_test() ->
    ?assertEqual(
       "FROM all vms TO ip 10.2.0.1 BLOCK tcp PORT 25",
       b({block, [{vm, all}], [{ip, 16#0A020001}], tcp, [25]})),
    ?assertEqual(
       "FROM any TO vm 04128191-d2cb-43fc-a970-e4deefe970d8 ALLOW tcp PORT 80",
       b({allow, [any],
          [{vm, "04128191-d2cb-43fc-a970-e4deefe970d8"}], tcp, [80]})),
    ?assertEqual(
       "FROM subnet 10.8.0.0/16 TO vm 0f570678-c007-4610-a2c0-bbfcaab9f4e6 "
       "ALLOW tcp PORT 443",
       b({allow, [{subnet, 16#0A080000, 16}],
          [{vm, "0f570678-c007-4610-a2c0-bbfcaab9f4e6"}], tcp, [443]})),
    ?assertEqual(
       "FROM all vms TO tag \"syslog\" ALLOW udp PORT 514",
       b({allow, [{vm, all}],
          [{tag, <<"syslog">>}], udp, [514]})),
    ?assertEqual(
       "FROM tag \"role\" = \"db\" TO tag \"role\" = \"www\" ALLOW tcp "
       "PORT 5432",
       b({allow, [{tag, <<"role">>, <<"db">>}],
          [{tag, <<"role">>, <<"www">>}], tcp, [5432]})),
    ?assertEqual(
       "FROM all vms TO tag \"VM type\" = \"LDAP server\" ALLOW tcp PORT 389",
       b({allow, [{vm, all}],
          [{tag, <<"VM type">>, <<"LDAP server">>}], tcp, [389]})),
    ?assertEqual(
       "FROM all vms TO all vms ALLOW tcp PORT 22",
       b({allow, [{vm, all}],
          [{vm, all}], tcp, [22]})),
    ?assertEqual(
       "FROM any TO all vms ALLOW tcp PORT 80",
       b({allow, [any],
          [{vm, all}], tcp, [80]})),


    ?assertEqual(
       "FROM (vm 163dcedb-828d-43c9-b076-625423250ee2 OR tag \"db\") TO "
       "(subnet 10.2.2.0/24 OR ip 10.3.0.1) BLOCK tcp PORT 443",
       b({block,
          [{vm, "163dcedb-828d-43c9-b076-625423250ee2"}, {tag, <<"db">>}],
          [{subnet, 16#0A020200, 24}, {ip, 16#0A030001}], tcp, [443]})),

    ?assertEqual(
       "FROM any TO all vms BLOCK tcp PORT 143",
       b({block, [any], [{vm, all}], tcp, [143]})),

    ?assertEqual(
       "FROM all vms TO any ALLOW tcp PORT 25",
       b({allow, [{vm, all}], [any], tcp, [25]})),

    ?assertEqual(
       "FROM tag \"www\" TO any ALLOW tcp (PORT 80 AND PORT 443)",
       b({allow, [{tag, <<"www">>}], [any], tcp, [80, 443]})),

    ?assertEqual(
        "FROM any TO all vms ALLOW icmp TYPE 8 CODE 0",
       b({allow, [any], [{vm, all}], icmp, [{icmp, 8, 0}]})),

    ?assertEqual(
       "FROM all vms TO any BLOCK icmp TYPE 0",
       b({block, [{vm, all}], [any], icmp, [{icmp, 0}]})),

    ok.

-endif.
