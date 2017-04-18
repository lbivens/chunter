%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_zone).

-define(ZONEADM, "/usr/sbin/zoneadm").


-export([list/0, list_/0, get/1, get_raw/1, zonecfg/1]).


-ignore_xref([get_raw/1]).


list() ->
    %% TODO: find a way to unify this!
    [chunter_zoneparser:load(#{<<"name">> => Name,
                               <<"uuid">> => UUID}) ||
        #{name := Name, uuid := UUID} <- list_()].

list_() ->
    case chunter_utils:system() of
        smartos ->
            [#{uuid => UUID, name => Name, state => VMState} ||
                %% SmartOS seems to have one more coumn
                [ID, Name, VMState, _Path, UUID, _Type, _IP | _] <-
                    zoneadm_list(), ID =/= <<"0">>];
        S when S =:= omnios; S =:= solaris ->
            [#{uuid => UUID, name => UUID, state => VMState} ||
                %% SmartOS seems to have one more coumn
                [ID, UUID, VMState, _Path, _OtherUUID, _Type, _IP | _] <-
                    zoneadm_list(), ID =/= <<"0">>];
        freebsd ->
            {ok, L} = iocage:list(),
            L1 = re:split(L, "\n"),
            L2 = [E || E <- L1, E =/= <<>>],
            L3 = [re:split(E, "\t") || E <- L2],
            [#{uuid => UUID, name => UUID,
               state => chunter_zonemon:simplifie_state(State)} ||
                [_ID, UUID, _Boot, State, _Tag, _Type, _IP, _Release, _TPL]
                    <- L3]
    end.

-spec get(ZUUID::fifo:uuid()) -> {ok, fifo:vm_config()} | {error, not_found}.

get(ZUUID) ->
    case [chunter_zoneparser:load(#{<<"name">> => Name,
                                    <<"state">> => VMState,
                                    <<"zonepath">> => Path,
                                    <<"type">> => Type}) ||
             {_ID, Name, VMState, Path, _UUID, Type} <- get_raw(ZUUID)] of
        [{error, not_found} | _] ->
            {error, not_found};
        [VM | _] ->
            {ok, apply_resolvers(ZUUID, VM)};
        [] ->
            {error, not_found}
    end.

%% The way vmadm handles maintain_resolvers is different to other boolean fields
%% so we got to adapt for that and actively set it false when it's not true.
apply_resolvers(ZUUID, VM) ->
    VM1 = jsxd:update([<<"maintain_resolvers">>], fun(V) -> V end, false, VM),
    apply_indestructible(ZUUID, VM1).

apply_indestructible(ZUUID, VM) ->
    VM#{
      <<"indestructible_zoneroot">> => indestructible_zoneroot(ZUUID),
      <<"indestructible_delegated">> => indestructible_delegated(ZUUID)
     }.

-spec get_raw(ZUUID::fifo:uuid()) -> [{ID::binary(),
                                       Name::binary(),
                                       VMState::binary(),
                                       Path::binary(),
                                       UUID::binary(),
                                       Type::binary()}].

get_raw(ZUUID) when is_binary(ZUUID) ->
    UUIDs = binary_to_list(ZUUID),
    case chunter_utils:system() of
        smartos ->
            Zones = [ re:split(Line, ":")
                      || Line <- re:split(os:cmd([?ZONEADM, " -u ", UUIDs,
                                                  " list -p"]), "\n")],
            [{ID, Name, VMState, Path, UUID, Type} ||
                [ID, Name, VMState, Path, UUID, Type, _IP | _] <- Zones];
        S when S =:= omnios; S =:= solaris ->
            Zones = [ re:split(Line, ":")
                      || Line <- re:split(os:cmd([?ZONEADM, " -z ", UUIDs,
                                                  " list -p"]), "\n")],
            [{ID, UUID, VMState, Path, UUID, Type} ||
                [ID, UUID, VMState, Path, _UUID, Type, _IP | _] <- Zones];
        freebsd ->
            {ok, L} = iocage:list(),
            L1 = re:split(L, "\n"),
            L2 = [re:split(E, "\t") || E <- L1, E =/= <<>>],
            [{ID, ZUUID, chunter_zonemon:simplifie_state(State),
              <<"/iocage/", ZUUID/binary>>, ZUUID, <<"jail">>}
             || [ID, UUID, _Boot, State, _Tag, _Type, _IP, _Release, _TPL]
                    <- L2, UUID =:= ZUUID]
    end.

%% zonecfg -z 2398fe7c-032f-11e5-abb0-b33f9f953915 delete -F

%% zoneadm: WARNING: Ignoring unrecognized rctl 'zone.max-physical-memory'.
%% zoneadm -z 2398fe7c-032f-11e5-abb0-b33f9f953915 install
%% zoneadm -z 2398fe7c-032f-11e5-abb0-b33f9f953915 boot

%% WARNING: skipping network interface 'net0': object not found
%% WARNING: The zone.cpu-shares rctl is set but
%% FSS is not the default scheduling class for
%% this zone.  FSS will be used for processes
%% in the zone but to get the full benefit of FSS,
%% it should be the default scheduling class.
%% See dispadmin(1M) for more details.
%% failed to add network device: Error 0
%% zoneadm: call to zoneadmd failed

zonecfg(L) ->
    [zonecfg1(C) || C <- L].

zonecfg1(create) ->
    "create\n";

zonecfg1(verify) ->
    "verify\n";

zonecfg1(commit) ->
    "commit\n";

zonecfg1(exit) ->
    "exit\n";


zonecfg1({N, V}) when is_atom(N) ->
    set(N, V);


zonecfg1({rctl, Name, Priv, Limit, Action}) ->
    ["add rctl\n",
     "set name=", v(Name), $\n,
     "add value ", rctl_val(Priv, Limit, Action), $\n,
     "end\n"];

zonecfg1({property, Name, Value}) ->
    ["add property ",
     val_list([{name, Name}, {value, ["\"" , Value, "\""]}]),
     $\n];

zonecfg1({add, What, Opts}) ->
    add(What, Opts);

zonecfg1({attr, Name, Type, Value}) ->
    add(attr,
        [{name, Name},
         {type, Type},
         {value, Value}]).

add(Type, Values) ->
    ["add ", v(Type), $\n,
     [zonecfg1(Value) || Value <- Values],
     "end\n"].

set(Name, Value) ->
    ["set ", v(Name), "=", v(Value), $\n].

rctl_val(Priv, Limit, Action) ->
    val_list([{priv, Priv}, {limit, Limit}, {action, rctl_action(Action)}]).

val_list(Vs) ->
    [$(, string:join([[v(N), $=, v(V)] || {N, V} <- Vs], ","),  $)].

rctl_action(none) ->
    "none";
rctl_action(deny) ->
    "deny".

v(I) when is_integer(I) ->
    integer_to_list(I);

v(B) when is_binary(B) ->
    B;

v({list, L}) ->
    string:join([v(E) || E <- L], ",");

v([_E | _] = L) when not is_integer(_E) ->
    v({list, L});

v(L) when is_list(L) ->
    L;
v(A) when is_atom(A) ->
    atom_to_list(A).

zoneadm_list() ->
    [re:split(Line, ":")
     || Line <- re:split(os:cmd("/usr/sbin/zoneadm list -ip"), "\n")].

indestructible(D) ->
    case chunter_zfs:list(<<D/binary, "@indestructible">>) of
        {ok, _} ->
            true;
        _ ->
            false
    end.
indestructible_zoneroot(UUID) ->
    indestructible(<<"zones/", UUID/binary>>).
indestructible_delegated(UUID) ->
    indestructible(<<"zones/", UUID/binary, "/data">>).
