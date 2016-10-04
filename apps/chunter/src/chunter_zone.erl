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


-export([ex2/0]).
-ignore_xref([ex2/0, get_raw/1]).


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
                    zoneadm_list(), ID =/= <<"0">>]
    end.

-spec get(ZUUID::fifo:uuid()) -> fifo:vm_config() | {error, not_found}.

get(ZUUID) ->
    case [chunter_zoneparser:load(#{<<"name">> => Name,
                                    <<"state">> => VMState,
                                    <<"zonepath">> => Path,
                                    <<"type">> => Type}) ||
             {_ID, Name, VMState, Path, _UUID, Type} <- get_raw(ZUUID)] of
        [VM | _] ->
            VM;
        [] ->
            {error, not_found}
    end.

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
                [ID, UUID, VMState, Path, _UUID, Type, _IP | _] <- Zones]
    end.

ex2() ->
    zonecfg(
      [create,
       {zonepath, <<"/zones/2398fe7c-032f-11e5-abb0-b33f9f953915">>},
       {brand, ipkg},
       {autoboot, true},
       {limitpriv, default},
       {'ip-type', exclusive},
       {add, net,
        [{physical, <<"net0">>},
         {'mac-addr', <<"12:1e:96:73:04:17">>},
         {'global-nic', admin},
         {property, gateway, "192.168.1.1"},
         {property, ip, "192.168.1.44"},
         {property, netmask, "255.255.255.0"},
         {property, primary, "true"}
        ]},
       {rctl, <<"zone.cpu-shares">>, privileged, 100, none},
       {rctl, <<"zone.max-lwps">>, privileged, 2000, deny},
       {rctl, <<"zone.max-msg-ids">>, privileged, 4096, deny},
       {rctl, <<"zone.max-sem-ids">>, privileged, 4096, deny},
       {rctl, <<"zone.max-shm-ids">>, privileged, 4096, deny},
       {rctl, <<"zone.max-shm-memory">>, privileged, 3221225472, deny},
       {rctl, <<"zone.zfs-io-priority">>, privileged, 100, none},
       {rctl, <<"zone.cpu-cap">>, privileged, 400, deny},
       {rctl, <<"zone.max-physical-memory">>, privileged, 6442450944, deny},
       {rctl, <<"zone.max-locked-memory">>, privileged, 6442450944, deny},
       {rctl, <<"zone.max-swap">>, privileged, 6442450944, deny},
       {attr, <<"vm-version">>, string, 1},
       {attr, <<"create-timestamp">>, string, <<"2015-04-26T11:29:31.297Z">>},
       {attr, <<"billing-id">>, string,
        <<"00000000-0000-0000-0000-000000000000">>},
       {attr, <<"owner-uuid">>, string,
        <<"00000000-0000-0000-0000-000000000000">>},
       {attr, <<"hostname">>, string, <<"fifo01">>},
       {attr, <<"dns-domain">>, string, <<"local">>},
       {attr, <<"resolvers">>, string, <<"8.8.8.8,8.8.4.4">>},
       {attr, <<"alias">>, string, <<"ZmlmbzAx">>},
       {attr, <<"tmpfs">>, string, 6144}]).

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
