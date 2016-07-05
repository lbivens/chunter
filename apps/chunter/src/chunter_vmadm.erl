%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vmadm).

%% API
-export([start/1,
         start/2,
         stop/1,
         force_stop/1,
         info/1,
         reboot/1,
         force_reboot/1,
         delete/1,
         create/2,
         update/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

zoneadm(UUID, uninstall) ->
    zoneadmf(UUID, <<"uninstall">>);
zoneadm(UUID, SubCmd) when is_atom(SubCmd) ->
    zoneadm(UUID, atom_to_list(SubCmd));
zoneadm(UUID, SubCmd) when is_list(SubCmd) ->
    zoneadm(UUID, list_to_binary(SubCmd));

zoneadm(UUID, SubCmd) ->
    Cmd = <<"/usr/sbin/zoneadm -z ", UUID/binary, " ", SubCmd/binary>>,
    lager:debug("zoneadm:cmd - ~s.", [Cmd]),
    R = os:cmd(binary_to_list(Cmd)),
    lager:debug("[zoneadm] ~s", [R]),
    R.

zoneadmf(UUID, SubCmd) ->
    Cmd = <<"/usr/sbin/zoneadm -z ", UUID/binary, " ", SubCmd/binary, " -F">>,
    lager:debug("zoneadm:cmd - ~s.", [Cmd]),
    R = os:cmd(binary_to_list(Cmd)),
    lager:debug("[zoneadm] ~s", [R]),
    R.

zonecfg(UUID, SubCmd) when is_atom(SubCmd) ->
    zonecfg(UUID, atom_to_list(SubCmd));
zonecfg(UUID, SubCmd) when is_list(SubCmd) ->
    zonecfg(UUID, list_to_binary(SubCmd));

zonecfg(UUID, SubCmd) ->
    Cmd = <<"/usr/sbin/zonecfg -z ", UUID/binary, " ", SubCmd/binary>>,
    lager:debug("zonecfg:cmd - ~s.", [Cmd]),
    R = os:cmd(binary_to_list(Cmd)),
    lager:debug("[zonecfg] ~s", [R]),
    R.

%% zonecfgf(UUID, SubCmd) ->
%%     Cmd = <<"/usr/sbin/zonecfg -z ", UUID/binary, " ",
%%             SubCmd/binary, " -F">>,
%%     lager:debug([{fifi_component, chunter}],
%%                 "zonecfg:cmd - ~s.", [Cmd]),
%%     R = os:cmd(binary_to_list(Cmd)),
%%     lager:debug("[zonecfg] ~s", [R]),
%%     R.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec start(UUID::fifo:uuid()) -> list().
start(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info("vmadm:start - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
            lager:debug("vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        S when S =:= omnios; S =:= solaris ->
            lager:info("zoneadm:start - UUID: ~s.", [UUID]),
            zoneadm(UUID, boot)
    end.

-spec start(UUID::fifo:uuid(), Image::binary()) -> list().

start(UUID, Image) ->
    case chunter_utils:system() of
        smartos ->
            lager:info("vmadm:start - UUID: ~s, Image: ~s.", [UUID, Image]),
            Cmd = <<"/usr/sbin/vmadm start ", UUID/binary>>,
            lager:debug("vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        S when S =:= omnios; S =:= solaris ->
            lager:info("zoneadm:start - UUID: ~s, Image: ~s.", [UUID, Image]),
            zoneadm(UUID, boot)
    end.

-spec delete(UUID::fifo:uuid()) -> string().

delete(UUID) ->
    R1 = case chunter_utils:system() of
             smartos ->
                 lager:info("vmadm:delete - UUID: ~s.", [UUID]),
                 Cmd = <<"/usr/sbin/vmadm delete ", UUID/binary>>,
                 lager:debug("vmadm:cmd - ~s.", [Cmd]),
                 R = os:cmd(binary_to_list(Cmd)),
                 lager:debug("[vmadm] ~s", [R]),
                 R;
             S when S =:= omnios; S =:= solaris ->
                 VM = chunter_zone:get(UUID),
                 force_stop(UUID),
                 lager:info("zoneadm:uninstall - UUID: ~s / ~p.", [UUID, VM]),
                 zoneadm(UUID, uninstall),
                 zonecfg(UUID, delete),
                 {ok, Nics} = jsxd:get(<<"nics">>, VM),
                 lists:map(fun(N) ->
                                   {ok, IFace} = jsxd:get(<<"interface">>, N),
                                   chunter_nic_srv:delete(IFace),
                                   $. %% This is so we have a nice list of dots
                           end, Nics)
         end,
    chunter_server:update_mem(),
    R1.

-spec info(UUID::fifo:uuid()) -> fifo:config_list().

%% JSX is spected badly for the jsx:decode so we ignore this function!
-dialyzer({nowarn_function, decode_info/1}).
info(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info("vmadm:info - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm info ", UUID/binary>>,
            lager:debug("vmadm:cmd - ~s.", [Cmd]),
            Output = os:cmd(binary_to_list(Cmd)),
            decode_info(Output);
        S when S =:= omnios; S =:= solaris ->
            {error, no_info}
    end.
decode_info("Unable" ++ _) ->
    {error, no_info};

decode_info(JSON) ->
    case jsx:decode(list_to_binary(JSON)) of
        {incomplete, _} ->
            {error, no_info};
        R ->
            {ok, R}
    end.

-spec stop(UUID::fifo:uuid()) -> list().

stop(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info("vmadm:stop - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm stop ", UUID/binary>>,
            lager:debug("vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        S when S =:= omnios; S =:= solaris ->
            lager:info("zoneadm:stop - UUID: ~s.", [UUID]),
            zoneadm(UUID, shutdown)
    end.

-spec force_stop(UUID::fifo:uuid()) -> list().

force_stop(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info("vmadm:force-stop - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm stop ", UUID/binary, " -F">>,
            lager:debug("vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        S when S =:= omnios; S =:= solaris ->
            lager:info("vmadm:force-stop - UUID: ~s.", [UUID]),
            zoneadm(UUID, halt)
    end.

-spec reboot(UUID::fifo:uuid()) -> list().

reboot(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info("vmadm:reboot - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm reboot ", UUID/binary>>,
            lager:debug("vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        S when S =:= omnios; S =:= solaris ->
            lager:info("vmadm:reboot - UUID: ~s.", [UUID]),
            zoneadm(UUID, "shutdown -r")
    end.

-spec force_reboot(UUID::fifo:uuid()) -> list().

force_reboot(UUID) ->
    case chunter_utils:system() of
        smartos ->
            lager:info("vmadm:reboot - UUID: ~s.", [UUID]),
            Cmd = <<"/usr/sbin/vmadm reboot ", UUID/binary, " -F">>,
            lager:debug("vmadm:cmd - ~s.", [Cmd]),
            R = os:cmd(binary_to_list(Cmd)),
            lager:debug("[vmadm] ~s", [R]),
            R;
        S when S =:= omnios; S =:= solaris ->
            lager:info("vmadm:reboot - UUID: ~s.", [UUID]),
            zoneadm(UUID, reboot)
    end.

-spec create(UUID::binary(), Data::fifo:vm_config()) -> ok |
                                                        {error, binary() |
                                                         timeout |
                                                         unknown}.

create(UUID, Data) ->
    lager:info("New Create: ~p", [Data]),
    lager:info("Creation of VM '~s' started.", [UUID]),
    lager:info("vmadm:create"),
    Cmd = "/usr/sbin/vmadm create",
    lager:debug("vmadm:cmd - ~s.", [Cmd]),
    Port = port_json(Cmd, Data),
    lager:info("vmadm:create - handed to vmadm, waiting ...", []),
    timer:apply_after(5000, chunter_server, update_mem, []),
    Res = case wait_for_text(Port, UUID, 60*10) of
              ok ->
                  lager:info("vmadm:create - vmadm returned sucessfully.", []),
                  libhowl:send(<<"command">>,
                               [{<<"event">>, <<"vm-create">>},
                                {<<"uuid">>, fifo_utils:uuid()},
                                {<<"data">>,
                                 [{<<"uuid">>, UUID}]}]),
                  chunter_vm_fsm:load(UUID);
              {error, E} ->
                  delete(UUID),
                  lager:error("vmad:create - Failed: ~p.", [E]),
                  {error, E}
          end,
    lager:info("vmadm:create - updating memory.", []),
    chunter_server:update_mem(),
    ls_vm:creating(UUID, false),
    Res.

update(UUID, Data) ->
    lager:info("~p", [<<"Updating of VM '", UUID/binary, "' started.">>]),
    lager:info("vmadm:update", []),
    Cmd =  code:priv_dir(chunter) ++ "/vmadm_wrap.sh update " ++
        binary_to_list(UUID),
    lager:debug("vmadm:cmd - ~s.", [Cmd]),
    Port = port_json(Cmd, Data),
    port_command(Port, "\nEOF\n"),
    wait_for_result(Port, UUID).

wait_for_result(Port, UUID) ->
    receive
        {Port, {data, {eol, Data}}} ->
            lager:debug("[vmadm] ~s", [Data]),
            % take good care of the `exit_status` message
            wait_for_result(Port, UUID);
        {Port, {data, Data}} ->
            lager:debug("[vmadm] ~s", [Data]),
            wait_for_result(Port, UUID);
        {Port, {exit_status, 0}} ->
            chunter_server:update_mem(),
            chunter_vm_fsm:load(UUID);
        {Port, {exit_status, E}} ->
            chunter_server:update_mem(),
            chunter_vm_fsm:load(UUID),
            {error, E}
    after
        60000 ->
            chunter_server:update_mem(),
            chunter_vm_fsm:load(UUID)
    end.

wait_for_text(Port, Lock, Max) ->
    wait_for_text(Port, Lock, Max, erlang:system_time(seconds)).

wait_for_text(Port, Lock, Max, T0) ->
    receive
        {Port, {data, {eol, Data}}} ->
            lager:debug("[vmadm] ~s", [Data]),
            relock(Lock),
            wait_for_text(Port, Lock, Max, T0);
        {Port, {data, Data}} ->
            lager:debug("[vmadm] ~s", [Data]),
            relock(Lock),
            wait_for_text(Port, Lock, Max, T0);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, S}} ->
            {error, S}
    after
        3000 ->
            case erlang:system_time(seconds) - T0 of
                _To when _To  > Max ->
                    lager:debug("[vmadm] timeout after ~ps", [Max]);
                _ ->
                    relock(Lock),
                    wait_for_text(Port, Lock, Max, T0)
            end
    end.

relock(undefined) ->
    ok;
relock(Lock) ->
    chunter_lock:lock(Lock).



%%%===================================================================
%%% Internal functions
%%%===================================================================

port_json(Cmd, JSON) ->
    Port = open_port({spawn, Cmd}, [use_stdio, binary, {line, 1000},
                                    stderr_to_stdout, exit_status]),
    port_command(Port, jsx:encode(JSON)),
    Port.
