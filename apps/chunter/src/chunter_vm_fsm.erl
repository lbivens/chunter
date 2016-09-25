%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_vm_fsm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_fsm).
-behaviour(ezdoor_behaviour).
-define(CACHE_SIZE, 10240).

%% API
-export([start_link/1, door_event/3]).
-ignore_xref([start_link/1,
              preloading/2,
              initialized/2,
              creating/2,
              loading/2,
              stopped/2,
              booting/2,
              running/2,
              restoring_backup/2,
              creating_backup/2,
              rolling_back_snapshot/2,
              creating_snapshot/2,
              deleting_snapshot/2,
              shutting_down/2]).

-export([create/4,
         load/1,
         type/1,
         start/1,
         update_fw/1,
         delete/1,
         store/1,
         transition/2,
         update/3,
         backup/3,
         restore_backup/3,
         snapshot/2,
         delete_snapshot/2,
         delete_backup/2,
         rollback_snapshot/2,
         service_action/3,
         force_state/2]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% This functions have to be exported but are only used internally.
-export([preloading/2,
         initialized/2,
         creating/2,
         loading/2,
         stopped/2,
         booting/2,
         running/2,
         shutting_down/2,
         restoring_backup/2,
         creating_backup/2,
         rolling_back_snapshot/2,
         creating_snapshot/2,
         deleting_snapshot/2
        ]).

-record(state, {hypervisor,
                type = unknown,
                zone_type = unknown,
                uuid :: binary(),
                orig_state,
                system = smartos,
                args,
                services = [],
                public_state,
                auth_ref,
                api_ref}).

-define(AUTH_DOOR, "_joyent_sshd_key_is_authorized").
-define(API_DOOR, "_fifo").

-type snapshot_fun() :: fun((binary(), binary(), binary(), [term()]) ->
                                   {ok, binary()} |
                                   {error, integer(), binary()}).

-type snapshot_complete_fun() :: fun((_, _, _, _) ->
                                            'error' |
                                            'ok' |
                                            {'error', 'no_servers'}).

-type snapshot_res() :: {error, binary()} | {ok, binary()}.


%%%===================================================================
%%% API
%%%===================================================================

-spec create(UUID::fifo:uuid(), PackageSpec::fifo:package(),
             DatasetSpec::fifo:dataset(), VMSpec::fifo:config()) ->
                    ok.

create(UUID, PackageSpec, DatasetSpec, VMSpec) ->
    chunter_vm_sup:start_child(UUID),
    gen_fsm:send_event({global, {vm, UUID}},
                       {create, PackageSpec, DatasetSpec, VMSpec}).

update(UUID, Package, Config) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}},
                                 {update, Package, Config}).

-spec load(UUID::fifo:uuid()) -> ok.

load(UUID) ->
    case global:whereis_name({vm, UUID}) of
        undefined ->
            chunter_vm_sup:start_child(UUID),
            gen_fsm:send_event({global, {vm, UUID}}, load);
        _ ->
            ok
    end,
    register(UUID).

update_fw(UUID) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, update_fw).

type(UUID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}}, type).


-spec transition(UUID::fifo:uuid(), State::fifo:vm_state()) -> ok.

transition(UUID, State) ->
    gen_fsm:send_event({global, {vm, UUID}}, {transition, State}).

-spec delete(UUID::fifo:uuid()) -> ok.

delete(UUID) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, delete).

-spec store(UUID::fifo:uuid()) -> ok.

store(UUID) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, store).

-spec force_state(UUID::fifo:uuid(), State::fifo:vm_state()) -> ok.

force_state(UUID, State) ->
    case global:whereis_name({vm, UUID}) of
        undefined ->
            chunter_vm_sup:start_child(UUID),
            gen_fsm:send_event({global, {vm, UUID}}, load),
            register(UUID);
        _ ->
            gen_fsm:send_all_state_event({global, {vm, UUID}},
                                         {force_state, State})
    end.

-spec register(UUID::fifo:uuid()) -> ok.

register(UUID) ->
    gen_fsm:send_all_state_event({global, {vm, UUID}}, register).

start(UUID) ->
    gen_fsm:send_event({global, {vm, UUID}}, start).

next() ->
    gen_fsm:send_event(self(), next).

restore_backup(UUID, SnapID, Options) ->
    case global:whereis_name({vm, UUID}) of
        undefined ->
            chunter_vm_sup:start_child(UUID),
            gen_fsm:send_event({global, {vm, UUID}},
                               {restore, SnapID, Options});
        _ ->
            gen_fsm:sync_send_all_state_event(
              {global, {vm, UUID}}, {backup, restore, SnapID, Options})
    end.

door_event(Pid, Ref, down) ->
    gen_fsm:send_all_state_event(Pid, {door, Ref, down});

door_event(Pid, Ref, Data) ->
    gen_fsm:sync_send_all_state_event(Pid, {door, Ref, Data}, 1000).

service_action(UUID, Action, Service)
  when Action =:= enable;
       Action =:= refresh;
       Action =:= restart;
       Action =:= disable;
       Action =:= clear ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}},
                                      {service, Action, Service}).

backup(UUID, SnapID, Options) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}},
                                      {backup, SnapID, Options}).

delete_backup(UUID, SnapID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}},
                                      {backup, delete, SnapID}).

snapshot(UUID, SnapID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}}, {snapshot, SnapID}).

delete_snapshot(UUID, SnapID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}},
                                      {snapshot, delete, SnapID}).

rollback_snapshot(UUID, SnapID) ->
    gen_fsm:sync_send_all_state_event({global, {vm, UUID}},
                                      {snapshot, rollback, SnapID}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(UUID) ->
    gen_fsm:start_link({global, {vm, UUID}}, ?MODULE, [UUID], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([UUID]) ->
    process_flag(trap_exit, true),
    System = chunter_utils:system(),
    {ok, preloading, #state{uuid = UUID, system = System}, 0}.


preloading(_, State = #state{uuid = UUID}) ->
    {Hypervisor, _, _} = chunter_server:host_info(),
    SnapshotIVal = application:get_env(chunter, snapshot_update_interval,
                                       900000),
    ServiceIVal = application:get_env(chunter, update_services_interval, 10000),
    %% This is every 15 minutes
    timer:send_interval(SnapshotIVal, update_snapshots),
    %% This is every 10 seconds
    timer:send_interval(ServiceIVal, update_services),
    snapshot_sizes(UUID),
    update_fw(UUID),
    register(UUID),
    {next_state, initialized, State#state{hypervisor = Hypervisor}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------


-spec initialized(Action::load |
                          {create,  PackageSpec::fifo:package(),
                           DatasetSpec::fifo:dataset(), VMSpec::fifo:config()},
                  State::term()) ->
                         {next_state, loading, State::term()} |
                         {next_state, creating, State::term()} |
                         {next_state, initialized, State::term()}.

initialized(load, State) ->
    {next_state, loading, State};

initialized({create, Package, {docker, DockerID}, VMSpec}, State) ->
    TID = {erlang:system_time(milli_seconds), node()},
    D = ft_dataset:new(TID),
    D1 = ft_dataset:type(TID, zone, D),
    D2 = ft_dataset:zone_type(TID, docker, D1),
    D3 = ft_dataset:kernel_version(TID, <<"3.13.0">>, D2),
    {ok, UUID} = chunter_imgadm:import(DockerID),
    D4 = ft_dataset:uuid(TID, UUID, D3),
    chunter_zlogin:start(UUID, docker),
    initialized({create, Package, D4, VMSpec}, State#state{zone_type = docker});

initialized({create, Package, Dataset, VMSpec},
            State=#state{hypervisor = Hypervisor, uuid=UUID}) ->
    PackageSpec = ft_package:to_json(Package),
    DatasetSpec = ft_dataset:to_json(Dataset),
    DatasetUUID = ft_dataset:uuid(Dataset),
    VMData = chunter_spec:to_vmadm(PackageSpec, DatasetSpec, VMSpec),
    lager:debug("Creating with spec: ~p", [VMData]),
    Ram = ft_package:ram(Package),
    chunter_server:reserve_mem(Ram),
    case ft_dataset:zone_type(Dataset) of
        ipkg ->
            create_ipkg(Dataset, Package, VMSpec, State);
        lipkg ->
            create_ipkg(Dataset, Package, VMSpec, State);
        _ ->
            SniffleData  = chunter_spec:to_sniffle(VMData),
            SniffleData1 = jsxd:set(<<"ram">>, Ram, SniffleData),
            change_state(UUID, <<"installing_dataset">>),
            howl_update(UUID, [{<<"hypervisor">>, Hypervisor},
                               {<<"config">>, SniffleData1},
                               {<<"package">>, ft_package:uuid(Package)}]),
            {Type, ZoneType} =
                case jsxd:get(<<"type">>, SniffleData1) of
                    {ok, <<"kvm">>} -> {kvm, kvm};
                    _ -> {zone, ft_dataset:zone_type(Dataset)}
                end,
            ls_vm:set_config(UUID, SniffleData1),
            lager:debug("[create:~s] Done generating config, handing to img "
                        "install.", [UUID]),
            case chunter_dataset_srv:install(DatasetUUID, UUID) of
                ok ->
                    lager:debug("[create:~s] Done installing image going to "
                                "create now.", [UUID]),
                    %% We run the zlogin first even so no zone will be
                    %% existing at that point, but we need that to be
                    %% running already when the vmadm is doing it's work
                    chunter_zlogin:start(UUID, ZoneType),
                    do_create(UUID, VMData, VMSpec),
                    {next_state, creating,
                     State#state{type = Type,
                                 zone_type = ZoneType,
                                 public_state = change_state(UUID,
                                                             <<"creating">>)}};
                E ->
                    lager:error("[create:~s] Dataset import failed with: ~p",
                                [UUID, E]),
                    ls_vm:log(UUID, <<"Failed to import Dataset, please check "
                                      "your image storage configuration. "
                                      "Please check the chunter logs for "
                                      "details.">>),
                    ls_vm:creating(UUID, false),
                    change_state(UUID, <<"failed">>),
                    {stop, normal, State}
            end
    end;

initialized({restore, SnapID, Options},
            State=#state{uuid=UUID}) ->
    {ok, VMObj} = ls_vm:get(UUID),
    Remote = ft_vm:backups(VMObj),
    case jsxd:get([SnapID, <<"xml">>], false, Remote) of
        true ->
            UUIDL = binary_to_list(UUID),
            Conf = chunter_snap:mk_s3_conf(Options),
            Bucket = proplists:get_value(s3_bucket, Options),
            {ok, XML} = fifo_s3:download(
                          Bucket, <<UUID/binary, "/", SnapID/binary, ".xml">>,
                          Conf),
            ok = file:write_file(<<"/etc/zones/", UUID/binary, ".xml">>, XML),
            os:cmd("echo '" ++ UUIDL ++ ":installed:/zones/" ++ UUIDL ++ ":" ++
                       UUIDL ++ "' >> /etc/zones/index");
        false ->
            ok
    end,
    Local = chunter_snap:get(UUID),
    case chunter_snap:restore_path(SnapID, Remote, Local) of
        {ok, Path} ->
            chunter_snap:describe_restore(Path),
            Toss = snaps_to_toss(Path, Remote, SnapID),
            backup_update(UUID, SnapID, <<"local">>, true),
            SniffleData = ft_vm:config(VMObj),
            {Type, ZoneType} =
                case {jsxd:get(<<"type">>, SniffleData),
                      jsxd:get(<<"zone_type">>, SniffleData)} of
                    {{ok, <<"kvm">>}, _} ->
                        {kvm, kvm};
                    {{ok, <<"zone">>}, {ok, <<"docker">>}} ->
                        {zone, docker};
                    {{ok, <<"zone">>}, {ok, <<"lx">>}} ->
                        {zone, lx};
                    _ ->
                        {zone, zone}
                end,
            chunter_zlogin:start(UUID, ZoneType),
            State1 = State#state{orig_state=loading,
                                 type = Type, zone_type = ZoneType,
                                 args={SnapID, Options, Path, Toss}},
            vm_event(UUID,  <<"vm-restored">>),
            restoring_backup(next, State1);
        E ->
            R = chunter_lock:release(UUID),
            lager:debug("[creating:~s] Log released after failure: ~p.",
                [UUID, R]),
            {next_state, E, initialized, State}
    end;

initialized(_, State) ->
    {next_state, initialized, State}.

-spec creating({transition, NextState::fifo:vm_state()}, State::term()) ->
                      {next_state, atom(), State::term()}.

creating({transition, NextState}, State = #state{uuid=UUID}) ->
    lager:debug("[creating:~s] Transitioning to state ~s.",
                [UUID, NextState]),
    R = chunter_lock:release(UUID),
    lager:debug("[creating:~s] Log released with: ~p.",
                [UUID, R]),
    {next_state, binary_to_atom(NextState),
     State#state{public_state = change_state(State#state.uuid, NextState)}}.

-spec loading({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

loading({transition, NextState}, State = #state{uuid = UUID}) ->
    {next_state, binary_to_atom(NextState),
     State#state{public_state = change_state(UUID, NextState, false)}}.

-spec stopped({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

stopped({transition, NextState = <<"booting">>}, State) ->
    {next_state, binary_to_atom(NextState),
     State#state{public_state = change_state(State#state.uuid, NextState)}};

stopped(start, State = #state{uuid = UUID, zone_type = docker}) ->
    update_timeout(UUID),
    chunter_vmadm:start(UUID),
    {next_state, stopped, State};

stopped(start, State) ->
    chunter_vmadm:start(State#state.uuid),
    {next_state, stopped, State};

stopped(_, State) ->
    {next_state, stopped, State}.

-spec booting({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

booting({transition, NextState = <<"shutting_down">>}, State) ->
    {next_state, binary_to_atom(NextState),
     State#state{public_state = change_state(State#state.uuid, NextState)}};

booting({transition, NextState = <<"running">>}, State) ->
    timer:send_after(500, get_info),
    {next_state, binary_to_atom(NextState),
     State#state{public_state = change_state(State#state.uuid, NextState)}};

booting(_, State) ->
    {next_state, booting, State}.

-spec running({transition, NextState::fifo:vm_state()}, State::term()) ->
                     {next_state, atom(), State::term()}.

running({transition, NextState = <<"shutting_down">>}, State) ->
    {next_state, binary_to_atom(NextState),
     State#state{public_state = change_state(State#state.uuid, NextState)}};

running(_, State) ->
    {next_state, running, State}.

-spec shutting_down({transition, NextState::fifo:vm_state()}, State::term()) ->
                           {next_state, atom(), State::term()}.

shutting_down({transition, NextState = <<"stopped">>}, State) ->
    {next_state, binary_to_atom(NextState),
     State#state{public_state = change_state(State#state.uuid, NextState)}};

shutting_down(_, State) ->
    {next_state, shutting_down, State}.

restoring_backup(next, State =
                     #state{orig_state = NextState,
                            args = {SnapID, Options, Path, Toss},
                            uuid = VM}) ->
    [snapshot_action(VM, Snap, fun do_restore/4, Options)
     || Snap <- Path],
    [snapshot_action(VM, Snap, fun do_delete_snapshot/4,
                     fun finish_delete_snapshot/4, Options)
     || Snap <- Toss],
    libhowl:send(VM,
                 [{<<"event">>, <<"backup">>},
                  {<<"data">>,
                   [{<<"action">>, <<"restored">>},
                    {<<"uuid">>, SnapID}]}]),
    timer:send_after(500, get_info),
    %% The restored dataset does not have the correct quota,
    %% we reapply the package to make sure it is applied.
    {ok, V} = ls_vm:get(VM),
    PID = ft_vm:package(V),
    {ok, Package} = ls_package:get(PID),
    update(VM, Package, []),
    {next_state, NextState, State#state{orig_state=undefined, args={}}}.

creating_backup(next, State = #state{orig_state = NextState, uuid=VM,
                                        args={SnapID, Options}}) ->
    lager:debug("Creating Backup with options: ~p", [Options]),
    case proplists:is_defined(create, Options) of
        true ->
            lager:debug("New Snapshot: ~p", [SnapID]),
            snapshot_action(
              VM, SnapID, fun do_snapshot/4,
              fun finish_snapshot/4, [backup]);
        _ ->
            ok
    end,
    spawn(fun () ->
                  create_backup_fn(VM, SnapID, Options)
          end),
    {next_state, NextState, State#state{orig_state=undefined, args={}}}.

create_backup_fn(VM, SnapID, Options) ->
    snapshot_action(VM, SnapID, fun do_backup/4,
                    fun finish_backup/4, Options),
    case {proplists:get_value(delete, Options),
          proplists:get_value(parent, Options)} of
        {true, _} ->
            lager:debug("Deleint snapshot: ~p", [SnapID]),
            snapshot_action(VM, SnapID, fun do_delete_snapshot/4,
                            fun finish_delete_snapshot/4, Options),
            backup_update(VM, SnapID, <<"local">>, false);
        {parent, undefined} ->
            lager:debug("Deleting parent but not defined."),
            backup_update(VM, SnapID, <<"local">>, true);
        {parent, Parent} ->
            lager:debug("Deleting parent: ~p", [Parent]),
            snapshot_action(VM, Parent, fun do_delete_snapshot/4,
                            fun finish_delete_snapshot/4, Options),
            backup_update(VM, SnapID, <<"local">>, true),
            backup_update(VM, Parent, <<"local">>, false);
        {undefined, _} ->
            backup_update(VM, SnapID, <<"local">>, true)
    end.

creating_snapshot(next, State = #state{orig_state = NextState,
                                          args={SnapID}}) ->
    snapshot_action(State#state.uuid, SnapID, fun do_snapshot/4,
                    fun finish_snapshot/4, []),
    {next_state, NextState, State#state{orig_state=undefined, args={}}}.

deleting_snapshot(next, State = #state{orig_state = NextState,
                                          args={SnapID}}) ->
    snapshot_action(State#state.uuid, SnapID, fun do_delete_snapshot/4,
                    fun finish_delete_snapshot/4, []),
    {next_state, NextState, State#state{orig_state=undefined, args={}}}.

rolling_back_snapshot(next, State = #state{orig_state = NextState,
                                              args={SnapID}}) ->
    snapshot_action(State#state.uuid, SnapID, fun do_rollback_snapshot/4,
                    fun finish_rollback_snapshot/4, []),
    {next_state, NextState, State#state{orig_state=undefined, args={}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

-spec handle_event({force_state, NextState::fifo:vm_state()},
                   StateName::atom(),
                   State::term()) ->
                          {next_state, NextStateName::fifo:vm_state_atom(),
                           NextState::term()} |
                          {stop, Reason::term(), NewState::term()}.

handle_event({force_state, NextState}, StateName,
             State = #state{uuid = UUID}) ->
    case binary_to_atom(NextState) of
        StateName
          when NextState =:= State#state.public_state ->
            {next_state, StateName, State};
        StateName ->
            {next_state, StateName,
             State#state{public_state = change_state(UUID, NextState, false)}};
        running = N ->
            {next_state, running,
             State#state{public_state = change_state(UUID, NextState,
                                                     StateName =:= N)}};
        Other ->
            {next_state, Other,
             State#state{public_state = change_state(UUID, NextState,
                                                     StateName =:= Other)}}
    end;

handle_event(register, StateName, State = #state{uuid = UUID}) ->
    ls_vm:register(UUID, State#state.hypervisor),
    %%    change_state(State#state.uuid, atom_to_binary(StateName)),
    case load_vm(UUID) of
        {error, not_found} ->
            lager:debug("[~s] Stopping in load, notfound.", [State#state.uuid]),
            {stop, {shutdown, not_found}, State};
        VMData ->
            snapshot_sizes(UUID),
            timer:send_after(500, get_info),
            SniffleData = chunter_spec:to_sniffle(VMData),
            {Type, ZoneType} =
                case {jsxd:get(<<"type">>, SniffleData),
                      jsxd:get(<<"zone_type">>, SniffleData)} of
                    {{ok, <<"kvm">>}, _} ->
                        {kvm, kvm};
                    {{ok, <<"zone">>}, {ok, <<"docker">>}} ->
                        {zone, docker};
                    {{ok, <<"zone">>}, {ok, <<"lx">>}} ->
                        {zone, lx};
                    _ ->
                        {zone, zone}
                end,
            lager:info("[~s] Has type: ~p.", [UUID, Type]),
            howl_update(UUID, [{<<"config">>, SniffleData}]),
            ls_vm:set_config(UUID, SniffleData),
            chunter_zlogin:start(UUID, ZoneType),
            State1 = State#state{type = Type, zone_type = ZoneType},
            change_state(State1#state.uuid, atom_to_binary(StateName), false),
            update_fw(UUID),
            {next_state, StateName, State1#state{services = []}}
    end;

handle_event({update, undefined, Config}, StateName,
             State = #state{uuid = UUID}) ->
    case load_vm(UUID) of
        {error, not_found} ->
            lager:debug("[~s] Stopping in update, notfound.",
                        [State#state.uuid]),
            {stop, {shutdown, not_found}, State};
        VMData ->
            Update = chunter_spec:create_update(VMData, undefined, Config),
            case chunter_vmadm:update(UUID, Update) of
                ok ->
                    SniffleData = finalize_update(UUID),
                    UpdateData = [{<<"config">>, SniffleData}],
                    howl_update(UUID, UpdateData),
                    {next_state, StateName, State};
                {error, E} ->
                    lager:error("[~s] updated failed with ~p", [UUID, E]),
                    ls_vm:log(UUID, <<"Update failed.">>),
                    {next_state, StateName, State}
            end
    end;

handle_event({update, Package, Config}, StateName,
             State = #state{uuid = UUID}) ->
    case load_vm(UUID) of
        {error, not_found} ->
            lager:debug("[~s] Stopping in update, notfound.",
                        [State#state.uuid]),
            {stop, {shutdown, not_found}, State};
        VMData ->
            P = ft_package:to_json(Package),
            Update = chunter_spec:create_update(VMData, P, Config),
            case chunter_vmadm:update(UUID, Update) of
                ok ->
                    SniffleData = finalize_update(UUID),
                    UpdateData = [{<<"package">>, ft_package:uuid(Package)},
                                  {<<"config">>, SniffleData}],
                    howl_update(UUID, UpdateData),
                    {next_state, StateName, State};
                {error, E} ->
                    lager:error("[~s] updated failed with ~p", [UUID, E]),
                    ls_vm:log(UUID, <<"Update failed.">>),
                    {next_state, StateName, State}
            end
    end;

handle_event(remove, _StateName, State) ->
    lager:debug("[~s] Calling remove.", [State#state.uuid]),
    ls_vm:unregister(State#state.uuid),
    %% TODO: Delete all FW rules for this vm?
    {stop, normal, State};

handle_event(delete, _StateName, State = #state{uuid = UUID}) ->
    lager:debug("[~s] Calling delete.", [State#state.uuid]),
    case load_vm(UUID) of
        {error, not_found} ->
            ok;
        _VM ->
            chunter_vmadm:delete(UUID),
            lager:info("Deleting ~s successfull, letting sniffle know.",
                       [UUID]),
            ls_vm:delete(UUID)
    end,
    wait_for_delete(UUID),
    chunter_zlogin:stop(UUID),
    {stop, normal, State};

handle_event(store, _StateName, State = #state{uuid = UUID}) ->
    lager:debug("[~s] Calling delete to store VM.", [State#state.uuid]),
    case load_vm(UUID) of
        {error, not_found} ->
            ok;
        _VM ->
            chunter_vmadm:delete(UUID),
            lager:info("Deleting ~s successfull, letting sniffle know.",
                       [UUID]),
            vm_event(UUID,  <<"vm-stored">>),
            ensure_state(UUID, <<"stored">>)
    end,
    wait_for_delete(UUID),
    chunter_zlogin:stop(UUID),
    {stop, normal, State};

handle_event(update_fw, StateName,
             State = #state{uuid = UUID, system = smartos}) ->
    %% TODO: get the fw rules and do something with them
    case {ls_vm:get(UUID), fwadm:list_fifo(UUID)} of
        {{ok, VM}, {ok, OldRules}} ->
            Owner = ft_vm:owner(VM),
            NewRules = ft_vm:fw_rules(VM),
            NewRules1 = [fwadm:convert(UUID, R) || R <- NewRules],
            NewRules2 = lists:flatten(NewRules1),
            OldRules1 = [map_rule(R) || R <- OldRules],
            {Add, Delete} = split_rules(OldRules1, NewRules2),
            lager:info("[vm:~s(~s)] Updating FW rules, adding ~p deleting ~p.",
                       [UUID, Owner, Add, Delete]),
            [fwadm:add(Owner, UUID, R) || R <- Add],
            [fwadm:delete(R) || R <- Delete],
            case fwadm:list(UUID) of
                {ok, []} ->
                    fwadm:stop(UUID);
                {ok, _} ->
                    fwadm:start(UUID);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {next_state, StateName, State};

%% TODO: Only SmartOS supports FW rules
handle_event(update_fw, StateName, State) ->
    {next_state, StateName, State};

handle_event({door, _Ref, down}, StateName,
             State = #state{auth_ref=_Ref, uuid=UUID}) ->
    lager:warning("[vm:~s] door down!", [UUID]),
    timer:send_after(1000, {init, zonedoor}),
    {next_state, StateName, State#state{auth_ref = undefined}};

handle_event({door, _Ref, down}, StateName,
             State = #state{api_ref=_Ref, uuid=UUID}) ->
    lager:warning("[vm:~s] api door down!", [UUID]),
    timer:send_after(1000, {init, zonedoor}),
    {next_state, StateName, State#state{api_ref = undefined}};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------


handle_sync_event(type, _From, StateName,
                  State = #state{zone_type = Type}) ->
    {reply, {ok, Type}, StateName, State};

handle_sync_event({door, Ref, Data}, _From, StateName,
                  State = #state{auth_ref=Ref, uuid=UUID}) ->
    R = case auth_credintials(UUID, Data) of
            true ->
                <<"1">>;
            false ->
                <<"0">>
        end,
    {reply, {ok, R}, StateName, State};

handle_sync_event({door, Ref, Data}, _From, StateName,
                  State = #state{api_ref=Ref, uuid=UUID}) ->
    lager:info("[zone:~s] API: ~s", [UUID, Data]),
    try jsone:decode(Data) of
        JSON ->
            JSON1 = jsxd:from_list(JSON),
            Reply = case chunter_api:call(UUID, JSON1) of
                        {ok, D} ->
                            Bin = jsone:encode(D),
                            {ok, <<$1, Bin/binary>>};
                        {error, E} ->
                            lager:warning("[zdoor] error: ~p", [E]),
                            RJSON = jsone:encode([{error, list_to_binary(E)}]),
                            {ok, <<$0, RJSON/binary>>}
                    end,
            {reply, Reply, StateName, State}
    catch
        _:_ ->
            lager:warning("[zdoor] error: ~p", [Data]),
            RJSON = jsone:encode([{error,  <<"format error: ", Data/binary>>}]),
            Reply = {ok, <<$0, RJSON/binary>>},
            {reply, Reply, StateName, State}
    end;

handle_sync_event({backup, restore, SnapID, Options}, _F, StateName, State) ->
    VM = State#state.uuid,
    {ok, VMObj} = ls_vm:get(VM),
    Remote = ft_vm:backups(VMObj),
    Local = chunter_snap:get(VM),
    case chunter_snap:restore_path(SnapID, Remote, Local) of
        {ok, Path} ->
            chunter_snap:describe_restore(Path),
            Toss = snaps_to_toss(Path, Remote, SnapID),
            backup_update(VM, SnapID, <<"local">>, true),
            State1 = State#state{orig_state=StateName,
                                 args={SnapID, Options, Path, Toss}},
            next(),
            {reply, ok, restoring_backup, State1};
        E ->
            {reply, E, StateName, State}
    end;

handle_sync_event({backup, delete, SnapID}, _From, StateName, State) ->
    State1 = State#state{orig_state=StateName, args={SnapID}},
    backup_update(State#state.uuid, SnapID, <<"local">>, false),
    backup_update(State#state.uuid, SnapID, <<"local_size">>, 0),
    next(),
    {reply, ok, deleting_snapshot, State1};

handle_sync_event({service, enable, Service}, _From, StateName, State) ->
    {reply, smurf:enable(Service, [{zone, State#state.uuid}]),
     StateName, State};

handle_sync_event({service, refresh, Service}, _From, StateName, State) ->
    {reply, smurf:refresh(Service, [{zone, State#state.uuid}]),
     StateName, State};

handle_sync_event({service, restart, Service}, _From, StateName, State) ->
    {reply, smurf:restart(Service, [{zone, State#state.uuid}]),
     StateName, State};

handle_sync_event({service, disable, Service}, _From, StateName, State) ->
    {reply, smurf:disable(Service, [{zone, State#state.uuid}]),
     StateName, State};

handle_sync_event({service, clear, Service}, _From, StateName, State) ->
    {reply, smurf:clear(Service, [{zone, State#state.uuid}]),
     StateName, State};

handle_sync_event({backup, SnapID, Options}, _From, StateName, State) ->
    State1 = State#state{orig_state=StateName, args={SnapID, Options}},
    next(),
    {reply, ok, creating_backup, State1};

handle_sync_event({snapshot, SnapID}, _From, StateName, State) ->
    State1 = State#state{orig_state=StateName,
                         args={SnapID}},
    next(),
    {reply, ok, creating_snapshot, State1};

handle_sync_event({snapshot, delete, SnapID}, _From, StateName, State) ->
    State1 = State#state{orig_state=StateName,
                         args={SnapID}},
    next(),
    {reply, ok, deleting_snapshot, State1};


handle_sync_event({snapshot, rollback, SnapID}, _From, StateName, State) ->
    State1 = State#state{orig_state=StateName,
                         args={SnapID}},
    next(),
    {reply, ok, rolling_back_snapshot, State1};

handle_sync_event(delete, _From, StateName, State) ->
    case load_vm(State#state.uuid) of
        {error, not_found} ->
            lager:debug("[~s] Delete sync event.", [State#state.uuid]),
            {stop, {shutdown, not_found}, State};
        _VM ->
            spawn(chunter_vmadm, delete, [State#state.uuid]),
            libhowl:send(State#state.uuid, [{<<"event">>, <<"delete">>}]),
            {reply, ok, StateName, State}
    end;

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------


handle_info(update_snapshots, StateName, State = #state{uuid = UUID}) ->
    snapshot_sizes(UUID),
    {next_state, StateName, State};

handle_info({'EXIT', _D, _PosixCode}, StateName, State) ->
    {next_state, StateName, State};

handle_info(update_services, running,
            State=#state{
                     uuid=UUID,
                     services = OldServices,
                     type = zone,
                     zone_type = Type
                    }) when Type =/= lx,
                            Type =/= docker ->
    case {chunter_smf:update(UUID, OldServices), OldServices} of
        {{ok, ServiceSet, Changed}, []} ->
            lager:debug("[~s] Initializing ~p Services.",
                        [UUID, length(Changed)]),
            ls_vm:set_service(UUID,
                              [{Srv, St}
                               || {Srv, _, St} <- Changed]),
            {next_state, running, State#state{services = ServiceSet}};
        {{ok, ServiceSet, Changed}, _} ->
            case length(Changed) of
                0 -> ok;
                L ->
                    lager:debug("[~s] Updating ~p Services.", [UUID, L])
            end,
            %% Update changes which are not removes
            ls_vm:set_service(UUID,
                              [{Srv, SrvState}
                               || {Srv, _, SrvState} <- Changed,
                                  SrvState =/= <<"removed">>]),
            %% Delete services that were changed.
            ls_vm:set_service(UUID,
                              [{Srv, delete}
                               || {Srv, _, <<"removed">>} <- Changed]),
            update_services(UUID, Changed),
            {next_state, running, State#state{services = ServiceSet}};
        _ ->
            {next_state, running, State}
    end;

handle_info(update_services, StateName, State) ->
    {next_state, StateName, State};

handle_info(get_info, stopped, State) ->
    timer:send_after(1000, get_info),
    {next_state, stopped, State};

handle_info(get_info, running, State = #state{type=zone}) ->
    State1 = ensure_zonedoor(State),
    timer:send_after(1000, get_info),
    {next_state, running, State1};

handle_info(get_info, StateName, State=#state{type=zone}) ->
    timer:send_after(1000, get_info),
    {next_state, StateName, State};

handle_info(get_info, StateName, State=#state{type=kvm}) ->
    case chunter_vmadm:info(State#state.uuid) of
        {error, no_info} ->
            timer:send_after(1000, get_info),
            {next_state, StateName, State};
        {ok, Info} ->
            ls_vm:set_info(State#state.uuid, Info),
            {next_state, StateName, State}
    end;

handle_info({init, _}, stopped, State) ->
    {next_state, stopped, State};

%% TODO: Would be really nice to have zone doors for lx
handle_info({init, zonedoor}, StateName, State=#state{type=zone, zone_type=_T})
  when _T =/= lx; _T =/= docker ->
    {next_state, StateName, ensure_zonedoor(State)};

handle_info({init, _}, StateName, State) ->
    {next_state, StateName, State};

handle_info(Info, StateName, State) ->
    lager:warning("unknown data: ~p", [Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(normal, _StateName, #state{auth_ref=AuthRef, api_ref=APIRef}) ->
    ezdoor_server:remove(AuthRef),
    ezdoor_server:remove(APIRef),
    ok;
terminate(Reason, StateName,
          State = #state{uuid = UUID, auth_ref=AuthRef, api_ref=APIRef}) ->
    ezdoor_server:remove(AuthRef),
    ezdoor_server:remove(APIRef),
    lager:warning("[terminate:~s] Terminating from ~p with reason ~p.",
                  [UUID, StateName, Reason]),
    lager:warning("[terminate:~s] The state: ~p .", [UUID, State]),
    terminate(normal, StateName, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ensure_zonedoor(State) ->
    State1 = case ezdoor_server:add(?MODULE, State#state.uuid, ?AUTH_DOOR) of
                 {ok, AuthRef} ->
                     State#state{auth_ref = AuthRef};
                 {error, doublicate} ->
                     State
             end,
    case ezdoor_server:add(?MODULE, State1#state.uuid, ?API_DOOR) of
        {ok, APIRef} ->
            State1#state{api_ref = APIRef};
        {error, doublicate} ->
            State1
    end.



do_snapshot(<<_:1/binary, P/binary>>, _VM, SnapID, _) ->
    chunter_zfs:snapshot(P, SnapID).

finish_snapshot(_VM, _SnapID, [backup], ok) ->
    ok;
finish_snapshot(VM, SnapID, _, ok) ->
    snap_state(VM, SnapID, <<"completed">>),
    {ok, VmData} = ls_vm:get(VM),
    Snaps = ft_vm:snapshots(VmData),
    TheSnap = proplists:get_value(SnapID, Snaps),
    libhowl:send(VM,
                 [{<<"event">>, <<"snapshot">>},
                  {<<"data">>,
                   [{<<"uuid">>, SnapID},
                    {<<"action">>, <<"completed">>}] ++
                   [{<<"data">>, TheSnap}]}]),
    ok;

finish_snapshot(_VM, _SnapID, _, error) ->
    error.

snap_state(VM, SnapID, State) ->
    ls_vm:set_snapshot(VM, [{[SnapID, <<"state">>], State}]).


do_delete_snapshot(<<_:1/binary, P/binary>>, _VM, SnapID, _) ->
    chunter_zfs:destroy_snapshot(P, SnapID, [f, r]).

finish_delete_snapshot(VM, SnapID, _, ok) ->
    lager:debug("Deleting ~p", [SnapID]),
    ls_vm:set_snapshot(VM, [{[SnapID], delete}]),
    libhowl:send(VM,
                 [{<<"event">>, <<"snapshot">>},
                  {<<"data">>,
                   [{<<"action">>, <<"deleted">>},
                    {<<"uuid">>, SnapID}]}]),
    ok;

finish_delete_snapshot(_VM, _SnapID, _, error) ->
    error.

do_rollback_snapshot(<<_:1/binary, P/binary>>, _VM, SnapID, _) ->
    chunter_zfs:rollback(P, SnapID, [r]).

finish_rollback_snapshot(VM, SnapID, _, ok) ->
    libhowl:send(VM,
                 [{<<"event">>, <<"snapshot">>},
                  {<<"data">>,
                   [{<<"action">>, <<"rollback">>},
                    {<<"uuid">>, SnapID}]}]),
    lager:debug("[Snapshot] committing rollback fo snapshot ~s on VM ~s.",
                [VM, SnapID]),
    ls_vm:commit_snapshot_rollback(VM, SnapID);

finish_rollback_snapshot(_VM, _SnapID, _, error) ->
    lager:error("Snapshot rollback failed!"),
    error.

do_backup(Path, VM, SnapID, Options) ->
    chunter_snap:upload(Path, VM, SnapID, Options).

finish_backup(VM, UUID, Opts, ok) ->
    case proplists:is_defined(xml, Opts) of
        true ->
            Conf = chunter_snap:mk_s3_conf(Opts),
            Bucket = proplists:get_value(s3_bucket, Opts),
            {ok, XML} = file:read_file(
                          binary_to_list(<<"/etc/zones/", VM/binary, ".xml">>)),
            fifo_s3:upload(Bucket, <<VM/binary, "/", UUID/binary, ".xml">>,
                           XML, Conf),
            backup_update(VM, UUID, <<"xml">>, true),
            ok;
        false ->
            ok
    end,
    backup_update(VM, UUID, <<"state">>, <<"completed">>);

finish_backup(_VM, _UUID, _, error) ->
    error.

-spec snapshot_action_on_disks(VM :: binary(),
                               UUID :: binary(),
                               Fun :: snapshot_fun(),
                               Res :: snapshot_res(),
                               Disks ::[[{binary(), term()}]],
                               Opts :: [term()]) -> snapshot_res().
snapshot_action_on_disks(VM, UUID, Fun, LastReply, Disks, Opts) ->
    AccIn = {{VM, UUID, Fun, Opts}, LastReply},
    case lists:foldl(fun snapshot_fold/2, AccIn, Disks) of
        {error, _} = E -> E;
        {_, Reply} ->
            Reply
    end.

-type disk_fold_acc() :: {error, binary()} |
                      {{VM :: binary(),
                        UUID :: binary(),
                        Fun :: snapshot_fun(),
                        Opts :: [term()]},
                       {ok, binary()}}.

-spec snapshot_fold(Disk :: [{binary(), term()}],
                    Return :: disk_fold_acc()) ->
                           disk_fold_acc().

snapshot_fold(_, {error, _E} = R) ->
    R;
snapshot_fold(Disk, {Env = {VM, UUID, Fun, Opts}, {ok, Reply0}}) ->
    case jsxd:get(<<"path">>, Disk) of
        {ok, <<_:14/binary, P1/binary>>} ->
            case Fun(P1, VM, UUID, Opts) of
                {ok, Res} ->
                    {Env, {ok, <<Reply0/binary, "\n", Res/binary>>}};
                {ok, Res, _} ->
                    {Env, {ok, <<Reply0/binary, "\n", Res/binary>>}};
                {error, Code, Res} ->
                    lager:error("Failed snapshot disk ~s from VM ~s ~p:~s.",
                                [P1, VM, Code, Res]),
                    ls_vm:log(VM, <<"Failed snapshot disk ",
                                    P1/binary, ": ", Res/binary>>),
                    {error, <<Reply0/binary, "\n", Res/binary>>}
            end;
        _ ->
            {error, <<"missing">>}
    end.

snapshot_action(VM, UUID, Fun, Opts) ->
    snapshot_action(VM, UUID, Fun, fun(_, _, _, _) -> ok end, Opts).

snapshot_action(VM, UUID, Fun, CompleteFun, Opts) ->
    case load_vm(VM) of
        {error, not_found} ->
            ok;
        VMData ->
            Spec = chunter_spec:to_sniffle(VMData),
            snapshot_action1(VM, Spec, UUID, Fun, CompleteFun, Opts)
    end.

snapshot_action1(VM, Spec, UUID, Fun, CompleteFun, Opts) ->
    case jsxd:get(<<"zonepath">>, Spec) of
        {ok, P} ->
            case Fun(P, VM, UUID, Opts) of
                %% TODO can this even happen?
                {ok, Rx, _} ->
                    snapshot_action2(VM, Spec, UUID, {ok, Rx}, Fun,
                                     CompleteFun, Opts),
                    {ok, Rx};
                {ok, Rx} ->
                    snapshot_action2(VM, Spec, UUID, {ok, Rx}, Fun,
                                     CompleteFun, Opts),
                    {ok, Rx};
                {error, Code, Reply} ->
                    lager:error("Failed snapshot VM ~s ~p: ~s.",
                                [VM, Code, Reply]),
                    ls_vm:log(VM, <<"Failed to snapshot: ",
                                    Reply/binary>>),
                    CompleteFun(VM, UUID, Opts, error)
            end
    end.

-spec snapshot_action2(binary(), term(), binary(), {'ok', binary()},
                       snapshot_fun(), snapshot_complete_fun(), [term()]) ->
                              'error' | 'ok' | {'error', 'no_servers'}.

snapshot_action2(VM, Spec, UUID, R0, Fun, CompleteFun, Opts) ->
    Disks = jsxd:get(<<"disks">>, [], Spec),
    case snapshot_action_on_disks(VM, UUID, Fun, R0, Disks, Opts) of
        {ok, Res} ->
            M = io_lib:format("Snapshot done: ~p",
                              [Res]),
            ls_vm:log(VM, iolist_to_binary(M)),
            CompleteFun(VM, UUID, Opts, ok);
        {error, E} ->
            libhowl:send(VM,
                         [{<<"event">>, <<"snapshot">>},
                          {<<"data">>,
                           [{<<"action">>, <<"error">>},
                            {<<"message">>, E},
                            {<<"uuid">>, UUID}]}]),
            lager:error("Snapshot failed with: ~p", E),
            CompleteFun(VM, UUID, Opts, error)
    end.

snapshot_sizes(VM) ->
    lager:debug("[~s] Updating Snapshots.", [VM]),
    case {libsniffle:servers(), ls_vm:get(VM)} of
        {[], _} ->
            lager:warning("[~s] No Servers to update snapshots.", [VM]),
            ok;
        {_, {ok, V}} ->
            Snaps = case load_vm(VM) of
                        {error, not_found} ->
                            [];
                        VMData ->
                            Spec = chunter_spec:to_sniffle(VMData),
                            chunter_snap:get_all(VM, Spec)
                    end,
            %% First we look at backups
            Bs = ft_vm:backups(V),
            KnownB = [ ID || {ID, _} <- Bs],
            Backups1 = lists:filter(fun ({Name, _}) ->
                                            lists:member(Name, KnownB)
                                    end, Snaps),
            Local = [N || {N, _ } <- Backups1],
            NonLocal = lists:subtract(KnownB, Local),
            Bs1 = [{[Name, <<"local_size">>], Size}
                   || {Name, Size} <- Backups1] ++
                [{[Name, <<"local_size">>], 0}
                 || Name <- NonLocal],
            ls_vm:set_backup(VM, Bs1),

            %% And then at sanpshots
            Ss = ft_vm:snapshots(V),
            KnownS = [ ID || {ID, _} <- Ss],
            Snaps1 =lists:filter(fun ({Name, _}) ->
                                         lists:member(Name, KnownS)
                                 end, Snaps),
            FlatSnaps = [Name || {Name, _Size} <- Snaps],
            SnapsGone = lists:filter(fun (Name) ->
                                             not lists:member(Name, FlatSnaps)
                                     end, KnownS),
            Ss1 = [{[Name, <<"size">>], Size}
                   || {Name, Size} <- Snaps1],
            Ss2 = [{[Name], delete} || Name <- SnapsGone],
            ls_vm:set_snapshot(VM, Ss1 ++ Ss2);
        _ ->
            lager:warning("[~s] Could not read VM data.", [VM]),
            ok
    end.

%% _:43 is '/zones/' + uuid
do_restore(Path, VM, {local, SnapId}, Opts) ->
    do_rollback_snapshot(Path, VM, SnapId, Opts);
do_restore(Path = <<_:43/binary, Dx/binary>>, VM, {Type, SnapId, SHAs}, Opts)
when Type =:= full;
     Type =:= incr ->
    File = <<VM/binary, "/", SnapId/binary, Dx/binary>>,
    SHA1 = jsxd:get([File], <<>>, SHAs),
    case Type of
        full -> do_destroy(Path, VM, SnapId, Opts);
        _    -> ok
    end,
    snap_state(VM, SnapId, <<"restoring">>),
    chunter_snap:download(Path, VM, SnapId, SHA1, Opts),
    wait_import(Path),
    snap_state(VM, SnapId, <<"completed">>),
    do_rollback_snapshot(Path, VM, SnapId, Opts).

do_destroy(<<_:1/binary, P/binary>>, _VM, _SnapID, _) ->
    chunter_zfs:destroy(P, [f, r]).

%%%===================================================================
%%% Utility
%%%===================================================================

-spec load_vm(ZUUID::fifo:uuid()) -> fifo:vm_config() | {error, not_found}.

load_vm(UUID) ->
    chunter_zone:get(UUID).

-spec change_state(UUID::binary(), State::fifo:vm_state()) -> fifo:vm_state().

change_state(UUID, State) ->
    change_state(UUID, State, true).

-spec change_state(UUID::binary(), State::fifo:vm_state(), boolean()) ->
                          fifo:vm_state().

change_state(UUID, State, true) ->
    change_state(UUID, State, false),
    ls_vm:log(UUID, <<"Transitioning ", State/binary>>),
    State;

change_state(UUID, State, false) ->
    lager:debug("[~s] Changing state: ~s", [UUID, State]),
    ensure_state(UUID, State),
    libhowl:send(UUID, [{<<"event">>, <<"state">>}, {<<"data">>, State}]),
    State.
ensure_state(UUID, State) ->
    ensure_state(UUID, State, 10).

ensure_state(UUID, State, 0) ->
    lager:error("[~s] Could not set state to ~s", [UUID, State]);
ensure_state(UUID, State, N) ->
    ok = ls_vm:state(UUID, State),
    {ok, VM} =ls_vm:get(UUID),
    case ft_vm:state(VM) of
        State ->
            ok;
        _ ->
            ensure_state(UUID, State, N - 1)
    end.

-spec binary_to_atom(B::binary()) -> A::atom().
binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

-spec atom_to_binary(I::binary()|atom()) -> A::binary().
atom_to_binary(B) when is_binary(B) ->
    B;
atom_to_binary(A) ->
    list_to_binary(atom_to_list(A)).

backup_update(VM, SnapID, K, V) ->
    ls_vm:set_backup(VM, [{[SnapID, K], V}]),
    libhowl:send(VM,
                 [{<<"event">>, <<"backup">>},
                  {<<"data">>,
                   [{<<"action">>, <<"update">>},
                    {<<"data">>, [{K, V}]},
                    {<<"uuid">>, SnapID}]}]).
update_services(_, []) ->
    ok;

update_services(UUID, Changed) ->
    case [{Srv, New} || {Srv, Old, New} <- Changed, Old =/= New] of
        [] ->
            ok;
        Changed1 ->
            libhowl:send(UUID, [{<<"event">>, <<"services">>},
                                {<<"data">>, Changed1}])
    end.

wait_import(<<_:1/binary, P/binary>>) ->
    Cmd = "zfs list -Hp -t all -r " ++ binary_to_list(P),
    wait_image(0, Cmd).

wait_image(N, Cmd) when N < 3 ->
    timer:sleep(5000),
    wait_image(length(re:split(os:cmd(Cmd), "\n")), Cmd);

wait_image(_, _) ->
    lager:debug("<IMG> done waiting.", []),
    ok.

timestamp() ->
    erlang:system_time(micro_seconds).

auth_credintials(UUID, Data) ->
    case re:split(Data, " ") of
        [User, _UserID, KeyID] ->
            chk_key(UUID, User, KeyID);
        _ ->
            lager:warning("[zonedoor] recived unknown data: ~p.", [Data]),
            false
    end.

chk_key(UUID, User, KeyID) ->
    KeyBin = libsnarl:keystr_to_id(KeyID),
    case ls_user:key_find(KeyBin) of
        {ok, UID} ->
            Res = libsnarl:allowed(UID, [<<"vms">>, UUID, <<"console">>])
                orelse libsnarl:allowed(UID, [<<"vms">>, UUID, <<"ssh">>])
                orelse libsnarl:allowed(UID, [<<"vms">>, UUID,
                                              <<"ssh">>, User]),
            lager:warning("[zonedoor:~s] User ~s trying to connect with key "
                          "~s -> ~s", [UUID, User, KeyID, Res]),
            Res;
        _ ->
            lager:warning("[zonedoor:~s] denied.", [UUID]),
            false
    end.

wait_for_delete(UUID) when is_binary(UUID) ->
    wait_for_delete(binary_to_list(UUID));

wait_for_delete(UUID) ->
    Cmd = "/usr/sbin/zoneadm",
    Port = open_port({spawn_executable, Cmd},
                     [{args, ["-z", UUID, "list"]}, use_stdio, binary,
                      stderr_to_stdout, exit_status]),
    receive
        {Port, {exit_status, 0}} ->
            wait_for_delete(UUID);
        {Port, {exit_status, 1}} ->
            ok
    end.

-spec split_rules([{fifo:uuid(), fifo:smartos_fw_rule()}],
                  [fifo:smartos_fw_rule()]) ->
                         {[fifo:smartos_fw_rule()], [fifo:uuid()]}.
split_rules(OldRules, NewRules) ->
    split_rules(OldRules, NewRules, [], []).

-spec split_rules(
        [{fifo:uuid(), fifo:smartos_fw_rule()}],
        [fifo:smartos_fw_rule()],
        [fifo:smartos_fw_rule()],
        [fifo:uuid()]) ->
                         {[fifo:smartos_fw_rule()], [fifo:uuid()]}.

split_rules([], [], Add, Delete) ->
    {Add, Delete};
split_rules(OldRules, [], Add, Delete) ->
    {Add, [UUID || {UUID, _} <- OldRules] ++ Delete};
split_rules([], New, Add, Delete) ->
    {New ++ Add, Delete};
split_rules([{UUID, Rule} | OldRules], NewRules, Add, Delete) ->
    case lists:member(Rule, NewRules) of
        true ->
            split_rules(OldRules, lists:delete(Rule, NewRules), Add, Delete);
        false ->
            split_rules(OldRules, NewRules, Add, [UUID | Delete])
    end.

-spec map_rule([{binary(), binary() | fifo:smartos_fw_rule()}]) ->
                      {binary(), fifo:smartos_fw_rule()}.
map_rule(JSX) ->
    %% We need proplists or dialyzer will insist it's a not a rule.
    UUID = proplists:get_value(<<"uuid">>, JSX),
    Rule = proplists:get_value(<<"rule">>, JSX),
    {UUID, Rule}.


create_ipkg(Dataset, Package, VMSpec, State = #state{uuid = UUID}) ->
    lager:info("The very first create request to a omnios hypervisor: ~s.",
               [UUID]),
    {NICS, Conf} = chunter_spec:to_zonecfg(Package, Dataset, VMSpec),
    UUIDs = binary_to_list(UUID),
    File = ["/tmp/", UUIDs, ".conf"],
    file:write_file(File, chunter_zone:zonecfg(Conf)),
    R1 = os:cmd(["/usr/sbin/zonecfg -z ", UUIDs, " -f ", File]),
    lager:info("[zonecfg:~s] ~p", [UUID, R1]),
    R2 = os:cmd(["/usr/sbin/zoneadm -z ", UUIDs, " install"]),
    lager:info("[zonecfg:~s] ~p", [UUID, R2]),
    R3 = os:cmd(["/usr/sbin/zoneadm -z ", UUIDs, " boot"]),
    lager:info("[zonecfg:~s] ~p", [UUID, R3]),
    lager:info("[setup:~s] Starting zone setup.", [UUID]),
    lager:info("[setup:~s] Waiting for zone to boot for the first time.",
               [UUID]),
    S = <<"svc:/milestone/multi-user-server:default">>,
    ok = wait_for_started(UUID, S),
    %% Do the basic setup we can do on the first boot.
    lager:info("[setup:~s] nsswitch conf.", [UUID]),
    zlogin(UUID, "cp -f /etc/nsswitch.dns /etc/nsswitch.conf"),
    lager:info("[setup:~s] dns.", [UUID]),
    zlogin(UUID, "echo > /etc/resolv.conf"),
    case jsxd:get(<<"resolvers">>, VMSpec) of
        {ok, ResolversL} ->
            ResolversBin = re:split(ResolversL, ","),
            Resolvers = [binary_to_list(Rslvr) || Rslvr <- ResolversBin],
            [zlogin(UUID, ["echo 'nameserver ", Rslvr, "' >> /etc/resolv.conf"])
             || Rslvr <- Resolvers];
        _ ->
            ok
    end,
    DomainBin = jsxd:get(<<"dns_domain">>, <<"local">>, VMSpec),
    Domain = binary_to_list(DomainBin),
    zlogin(UUID, ["echo \"domain ", Domain, "\" >> /etc/resolv.conf"]),
    zlogin(UUID, ["echo \"search ", Domain, "\" >> /etc/resolv.conf"]),

    case jsxd:get(<<"hostname">>, VMSpec) of
        {ok, Hostname} ->
            lager:info("[setup:~s] hostname.", [UUID]),
            zlogin(UUID, ["hostname ", binary_to_list(Hostname)]);
        _ ->
            ok
    end,
    %% TODO: this is entirely stupid but for some reason we can't do the
    %% network setup on the first zone boot so for some reason, we'll need
    %% to reboot the zone before we can go on - bug reported, lets hope someone
    %% smarter then me knows why this would hapen.
    lager:info("[setup:~s] Now reboot it.", [UUID]),
    chunter_vmadm:reboot(UUID),
    lager:info("[setup:~s] Giveit a few seconds.", [UUID]),
    timer:sleep(5000),
    lager:info("[setup:~s] And wait for the system to come up agian.", [UUID]),
    ok = wait_for_started(UUID, S),
    lager:info("[setup:~s] Initializing networks.", [UUID]),
    lists:map(fun({NicBin, Spec}) ->
                      Nic = binary_to_list(NicBin),
                      {ok, IPBin} = jsxd:get(<<"ip">>, Spec),
                      IP = binary_to_list(IPBin),
                      {ok, Netmask} = jsxd:get(<<"netmask">>, Spec),
                      CIDR = ft_iprange:mask_to_cidr(Netmask),
                      CIDRS = integer_to_list(CIDR),
                      zlogin(UUID, ["ipadm create-if ", Nic]),
                      zlogin(UUID, ["ipadm create-addr -T static",
                                    " -a ", IP, "/", CIDRS,
                                    " ", Nic, "/v4"]),
                      case jsxd:get(<<"primary">>, Spec) of
                          {ok, true} ->
                              {ok, GWBin} = jsxd:get(<<"gateway">>, Spec),
                              GW = binary_to_list(GWBin),
                              zlogin(UUID, ["route -p add default ", GW]);
                          _ ->
                              ok
                      end
              end, NICS),
    lager:info("[setup:~s] Zone setup completed.", [UUID]),
    {next_state, creating,
     State#state{type = zone, zone_type = ipgk,
                 public_state = change_state(UUID, <<"running">>)}}.

wait_for_started(UUID, S) ->
    timer:sleep(1000),
    case smurf:status(UUID, S) of
        {ok, {S, <<"online">>, _}} ->
            ok;
        _ ->
            wait_for_started(UUID, S)
    end.

zlogin(UUID, Cmd) ->
    UUIDs = binary_to_list(UUID),
    FullCmd = ["/usr/sbin/zlogin ", UUIDs, " '", Cmd, "'"],
    lager:info("[zlogin:~s] ~s", [UUID, FullCmd]),
    Rx = os:cmd(FullCmd),
    lager:info("[zlogin:~s]-> ~s", [UUID, Rx]),
    Rx.


snaps_to_toss(Path, Remote, SnapID) ->
    Toss0 =
        [S || {_, S, _} <- Path,
              jsxd:get([S, <<"local">>], false, Remote)
                  =:= false],
    [T || T <- Toss0, T =/= SnapID].

finalize_update(UUID) ->
    VMData = load_vm(UUID),
    chunter_server:update_mem(),
    SniffleData = chunter_spec:to_sniffle(VMData),
    ls_vm:set_config(UUID, SniffleData),
    ls_vm:log(UUID, <<"Update complete.">>),
    SniffleData.


howl_update(UUID, Data) ->
    libhowl:send(UUID, [{<<"event">>, <<"update">>},
                        {<<"data">>, Data}]).

do_create(UUID, CreateJSON, VMSpec) ->
    change_state(UUID, <<"creating">>),
    case chunter_vmadm:create(UUID, CreateJSON) of
        ok ->
            lager:debug(
              "[create:~s] Done creating continuing on.", [UUID]),
            Org = jsxd:get(<<"owner">>, <<>>, VMSpec),
            confirm_create(UUID, Org),
            update_timeout(UUID),
            chunter_vmadm:start(UUID);
        {error, E} ->
            lager:error(
              "[create:~s] Failed to create with error: ~p",
              [UUID, E]),
            ls_vm:log(UUID, <<"vmadm failed to create the zone. Please check "
                              "the chunter logs for details">>),
            change_state(UUID, <<"failed">>),
            delete(UUID)
    end.

confirm_create(_UUID, <<>>) ->
    ok;
confirm_create(UUID, Org) ->
    ls_acc:update(Org, UUID, timestamp(),
                  [{<<"event">>, <<"confirm_create">>}]).

update_timeout(UUID) ->
    T = erlang:system_time(milli_seconds) + 60000,
    chunter_vmadm:update(UUID, [{<<"set_internal_metadata">>,
                                 [{<<"docker:wait_for_attach">>, T}]}]).

vm_event(UUID, Event) ->
    libhowl:send(<<"command">>,
                 [{<<"event">>, Event},
                  {<<"uuid">>, fifo_utils:uuid()},
                  {<<"data">>,
                   [{<<"uuid">>, UUID}]}]).
