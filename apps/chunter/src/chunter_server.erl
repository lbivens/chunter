%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_server).

-behaviour(gen_server).

-include("chunter_version.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
         host_info/0,
         connect/0,
         update_mem/0,
         reserve_mem/1,
         service_action/2,
         kvm_mem/0,
         host_id/0,
         disconnect/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

-define(CPU_CAP_MULTIPLYER, 8).

-define(SERVER, ?MODULE).

-define(HOST_ID_FILE, "host_id").

-define(MAX_TICK, 10).

-record(state, {name,
                port,
                sysinfo,
                connected = false,
                services = [],
                system,
                capabilities = [],
                total_memory = 0,
                reserved_memory = 0,
                provisioned_memory = 0,
                tick = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

connect() ->
    gen_server:cast(?SERVER, connect).

update_mem() ->
    gen_server:cast(?SERVER, update_mem).

reserve_mem(N) ->
    gen_server:cast(?SERVER, {reserve_mem, N}).

disconnect() ->
    gen_server:cast(?SERVER, disconnect).

host_id() ->
    gen_server:call(?SERVER, host_id).

kvm_mem() ->
    try
        MemStr = os:cmd("NODE_PATH=$NODE_PATH:/usr/vm/node_modules/ "
                        "/usr/node/bin/node -e "
                        "'console.log(require(\"VM\").KVM_MEM_OVERHEAD)'"),
        list_to_integer(MemStr -- "\n")
    catch
        _:_ ->
            1024
    end.


service_action(Action, Service)
  when Action =:= enable;
       Action =:= refresh;
       Action =:= restart;
       Action =:= disable;
       Action =:= clear ->
    gen_server:call(?SERVER, {service, Action, Service}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    System = chunter_utils:system(),
    lager:info([{fifo_component, chunter}],
               "chunter:init.", []),
    %% We subscribe to sniffle register channel - that way we can reregister to
    %% dead sniffle processes.
    mdns_client_lib_connection_event:add_handler(chunter_connect_event),
    %% This is every 10 seconds
    ServiceIVal = application:get_env(chunter, update_services_interval, 10000),
    timer:send_interval(ServiceIVal, update_services),
    register_hypervisor(),
    lists:foldl(
      fun (#{<<"uuid">> := UUID}, _) ->
              chunter_vm_fsm:load(UUID)
      end, 0, chunter_zone:list()),

    {ok, SysInfo} = chunter_utils:sysinfo(),
    Capabilities =
        case System of
            omnios ->
                [<<"ipkg">>];
            solaris ->
                [<<"ipkg">>];
            freebsd ->
                [<<"jail">>];
            smartos ->
                case os:cmd("ls /dev/kvm") of
                    "/dev/kvm\n" ->
                        [<<"zone">>, <<"kvm">>];
                    _ ->
                        [<<"zone">>]
                end
        end,
    {Host, _IPStr, _Port} = host_info(),
    ls_hypervisor:sysinfo(Host, SysInfo),
    ls_hypervisor:version(Host, ?VERSION),
    ls_hypervisor:virtualisation(Host, Capabilities),
    ReservedMem = case application:get_env(reserved_memory) of
                      undefined ->
                          0;
                      {ok, Mem} ->
                          Mem / (1024*1024)
                  end,
    {ok, #state{
            reserved_memory = ReservedMem,
            sysinfo = SysInfo,
            name = Host,
            capabilities = Capabilities,
            system = System
           }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(host_id, _From, #state{name = Name} = State) ->
    {reply, {ok, Name}, State};

handle_call({call, _Auth, Call}, _From, State) ->
    lager:info([{fifo_component, chunter}],
               "unsupported call - ~p", [Call]),
    Reply = {error, {unsupported, Call}},
    {reply, Reply, State};

handle_call({service, _, _}, _From, State = #state{system = freebsd}) ->
    {reply, {error, not_supported}, State};

handle_call({service, enable, Service}, _From, State) ->
    {reply, smurf:enable(Service, []), State};

handle_call({service, refresh, Service}, _From, State) ->
    {reply, smurf:refresh(Service, []), State};

handle_call({service, restart, Service}, _From, State) ->
    {reply, smurf:restart(Service, []), State};

handle_call({service, disable, Service}, _From, State) ->
    {reply, smurf:disable(Service, []), State};

handle_call({service, clear, Service}, _From, State) ->
    {reply, smurf:clear(Service, []), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknwon}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------



handle_cast(update_mem, State = #state{
                                   reserved_memory = ReservedMem,
                                   name = Host
                                  }) ->
    {TotalMem, ProvMem} = mem(),
    lager:info("[~p] Counting ~p MB used out of ~p MB in total.",
               [Host, ProvMem, TotalMem]),
    FreeMem = TotalMem - ReservedMem - ProvMem,
    ls_hypervisor:set_resource(Host, [{[<<"free-memory">>], FreeMem},
                                      {[<<"reserved-memory">>], ReservedMem},
                                      {[<<"provisioned-memory">>], ProvMem},
                                      {[<<"total-memory">>], TotalMem}]),
    {noreply, State#state{
                total_memory = TotalMem,
                provisioned_memory = ProvMem
               }};

handle_cast({reserve_mem, N}, State =
                #state{
                   name = Host,
                   reserved_memory = ReservedMem,
                   total_memory = TotalMem,
                   provisioned_memory = ProvMem
                  }) ->
    ProvMem1 = ProvMem + N,
    Free = TotalMem - ReservedMem - ProvMem1,
    ls_hypervisor:set_resource(Host,
                               [{[<<"free-memory">>], Free},
                                {[<<"provisioned-memory">>], ProvMem1}]),
    {noreply, State#state{
                provisioned_memory = ProvMem1
               }};

handle_cast(connect, #state{name = Host,
                            system = System,
                            capabilities = Caps} = State) ->
    register_hypervisor(),
    ls_hypervisor:sysinfo(Host, State#state.sysinfo),
    ls_hypervisor:version(Host, ?VERSION),
    update_mem(),
    case System of
        smartos ->
            Networks = re:split(
                         os:cmd("cat /usbkey/config  | grep -v '^#' | "
                                "grep '_nic=' | sed 's/_nic.*$//'"),
                         "\n"),
            Networks1 = lists:delete(<<>>, Networks),
            Etherstub = re:split(
                          os:cmd("cat /usbkey/config | grep etherstub | "
                                 "sed -e 's/etherstub=\"\\(.*\\)\"/\\1/'"),
                          ",\\s*|\n"),
            Etherstub1 = lists:delete(<<>>, Etherstub),
            ls_hypervisor:networks(Host, Networks1),
            ls_hypervisor:etherstubs(Host, Etherstub1);
        _ ->
            {ok, Networks} = application:get_env(chunter, network_tags),
            Networks1 = [list_to_binary(N) || {N, _} <- Networks],
            ls_hypervisor:networks(Host, Networks1)
    end,
    ls_hypervisor:virtualisation(Host, Caps),
    register_vms(),
    {noreply, State#state{
                connected = true
               }};

handle_cast(disconnect,  State) ->
    {noreply, State#state{connected = false}};


handle_cast(Msg, #state{name = Name} = State) ->
    lager:warning("[~p] unknown message: ~p.", [Name, Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(update_services, State=#state{tick = _T}) when _T >= ?MAX_TICK ->
    {noreply, State#state{services = [], tick = 0}};
handle_info(update_services, State=#state{system = freebsd}) ->
    update_mem(),
    {noreply, State#state{services = [], tick = 0}};
handle_info(update_services, State=#state{
                                      name=Host,
                                      services = OldServices,
                                      system = System
                                     }) when System =/= freebsd->
    %% We also update the memory while we are at it.
    update_mem(),
    case {chunter_smf:update(OldServices), OldServices} of
        {{ok, ServiceSet, Changed}, []} ->
            lager:debug("[GZ] Initializing ~p Services.",
                        [length(Changed)]),
            ls_hypervisor:set_service(
              Host,
              [{[Srv], St}
               || {Srv, _, St} <- Changed]),
            {noreply, State#state{services = ServiceSet}};
        {{ok, ServiceSet, Changed}, _} ->
            %% Update changes which are not removes
            ls_hypervisor:set_service(
              Host,
              [{[Srv], case SrvState of
                           <<"removed">> ->
                               delete;
                           _ ->
                               SrvState
                       end}
               || {Srv, _, SrvState} <- Changed]),
            update_services(Host, Changed),
            {noreply, State#state{services = ServiceSet}};
        _ ->
            {noreply, State}
    end;

handle_info(timeout, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


host_info() ->
    Path = [code:root_dir(), "/etc/" ?HOST_ID_FILE],
    Host = case application:get_env(chunter, hostname) of
               undefined ->
                   [H|_] = re:split(os:cmd("uname -n"), "\n"),
                   H;
               {ok, H} when is_binary(H) ->
                   H;
               {ok, H} when is_list(H) ->
                   list_to_binary(H)
           end,
    {IPStr, Port} = case application:get_env(chunter, endpoint) of
                        undefined ->
                            {ok, {A, B, C, D}} =
                                inet:getaddr(binary_to_list(Host), inet),
                            {io_lib:format("~p.~p.~p.~p", [A, B, C, D]), 4200};
                        {ok, R} ->
                            R
                    end,
    IPBib = list_to_binary(IPStr),
    HostID = case filelib:is_file([code:root_dir(), "/etc/host_id"]) of
                 true ->
                     F = os:cmd(["cat ", Path]),
                     [HostIDi | _] = re:split(F, "\n"),
                     HostIDi;
                 _ ->
                     UUID = fifo_utils:uuid(hypervisor),
                     file:write_file(Path, UUID),
                     UUID
             end,

    {HostID, IPBib, Port}.


register_hypervisor() ->
    {Host, IPStr, Port} = host_info(),
    lager:info([{fifo_component, chunter}],
               "chunter:init - Host: ~s(~s)", [Host, IPStr]),
    [Alias|_] = re:split(os:cmd("uname -n"), "\n"),
    case ls_hypervisor:get(Host) of
        not_found ->
            ls_hypervisor:register(Host, IPStr, Port),
            ls_hypervisor:alias(Host, Alias);
        {ok, H} ->
            ls_hypervisor:register(Host, IPStr, Port),
            case ft_hypervisor:alias(H) of
                <<>> ->
                    ls_hypervisor:alias(Host, Alias);
                _ ->
                    ok
            end;
        _ ->
            ls_hypervisor:register(Host, IPStr, Port),
            ok
    end.

update_services(_, []) ->
    ok;

update_services(UUID, Changed) ->
    case [{Srv, New} || {Srv, Old, New} <- Changed, Old =/= New] of
        [] ->
            ok;
        Changed1 ->
            libhowl:send(UUID, #{<<"event">> => <<"services">>,
                                 <<"data">> => Changed1})
    end.

mem_total() ->
    case chunter_utils:system() of
        S when S =:= omnios; S =:= solaris; S =:= smartos ->
            mem_solaris_total();
        freebsd ->
            mem_bsd_total()
    end.

mem_bsd_total() ->
    "hw.physmem: " ++ R = os:cmd("sysctl hw.physmem"),
    list_to_integer(lib:nonl(R)) div 1024 div 1024.

mem_solaris_total() ->
    {TotalMem, _} =
        string:to_integer(
          os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'")),
    TotalMem.

mem() ->
    VMS = chunter_zone:list(),
    ProvMemA = lists:foldl(
                 fun (VM, Mem) ->
                         MTpl = {jsxd:get(<<"max_physical_memory">>, VM),
                                 jsxd:get([<<"mcap">>, <<"physcap">>], VM)},
                         case MTpl of
                             {{ok, Mx}, _} ->
                                 Mx;
                             {_, {ok, MCap}} ->
                                 binary_to_integer(MCap);
                             _ ->
                                 0
                         end + Mem
                 end, 0, VMS),
    ProvMem = round(ProvMemA / (1024*1024)),
    TotalMem = mem_total(),
    {TotalMem, ProvMem}.

register_vms() ->
    spawn(fun () ->
                  [begin
                       chunter_vm_fsm:load(UUID),
                       timer:sleep(1000)
                   end || #{uuid := UUID} <- chunter_zone:list_()]
          end).
