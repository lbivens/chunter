%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_dataset_srv).

-behaviour(gen_server).

%% API
-export([install/2, start_link/0]).
-ignore_xref([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(WRITE_RETRY, 10).

-record(state, {}).

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

install(DatasetUUID, UUID) ->
    gen_server:call(?SERVER, {install, DatasetUUID, UUID}, infinity).

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
    {ok, #state{}}.

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
handle_call({install, DatasetUUID, UUID}, _From, State) ->
    try install_image(DatasetUUID, UUID) of
        Reply ->
            {reply, Reply, State}
    catch
        E1:E2 ->
            lager:error("Dataset import failed: ~p:~p", [E1, E2]),
            {reply, {error, E1}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
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

-spec install_image(DatasetUUID::fifo:uuid(), VM::fifo:uuid()) -> ok | string().

install_image(DatasetUUID, VM) ->
    lager:debug("Installing dataset ~s.", [DatasetUUID]),
    Path = filename:join(<<"/zones">>, DatasetUUID),
    lager:debug("Checking path ~s.", [Path]),
    case os:cmd("zfs list zones/" ++ binary_to_list(DatasetUUID) ++
                    ">/dev/null; echo $?") of
        "0\n" ->
            lager:debug("found.", []),
            ok;
        _ ->
            do_download(DatasetUUID, VM)
    end.

do_download(DatasetUUID, VM) ->
    case libsniffle:s3(image) of
        {ok, {S3Host, S3Port, AKey, SKey, Bucket}} ->
            Chunk = case application:get_env(chunter, download_chunk) of
                        undefined ->
                            5242880;
                        {ok, S} ->
                            S
                    end,
            {ok, Download} = fifo_s3_download:new(
                               AKey, SKey, S3Host, S3Port, Bucket,
                               DatasetUUID, [{chunk_size, Chunk}]),
            {Cmd, B} = case fifo_s3_download:get(Download) of
                           {ok, <<31:8, 139:8, _/binary>> = AB} ->
                               {"/zfs_receive.gzip.sh", AB};
                           {ok, <<"BZh", _/binary>> = AB} ->
                               {"/zfs_receive.bzip2.sh", AB}
                       end,
            Cmd1 = code:priv_dir(chunter) ++  Cmd,
            Ctx1 = crypto:hash_update(crypto:hash_init(sha), B),
            {ok, D} = ls_dataset:get(DatasetUUID),
            lager:debug("not found going to run: ~s ~s.",
                        [Cmd1, DatasetUUID]),
            Port = open_port({spawn_executable, Cmd1},
                             [{args, [DatasetUUID]}, use_stdio, binary,
                              stderr_to_stdout, exit_status]),
            port_command(Port, B),
            case chunter_snap:download_to_port(
                   Port, Download, VM, ft_dataset:sha1(D), Ctx1, 1) of
                {ok, done} ->
                    finish_image(DatasetUUID);
                E ->
                    E
            end;
        {ok, no_s3} ->
            {ok, DatasetUUID} = chunter_imgadm:import(DatasetUUID),
            ok
    end.

finish_image(UUID) ->
    UUIDL = binary_to_list(UUID),
    {ok, DS} = ls_dataset:get(UUID),
    Manifest = #{
      <<"manifest">> => #{
          <<"v">> => 2,
          <<"uuid">> => UUID,
          <<"disabled">> => false,
          <<"type">> => <<"zvol">>,
          <<"state">> => <<"active">>
         },
      <<"zpool">> => <<"zones">>},
    %% Need to set the correct type
    Manifest1 = case ft_dataset:type(DS) of
                    zone ->
                        case ft_dataset:zone_type(DS) of
                            lx ->
                                jsxd:set([<<"manifest">>, <<"type">>],
                                         <<"lx-dataset">>, Manifest);
                            docker ->
                                jsxd:set([<<"manifest">>, <<"type">>],
                                         <<"docker">>, Manifest);
                            _ ->
                                jsxd:set([<<"manifest">>, <<"type">>],
                                         <<"zone-dataset">>, Manifest)
                        end;
                    _ ->
                        Manifest
                end,
    %% and write it to zoneamd's new destination folder ...
    file:write_file("/var/imgadm/images/zones-" ++ UUIDL ++ ".json",
                    jsone:encode(Manifest1)),
    Cmd = "zfs list -Hp -t all -r  zones/" ++ UUIDL,

    wait_image(0, Cmd).


wait_image(N, Cmd) when N < 3 ->
    timer:sleep(5000),
    wait_image(length(re:split(os:cmd(Cmd), "\n")), Cmd);

wait_image(_, _) ->
    lager:debug("<IMG> done waiting.", []),
    ok.
