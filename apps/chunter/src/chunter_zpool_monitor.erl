%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 11 Dec 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_zpool_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).


-define(SERVER, ?MODULE).

-record(state, {host, last,
                skipped = 0}).

-define(INTERVAL, 30*1000).

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
    timer:send_interval(dflt_env(zpool_interval, ?INTERVAL), tick),
    {Host, _, _} = chunter_server:host_info(),
    {ok, #state{host = Host}}.

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
handle_info(tick, State = #state{last = Last,
                                 skipped = Skipped}) ->
    case get_stats() of
        Last when Skipped < 120 ->
            {noreply, State};
        Pools ->
            ls_hypervisor:set_pool(State#state.host, Pools),
            {noreply, State#state{skipped = 0, last = Pools}}
    end;



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


get_stats() ->
    Opts = "name,size,alloc,free,dedup,health",
    {ok, Res} = chunter_zfs:zpool(["list", p, 'H', {o, Opts}]),
    lists:foldl(
      fun (Line, Acc) ->
              [Name, Size, Alloc, Free, Dedup, Health] = re:split(Line, "\t"),
              %% TODO: might be able to strip that later
              %% Not all systems return dedup as a integer, some have a float
              %% of the form "1.00x" - we need to sanatize
              D = case binary:last(Dedup) of
                      $x ->
                          round(binary_to_float(strip_x(Dedup, <<>>)) * 100);
                      _ ->
                          binary_to_integer(Dedup)
                  end,
              [{[Name, <<"size">>], bin_to_gb(Size)},
               {[Name, <<"used">>], bin_to_gb(Alloc)},
               {[Name, <<"free">>], bin_to_gb(Free)},
               {[Name, <<"dedup">>], D},
               {[Name, <<"health">>], Health} | Acc]
      end, [], re:split(Res, "\n", [trim])).

strip_x(<<"x">>, Acc) ->
    Acc;
strip_x(<<C, R/binary>>, Acc) ->
    strip_x(R, <<Acc/binary, C>>).

bin_to_gb(B) ->
    round(binary_to_integer(B)/(1024*1024)).

dflt_env(N, D) ->
    case application:get_env(N) of
        undefined ->
            D;
        {ok, V} ->
            V
    end.
