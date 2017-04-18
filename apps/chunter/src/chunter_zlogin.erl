-module(chunter_zlogin).

-define(ZLOGIN_NODE, 'fifo_zlogin@127.0.0.1').

-export([wait/0, start/2, stop/1, subscribe/2, send/3]).

start(UUID, Type) ->
    case global:whereis_name({zlogin, UUID}) of
        undefined ->
            rpc:call(?ZLOGIN_NODE, zlogin_fsm, start, [UUID, Type]);
        R ->
            R
    end.

stop(UUID) ->
    gen_fsm:send_all_state_event(n(UUID), stop).

send(UUID, Type, Data) ->
    start(UUID, Type),
    gen_fsm:send_event(n(UUID), {send, Data}).

subscribe(UUID, Type) ->
    start(UUID, Type),
    gen_fsm:send_all_state_event(n(UUID), {subscribe, self()}).

wait() ->
    case chunter_utils:has_feature(zlogin) of
        true ->
            case net_adm:ping(?ZLOGIN_NODE) of
                pong ->
                    ok;
                _ ->
                    lager:debug("[zlogin] node not yet ready ... waiting ..."),
                    timer:sleep(1000),
                    wait()
            end;
        false ->
            ok
    end.

n(UUID) ->
    {global, {zlogin, UUID}}.
