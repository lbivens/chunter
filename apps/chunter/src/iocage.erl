-module(iocage).

-export([list/0, destroy/1, start/1, stop/1, restart/1, create/2]).

-define(IOCAGE, "/usr/local/bin/iocage").

list() ->
    run(["list", 'H', l]).

start(UUID) ->
    run(["start", UUID]).

stop(UUID) ->
    run(["stop", UUID]).

restart(UUID) ->
    run(["restart", UUID]).

destroy(UUID) ->
    stop(UUID),
    run(["destroy", f, UUID]).

create(UUID, Tags) ->
    run(["create", <<"--uuid ", UUID/binary>> | Tags]).

run(Cmd) ->
    lager:debug("[iocage] ~p", [Cmd]),
    fifo_cmd:run(?IOCAGE, Cmd).
