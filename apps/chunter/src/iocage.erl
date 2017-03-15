-module(iocage).

-export([list/0, start/1, destroy/1, start/1, stop/1, restart/1]).

-define(IOCAGE, "/usr/local/bin/iocage").

list() ->
    run(["list", 'H']).

start(UUID) ->
    run(["start", UUID]).

stop(UUID) ->
    run(["stop", UUID]).

restart(UUID) ->
    run(["restart", UUID]).

destroy(UUID) ->
    run(["destroy", f, UUID]).

run(Cmd) ->
    fifo_cmd:run(?IOCAGE, Cmd).
