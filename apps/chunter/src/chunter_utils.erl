%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 May 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_utils).

-export([sysinfo/0, system/0]).


sysinfo() ->
    case system() of
        smartos ->
            BinResponse = list_to_binary(os:cmd("sysinfo")),
            SysInfo0 = jsone:decode(BinResponse),
            SysInfo = jsxd:delete([<<"Boot Parameters">>, <<"root_shadow">>],
                                  SysInfo0),
            {ok, SysInfo};
        omnios ->
            {ok, []};
        _ ->
            {ok, []}
    end.

system() ->
    case os:cmd("uname -v") of
        "joyent" ++ _ ->
            smartos;
        "omnios" ++ _ ->
            omnios;
        "11." ++ _ ->
            solaris;
        _ ->
            undefined
    end.
