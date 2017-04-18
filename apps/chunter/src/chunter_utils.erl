%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 May 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(chunter_utils).

-export([has_feature/1, sysinfo/0, system/0, vm_cli/0]).


-type vm_cli() :: iocage | zoneadm | vmadm.
-type feature() :: zlogin | zdoor | sysinfo | zonemon | vm_cli().
-type system() :: smartos | omnios | solaris | freebsd.



sysinfo() ->
    case has_feature(sysinfo) of
        true ->
            BinResponse = list_to_binary(os:cmd("sysinfo")),
            SysInfo0 = jsone:decode(BinResponse),
            SysInfo = jsxd:delete([<<"Boot Parameters">>, <<"root_shadow">>],
                                  SysInfo0),
            {ok, SysInfo};
        false ->
            {ok, []}
    end.

-spec system() -> system().

system() ->
    case application:get_env(chunter, system) of
        undefined ->
            S = system_(),
            application:set_env(chunter, system, S),
            S;
        {ok, S} ->
            S
    end.

-spec system_() -> system().

system_() ->
    case os:cmd("uname -v") of
        "joyent" ++ _ ->
            smartos;
        "omnios" ++ _ ->
            omnios;
        "11." ++ _ ->
            solaris;
        "FreeBSD " ++ _ ->
            freebsd;
        _ ->
            undefined
    end.

-spec has_feature(feature()) -> boolean().

has_feature(Feature) ->
    has_feature(Feature, system()).


-spec vm_cli() -> vm_cli().
vm_cli() ->
     case {has_feature(vmadm),
           has_feature(zoneadm),
           has_feature(iocage)} of
         {true, _, _} ->
             vmadm;
         {_, true, _} ->
             zoneadm;
         {_, _, true} ->
             iocage
     end.

-spec has_feature(feature(), system()) -> boolean().

has_feature(sysinfo, smartos) ->
    true;

has_feature(zlogin, smartos) ->
    true;
has_feature(zlogin, omnios) ->
    true;
has_feature(zlogin, solaris) ->
    true;

has_feature(zonemon, smartos) ->
    true;
has_feature(zonemon, omnios) ->
    true;
has_feature(zonemon, solaris) ->
    true;

has_feature(zdoor, smartos) ->
    true;
has_feature(zdoor, omnios) ->
    true;
has_feature(zdoor, solaris) ->
    true;

has_feature(vmadm, smartos) ->
    true;
has_feature(zoneadm, omnios) ->
    true;
has_feature(zoneadm, solaris) ->
    true;
has_feature(iocage, freebsd) ->
    true;

has_feature(_, _) ->
    false.
