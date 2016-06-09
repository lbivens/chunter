-module(chunter_imgadm).

-export([import/1, get/1]).

-define(IMGADM, "/usr/sbin/imgadm").

get(Image) ->
    fifo_cmd:run_json(?IMGADM, ["get", Image]).

import(Image) ->
    case fifo_cmd:run(?IMGADM, ["import", q, Image]) of
        {ok, <<"Importing ", UUID:36/binary, _/binary>>} ->
            {ok, UUID};
        {ok, <<"Image ", UUID:36/binary, _/binary>>} ->
            {ok, UUID};
        Error ->
            lager:error("[imgadm] Could not import image ~s: ~p",
                        [Image, Error]),
            Error
    end.
