%% @doc Interface for howl-admin commands.
-module(chunter_console).
-export([connections/1]).
-ignore_xref([connections/1]).

-export([update_mem/1]).
-ignore_xref([update_mem/1]).


-export([import_dataset/1]).
-ignore_xref([import_dataset/1]).


connections(["snarl"]) ->
    io:format("Snarl endpoints.~n"),
    print_endpoints(libsnarl:servers());

connections(["sniffle"]) ->
    io:format("Sniffle endpoints.~n"),
    print_endpoints(libsniffle:servers());

connections(["howl"]) ->
    io:format("Howl endpoints.~n"),
    print_endpoints(libsniffle:servers());

connections([]) ->
    case {connections(["snarl"]),
          connections(["sniffle"]),
          connections(["howl"])} of
        {ok, ok, ok} ->
            ok;
        _ ->
            error
    end.

import_dataset([UUIDs]) ->
    UUID = list_to_binary(UUIDs),
    io:format("Importing dataset ~s, this can take a moment.~n", [UUID]),
    chunter_dataset_srv:install(UUID, undefined).

update_mem([]) ->
    io:format("Updating memory.~n", []),
    chunter_server:update_mem().


print_endpoints(Es) ->
    io:format("Hostname            "
              "                    "
              " Node               "
              " Errors    ~n"),
    io:format("--------------------"
              "----------"
              " --------------------"
              " ---------------~n", []),
    case [print_endpoint(E) || E <- Es] of
        [] ->
            error;
        _ ->
            ok
    end.

print_endpoint({{Hostname, [{port, Port}, {ip, IP}]}, _, Fails}) ->
    HostPort = <<IP/binary, ":", Port/binary>>,
    io:format("~30s ~-24s ~9b~n", [Hostname, HostPort, Fails]).
