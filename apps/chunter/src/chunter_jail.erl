-module(chunter_jail).

-export([load/1]).
-define(IOCAGE, "/usr/local/bin/iocage").
-define(REOPTS, [{capture, all_but_first, binary}]).

load(#{<<"name">> := UUID} = VM) ->
    case fifo_cmd:run(?IOCAGE, ["get", "all", UUID]) of
        {ok, <<"CONFIG_VERSION:4\n", Config/binary>>} ->
            Es = re:split(Config, "\n"),
            KVs = [begin
                       {match, [K, V]} = re:run(E, "(.*?):(.*)", ?REOPTS),
                       {K, V}
                   end || E <- Es, E =/= <<>>],
            read_cfg(KVs, VM#{<<"brand">> => <<"jail">>});
        _ ->
            {error, not_found}
    end.


read_cfg([{<<"allow_quotas">>, Quota} | R], VM) ->
    read_cfg(R, VM#{<<"quota">> => Quota});

read_cfg([{<<"tag">>, Alias} | R], VM) ->
    read_cfg(R, VM#{<<"alias">> => Alias});

read_cfg([{<<"boot">>, <<"on">>} | R], VM) ->
    read_cfg(R, VM#{<<"autoboot">> => true});

read_cfg([{<<"boot">>, <<"off">>} | R], VM) ->
    read_cfg(R, VM#{<<"autoboot">> => false});

read_cfg([{<<"memoryuse">>, M} | R], VM) ->
    {match, [Sb, T]} = re:run(M, "([0-9]+)(.):.*", ?REOPTS),
    S = binary_to_integer(Sb),
    Ram = case T of
              <<"G">> ->
                  S * 1024;
              <<"M">> ->
                  S;
              <<"K">> ->
                  S div 1024
          end,
    read_cfg(R, VM#{<<"Ram">> => Ram});


read_cfg([{<<"ip4_addr">>, IPData} | R], VM) ->
    %% ip4_addr:
    %% vtnet0|192.168.1.202/24
    RE = "^([a-z0-9]*)\\|([0-9.]*)/([0-9]*)$",
    {match, [NIC, IP, CIDRS]} = re:run(IPData, RE, ?REOPTS),
    CIDR = binary_to_integer(CIDRS),
    Mask = ft_iprange:cidr_to_mask(CIDR),
    Network = #{
      <<"ip">> => IP,
      <<"interface">> => NIC,
      <<"netmask">> => Mask
     },
    read_cfg(R, VM#{<<"networks">> => [Network]});

read_cfg([_ | R], VM) ->
    read_cfg(R, VM);
read_cfg([], VM) ->
    VM.
