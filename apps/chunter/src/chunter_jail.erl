-module(chunter_jail).

-export([load/1]).
-define(IOCAGE, "/usr/local/bin/iocage").


load(#{<<"name">> := Name} = VM) ->
    Tag = <<"fifo:", Name/binary>>,
    case fifo_cmd:run(?IOCAGE, ["get", "all", Tag]) of
        {ok, <<"CONFIG_VERSION:4\n", Config/binary>>} ->
            Es = re:split(Config, "\n"),
            KVs = [re:split(E, ":") || E <- Es],
            read_cfg(KVs, VM);
        _ ->
            {error, not_found}
    end.


read_cfg([[<<"allow_quotas">>, Quota] | R], VM) ->
    read_cfg(R, VM#{<<"quota">> => Quota});
read_cfg([[<<"boot">>, <<"on">>] | R], VM) ->
    read_cfg(R, VM#{<<"vm_autoboot">> => true});
read_cfg([[<<"boot">>, <<"off">>] | R], VM) ->
    read_cfg(R, VM#{<<"vm_autoboot">> => false});
read_cfg([[<<"ip4_addr">>, IPData] | R], VM) ->
    %% ip4_addr:
    %% vtnet0|192.168.1.202/24
    RE = "^([a-z0-9]*)\\|([0-9.]*)/([0-9]*)$",
    Opts = [{capture, all_but_first, binary}],
    {match, [NIC, IP, CIDRS]} = re:run(IPData, RE, Opts),
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
