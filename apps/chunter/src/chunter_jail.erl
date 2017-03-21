-module(chunter_jail).

-export([load/1, create/4]).
-define(IOCAGE, "/usr/local/bin/iocage").
-define(REOPTS, [{capture, all_but_first, binary}]).

load(#{<<"name">> := UUID} = VM) ->
    case fifo_cmd:run(?IOCAGE, ["get", "all", UUID]) of
        {ok, Config} ->
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

read_cfg([{<<"tag">>, V} | R], VM) ->
    read_cfg(R, VM#{<<"alias">> => V});

read_cfg([{<<"pcpu">>, V} | R], VM) ->
    read_cfg(R, VM#{<<"cpu_cap">> => V});

read_cfg([{<<"resolver">>, V} | R], VM) ->
    read_cfg(R, VM#{<<"resolvers">> => V});

read_cfg([{<<"host_domainname">>, V} | R], VM) ->
    read_cfg(R, VM#{<<"dns_domain">> => V});

read_cfg([{<<"host_hostname">>, V} | R], VM) ->
    read_cfg(R, VM#{<<"hostname">> => V});


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
    read_cfg(R, VM#{<<"max_physical_memory">> => Ram*1024*1024});


read_cfg([{<<"ip4_addr">>, IPData} | R], VM) ->
    %% ip4_addr:
    %% vtnet0|192.168.1.202/24
    RE = "^([a-z0-9]*)\\|([0-9.]*)/([0-9]*)$",
    case re:run(IPData, RE, ?REOPTS) of
        {match, [NIC, IP, CIDRS]} ->
            CIDR = binary_to_integer(CIDRS),
            Mask = ft_iprange:cidr_to_mask(CIDR),
            Network = #{
              <<"primary">> => true,
              <<"ip">> => IP,
              <<"interface">> => NIC,
              <<"netmask">> => Mask
             },
            read_cfg(R, VM#{<<"nics">> => [Network]});
        _ ->
            read_cfg(R, VM)
    end;
read_cfg([_ | R], VM) ->
    read_cfg(R, VM);
read_cfg([], VM) ->
    VM.

build_tags([], Acc) ->
    Acc;
build_tags([{K, V} | R], Acc) ->
    Acc1 = [io_lib:format("~s=~s", [K, V]) | Acc],
    build_tags(R, Acc1).

create(Dataset, Package, VMSpec, UUID) ->
    lager:info("The very first create request to a omnios hypervisor: ~s.",
               [UUID]),
    {Conf, Release} = chunter_spec:to_iocage(Package, Dataset, VMSpec),
    Tags = build_tags(Conf, []),
    Args =  [{r, Release} | Tags],
    iocage:create(UUID, Args).

%% lager:info("[setup:~s] Starting zone setup.", [UUID]),
%% lager:info("[setup:~s] Waiting for zone to boot for the first time.",
%%            [UUID]),
%% S = <<"svc:/milestone/multi-user-server:default">>,
%% ok = wait_for_started(UUID, S),
%% %% Do the basic setup we can do on the first boot.
%% lager:info("[setup:~s] nsswitch conf.", [UUID]),
%% zlogin(UUID, "cp -f /etc/nsswitch.dns /etc/nsswitch.conf"),
%% lager:info("[setup:~s] dns.", [UUID]),
%% zlogin(UUID, "echo > /etc/resolv.conf"),
%% zlogin(UUID, ["echo \"domain ", Domain, "\" >> /etc/resolv.conf"]),
%% zlogin(UUID, ["echo \"search ", Domain, "\" >> /etc/resolv.conf"]),

%% %% TODO: this is entirely stupid but for some reason we can't do the
%% %% network setup on the first zone boot so for some reason, we'll need
%% %% to reboot the zone before we can go on - bug reported, lets hope someone
%% %% smarter then me knows why this would hapen.
%% lager:info("[setup:~s] Now reboot it.", [UUID]),
%% chunter_vmadm:reboot(UUID),
%% lager:info("[setup:~s] Giveit a few seconds.", [UUID]),
%% timer:sleep(5000),
%% lager:info("[setup:~s] And wait for the system to come up agian.", [UUID]),
%% ok = wait_for_started(UUID, S),
%% lager:info("[setup:~s] Initializing networks.", [UUID]),
%% lists:map(fun({NicBin, Spec}) ->
%%                   Nic = binary_to_list(NicBin),
%%                   {ok, IPBin} = jsxd:get(<<"ip">>, Spec),
%%                   IP = binary_to_list(IPBin),
%%                   {ok, Netmask} = jsxd:get(<<"netmask">>, Spec),
%%                   CIDR = ft_iprange:mask_to_cidr(Netmask),
%%                   CIDRS = integer_to_list(CIDR),
%%                   zlogin(UUID, ["ipadm create-if ", Nic]),
%%                   zlogin(UUID, ["ipadm create-addr -T static",
%%                                 " -a ", IP, "/", CIDRS,
%%                                 " ", Nic, "/v4"]),
%%                   case jsxd:get(<<"primary">>, Spec) of
%%                       {ok, true} ->
%%                           {ok, GWBin} = jsxd:get(<<"gateway">>, Spec),
%%                           GW = binary_to_list(GWBin),
%%                           zlogin(UUID, ["route -p add default ", GW]);
%%                       _ ->
%%                           ok
%%                   end
%%           end, NICS),
%% lager:info("[setup:~s] Jail setup completed.", [UUID]).
