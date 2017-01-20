%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------

-module(chunter_spec).

-export([to_vmadm/3,
         to_zonecfg/3,
         to_sniffle/1,
         create_update/3
        ]).

-define(EMPTY, <<"00000000-0000-0000-0000-000000000000">>).


-spec to_vmadm(Package::fifo:config(), Dataset::fifo:config(),
               OwnerData::fifo:config()) -> fifo:vm_config().

to_vmadm(Package, Dataset, OwnerData) ->
    case jsxd:get(<<"type">>, Dataset) of
        {ok, <<"kvm">>} ->
            generate_spec(Package, Dataset, OwnerData);
        {ok, <<"zone">>} ->
            generate_spec(Package, Dataset, OwnerData)
    end.

-spec to_sniffle(Spec::fifo:vm_config()) -> fifo:config_list().

to_sniffle(Spec) ->
    Spec1 = jsxd:from_list(Spec),
    Type = brand_to_type(jsxd:get(<<"brand">>, <<"joyent">>, Spec1)),
    generate_sniffle(Spec1, Type).

to_zonecfg(Package, Dataset, OwnerData) ->
    generate_zonecfg(Package, Dataset, OwnerData).

brand_to_type(<<"ipkg">>) ->
    ipkg;
brand_to_type(<<"lipkg">>) ->
    ipkg;
brand_to_type(<<"kvm">>) ->
    kvm;
brand_to_type(<<"lx">>) ->
    zone;
brand_to_type(<<"sngl">>) ->
    zone;
brand_to_type(<<"joyent">>) ->
    zone;
brand_to_type(<<"joyent-minimal">>) ->
    zone.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec generate_sniffle(Spec::fifo:vm_config(),
                       Type::fifo:vm_type()) -> fifo:config_list().

generate_sniffle(In, _Type) ->
    KeepKeys =
        [<<"state">>, <<"alias">>, <<"quota">>, <<"cpu_cap">>, <<"routes">>,
         <<"zfs_io_priority">>, <<"disk_driver">>, <<"vcpus">>,
         <<"nic_driver">>, <<"hostname">>, <<"autoboot">>, <<"created_at">>,
         <<"dns_domain">>, <<"resolvers">>, <<"ram">>, <<"uuid">>,
         <<"maintain_resolvers">>, <<"cpu_shares">>, <<"max_swap">>,
         <<"kernel_version">>, <<"indestructible_zoneroot">>,
         <<"indestructible_delegated">>, <<"boot_timestamp">>, <<"datasets">>],
    Sniffle0 = jsxd:select(KeepKeys, In),
    jsxd:fold(fun translate_to_sniffle/3, Sniffle0, In).

translate_to_sniffle(<<"owner_uuid">>, ?EMPTY, Obj) ->
    Obj;
translate_to_sniffle(<<"owner_uuid">>, V, Obj) ->
    jsxd:set(<<"owner">>, V, Obj);
translate_to_sniffle(<<"internal_metadata">>, Int, Obj) ->
    jsxd:merge(Int, Obj);
translate_to_sniffle(<<"dataset_uuid">>, V, Obj) ->
    jsxd:set(<<"dataset">>, V, Obj);
translate_to_sniffle(<<"image_uuid">>, V, Obj) ->
    jsxd:set(<<"dataset">>, V, Obj);
translate_to_sniffle(<<"package_name">>, V, Obj) ->
    jsxd:set(<<"package">>, V, Obj);
translate_to_sniffle(<<"docker">>, true, Obj) ->
    jsxd:set(<<"zone_type">>, <<"docker">>, Obj);
translate_to_sniffle(<<"brand">>, Brand, Obj) ->
    case brand_to_type(Brand) of
        kvm ->
            jsxd:set(<<"type">>, <<"kvm">>, Obj);
        zone ->
            O1 = jsxd:set(<<"type">>, <<"zone">>, Obj),
            case {Brand, jsxd:get(<<"zone_type">>, O1)} of
                {<<"lx">>, undefined} ->
                    jsxd:set(<<"zone_type">>, <<"lx">>, O1);
                _ ->
                    O1
            end;
        ipkg ->
            jsxd:set(<<"type">>, <<"ipkg">>, Obj)
    end;
translate_to_sniffle(<<"max_physical_memory">>, V, Obj) ->
    jsxd:update(<<"ram">>, fun(E) -> E end, round(V/(1024*1024)), Obj);
translate_to_sniffle(<<"zonepath">>, V, Obj) ->
    Result = os:cmd(binary_to_list(<<"/usr/sbin/zfs list -pH ", V/binary>>)),
    Parts = re:split(Result, "\t", [{return, list}]),
    case Parts of
        [_, Used, Avail | _] ->
            {UsedI, _} = string:to_integer(Used),
            {AvailI, _} = string:to_integer(Avail),
            Quota = round((UsedI + AvailI) / (1024*1024*1024)),
            jsxd:thread([{set, <<"zonepath">>, V},
                         {set, <<"quota">>, Quota}], Obj);
        _ ->
            jsxd:set(<<"zonepath">>, V, Obj)
    end;
translate_to_sniffle(<<"customer_metadata">>, V, Obj) ->
    jsxd:fold(fun (<<"root_authorized_keys">>, V1, Obj1) ->
                      jsxd:set(<<"ssh_keys">>, V1, Obj1);
                  (K, V1, Obj1) ->
                      jsxd:set([<<"metadata">>, K], V1, Obj1)
              end, Obj, V);

translate_to_sniffle(<<"disks">>, Disks, Obj) ->
    {ObjOut, Size} = jsxd:fold(
                       fun(_, Disk, {Obj1, Sum}) ->
                               Size = jsxd:get(<<"size">>, 0, Disk),
                               Total = Sum + round(Size / 1024),
                               case jsxd:get(<<"image_uuid">>, Disk) of
                                   {ok, Dataset} ->
                                       {jsxd:set(<<"dataset">>, Dataset, Obj1),
                                        Total};
                                   _ ->
                                       {Obj1, Total}
                               end
                       end, {Obj, 0}, Disks),
    jsxd:thread([{set, <<"quota">>, Size},
                 {set, <<"disks">>, Disks}], ObjOut);
translate_to_sniffle(<<"nics">>, V, Obj) ->
    jsxd:set(<<"networks">>, V, Obj);
translate_to_sniffle(_, _, Obj) ->
    Obj.


generate_zonecfg(Package, Dataset, OwnerData) ->
    Ram = ft_package:ram(Package),
    RamShare = ram_shares(Ram),
    MaxSwap = dflt(ft_package:max_swap(Package), Ram * 2),
    {ok, NicsIn} = jsxd:get(<<"nics">>, OwnerData),
    NicsIn1 = jsxd:set([0, <<"primary">>], true, NicsIn),
    lager:info("nics: ~p", [NicsIn1]),
    MaxSwap1 = erlang:max(256, MaxSwap) * 1024 * 1024,
    {ok, UUID} = jsxd:get(<<"uuid">>, OwnerData),
    NICs = [chunter_nic_srv:get_vnic(N) || N <- NicsIn1],
    {ok, ZoneRootS} = application:get_env(chunter, zone_root),
    ZoneRoot = list_to_binary(ZoneRootS),
    Base = [create,
            {zonename, UUID},
            {zonepath, <<ZoneRoot/binary, "/", UUID/binary>>},
            {brand, ft_dataset:zone_type(Dataset)},
            {autoboot, true},
            {limitpriv, [default, dtrace_proc, dtrace_user]},
            {'ip-type', exclusive}],
    Network = [{add, net, [{physical, NIC}]} || {NIC, _} <- NICs],
    RamB = Ram * 1024 * 1024,
    CPUShares = dflt(ft_package:cpu_shares(Package), RamShare),
    ZFSShares = dflt(ft_package:cpu_shares(Package), RamShare),
    CPUCap = ft_package:cpu_cap(Package),
    RCTL = [{rctl, <<"zone.cpu-shares">>, privileged, CPUShares, none},
            %% Constants
            {rctl, <<"zone.max-lwps">>,    privileged, 2000, deny},
            {rctl, <<"zone.max-msg-ids">>, privileged, 4096, deny},
            {rctl, <<"zone.max-sem-ids">>, privileged, 4096, deny},
            {rctl, <<"zone.max-shm-ids">>, privileged, 4096, deny},
            %% Seems = mem/2
            {rctl, <<"zone.max-shm-memory">>, privileged, RamB div 2, deny},
            {rctl, <<"zone.zfs-io-priority">>, privileged, ZFSShares, none},
            {rctl, <<"zone.cpu-cap">>, privileged, CPUCap, deny},
            {add, 'capped-memory',
             [{physical, RamB},
              {swap, MaxSwap1},
              {locked, RamB}]}],
    %% TODO: find a workaround, for some reason those things collide,
    %% is it the same as the others, where is the difference?
    %% {rctl, <<"zone.max-physical-memory">>, privileged, RamB, deny},
    %% {rctl, <<"zone.max-locked-memory">>, privileged, RamB, deny},
    %% {rctl, <<"zone.max-swap">>, privileged, MaxSwap1, deny}],
    Owner = jsxd:get(<<"owner">>, <<"00000000-0000-0000-0000-000000000000">>,
                     OwnerData),
    lager:warning("[TODO] We don't generate a propper timestamp here"),
    Time = <<"2015-04-26T11:29:31.297Z">>,
    Attr = [{attr, <<"vm-version">>, string, 1},
            %% TODO: generte proper date
            {attr, <<"create-timestamp">>, string, Time},
            {attr, <<"owner-uuid">>, string, Owner},
            {attr, <<"tmpfs">>, string, Ram * 2}],
    Opt = jsxd:fold(fun (<<"resolvers">>, V, Acc) ->
                            [{attr, <<"resolvers">>, string, V} | Acc];
                        (<<"hostname">>, V, Acc) ->
                            [{attr, <<"hostname">>, string, V} | Acc];
                        (<<"alias">>, V, Acc) ->
                            Base64 = base64:encode(V),
                            [{attr, <<"alias">>, string,
                              <<"\"", Base64/binary, "\"">>} | Acc];
                        (K, _V, Acc) ->
                            lager:warning("[zonecfg] Unsupported key: ~s", [K]),
                            Acc
                    end, [], OwnerData),
    Finish = [verify,
              commit,
              exit],
    {NICs, Base ++ Network ++ RCTL ++ Attr ++ Opt ++ Finish}.

-spec generate_spec(Package::fifo:config(),
                    Dataset::fifo:config(),
                    OwnerData::fifo:config()) -> fifo:vm_config().

generate_spec(Package, Dataset, OwnerData) ->
    {ok, Ram} = jsxd:get(<<"ram">>, Package),
    {ok, PackageUUID} = jsxd:get(<<"uuid">>, Package),
    {ok, CPUCap} = jsxd:get([<<"cpu_cap">>], Package),
    RamShare = ram_shares(Ram),
    MaxSwap = jsxd:get(<<"max_swap">>, Ram*2, Package),
    MaxSwap1 = erlang:max(256, MaxSwap),
    DefaultResolversS = application:get_env(chunter, resolvers,
                                            "8.8.8.8,8.8.4.4"),
    DefaultResolvers = re:split(DefaultResolversS, ","),
    Autoboot = jsxd:get(<<"autoboot">>, true,  OwnerData),
    CPUShares =jsxd:get(<<"cpu_shares">>, RamShare, Package),
    Owner = jsxd:get(<<"owner">>, <<"00000000-0000-0000-0000-000000000000">>,
                     OwnerData),
    ZFSIOPriority = jsxd:get(<<"zfs_io_priority">>, RamShare, Package),
    Base0 = jsxd:thread([{select, [<<"uuid">>, <<"alias">>, <<"routes">>,
                                   <<"nics">>, <<"indestructible_zoneroot">>]},
                         {set, [<<"nics">>, 0, <<"primary">>], true},
                         {set, <<"autoboot">>, Autoboot},
                         {set, <<"resolvers">>, DefaultResolvers},
                         {set, <<"cpu_shares">>, CPUShares},
                         {set, <<"max_swap">>, MaxSwap1},
                         {set, <<"owner_uuid">>, Owner},
                         {set, <<"zfs_io_priority">>, ZFSIOPriority},
                         {set, <<"package_name">>, PackageUUID},
                         {set, <<"cpu_cap">>, CPUCap},
                         {merge, jsxd:select([<<"nic_driver">>,
                                              <<"disk_driver">>], Dataset)}],
                        OwnerData),
    Base1 = case jsxd:get(<<"type">>, Dataset) of
                {ok, <<"kvm">>} ->
                    kvm_spec(Base0, Package, Dataset);
                {ok, <<"zone">>} ->
                    zone_spec(Base0, Package, Dataset, OwnerData)
            end,
    Result = jsxd:fold(fun (<<"ssh_keys">>, V, Obj) ->
                               jsxd:set([<<"customer_metadata">>,
                                         <<"root_authorized_keys">>], V, Obj);
                           (<<"resolvers">>, V, Obj) ->
                               jsxd:set(<<"resolvers">>, V, Obj);
                           (<<"hostname">>, V, Obj) ->
                               jsxd:set(<<"hostname">>, V, Obj);
                           (<<"metadata">>, V, Obj) ->
                               jsxd:update(<<"customer_metadata">>,
                                           fun(M) ->
                                                   jsxd:merge(M, V)
                                           end, V, Obj);
                           (<<"note">>, V, Obj) ->
                               jsxd:set([<<"internal_metadata">>, <<"note">>],
                                        V, Obj);
                           (<<"network_map">>, V, Obj) ->
                               jsxd:set([<<"internal_metadata">>,
                                         <<"network_map">>], V, Obj);
                           (K, V, Obj) ->
                               case re:run(K, "_pw$") of
                                   nomatch ->
                                       Obj;
                                   _ ->
                                       jsxd:set([<<"internal_metadata">>, K],
                                                V, Obj)
                               end
                       end, Base1, OwnerData),
    lager:debug("Converted ~p / ~p / ~p to: ~p.",
                [Package, Dataset, OwnerData, Result]),
    Result.


-spec create_update(Original::fifo:config(),
                    Package::fifo:config() | undefined,
                    Config::fifo:config()) -> fifo:config_list().

create_update(_, undefined, Config) ->
    KeepKeys = [<<"resolvers">>, <<"hostname">>, <<"alias">>, <<"remove_nics">>,
                <<"add_nics">>, <<"update_nics">>, <<"autoboot">>,
                <<"max_swap">>, <<"set_routes">>, <<"remove_routes">>,
                <<"maintain_resolvers">>, <<"indestructible_zoneroot">>,
                <<"indestructible_delegated">>],
    MDataFun = fun (<<"ssh_keys">>, V, Obj) ->
                       jsxd:set([<<"set_customer_metadata">>,
                                 <<"root_authorized_keys">>], V, Obj);
                   (<<"metadata">>, V, Obj) ->
                       jsxd:update(<<"set_customer_metadata">>,
                                   fun(M) ->
                                           jsxd:merge(M, V)
                                   end, V, Obj);
                   (<<"note">>, V, Obj) ->
                       jsxd:set([<<"set_internal_metadata">>, <<"note">>],
                                V, Obj);
                   (<<"owner">>, V, Obj) ->
                       jsxd:set([<<"owner_uuid">>], V, Obj);
                   (K, V, Obj) ->
                       case re:run(K, "_pw$") of
                           nomatch ->
                               Obj;
                           _ ->
                               jsxd:set([<<"set_internal_metadata">>, K],
                                        V, Obj)
                       end
               end,
    Result = jsxd:fold(MDataFun, jsxd:select(KeepKeys, Config), Config),
    R1 = case jsxd:get(<<"add_nics">>, Result) of
             {ok, Nics} ->
                 Nics1 = [jsxd:update([<<"model">>],
                                      fun(D) ->
                                              D
                                      end, <<"virtio">>, N) ||
                             N <- Nics],
                 jsxd:set(<<"add_nics">>, Nics1, Result);
             _ ->
                 Result
         end,
    lager:debug("Generated update: ~s.~n", [jsone:encode(R1)]),
    R1;

create_update(Original, Package, Config) ->
    Base = create_update(Original, undefined, Config),
    {ok, Ram} = jsxd:get(<<"ram">>, Package),
    {ok, Q} = jsxd:get(<<"quota">>, Package),
    {ok, PackageUUID} = jsxd:get(<<"uuid">>, Package),
    RamShare = ram_shares(Ram),
    MaxSwap = jsxd:get(<<"max_swap">>, Ram*2, Package),
    MaxSwap1 = erlang:max(256, MaxSwap),
    CPUShares = jsxd:get(<<"cpu_shares">>, RamShare, Package),
    ZFSPriority = jsxd:get(<<"zfs_io_priority">>, RamShare*2, Package),
    Base0 = jsxd:thread([{set, <<"package_name">>, PackageUUID},
                         {set, <<"cpu_shares">>, CPUShares},
                         {set, <<"zfs_io_priority">>, ZFSPriority},
                         {set, <<"max_swap">>, MaxSwap1},
                         {merge, jsxd:select([<<"cpu_cap">>], Package)}],
                        Base),
    Type = brand_to_type(jsxd:get(<<"brand">>, <<"joyent">>, Original)),
    Result = case Type of
                 kvm ->
                     {ok, CPUCap} = jsxd:get(<<"cpu_cap">>, Base0),
                     VCPUs = ceiling(CPUCap/100.0),
                     Base1 = jsxd:thread([{set, <<"ram">>, Ram},
                                          {set, <<"vcpus">>, VCPUs},
                                          {set, <<"max_physical_memory">>,
                                           Ram + chunter_server:kvm_mem()}],
                                         Base0),
                     case update_disk(jsxd:get(<<"disks">>, [], Original)) of
                         {ok, Path} ->

                             jsxd:set(<<"update_disks">>,
                                      [#{<<"path">> => Path,
                                         <<"size">> => Q * 1024}],
                                      Base1);
                         _ ->
                             Base1
                     end;
                 zone ->
                     Comp = jsxd:get([<<"compression">>], <<"off">>, Package),
                     jsxd:thread([{set, <<"max_physical_memory">>, Ram},
                                  {set, <<"quota">>, Q},
                                  {set, <<"zfs_root_compression">>, Comp},
                                  {set, <<"zfs_data_compression">>, Comp}],
                                 Base0)
             end,
    lager:debug("Created Update package ~p / ~p / ~p to: ~p.",
                [Original, Package, Config, Result]),
    Result.

update_disk([]) ->
    error;

update_disk([Disk | Rest]) ->
    case {jsxd:get(<<"media">>, Disk), jsxd:get(<<"boot">>, Disk)} of
        {{ok, <<"disk">>}, {ok, false}} ->
            jsxd:get(<<"path">>, Disk);
        _ ->
            update_disk([Disk | Rest])
    end.

-spec ceiling(X::float()) -> integer().

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg =< 0 -> T;
        Pos when Pos > 0 -> T + 1
    end.

ram_shares(Ram) ->
    Output = os:cmd("/usr/sbin/prtconf | grep Memor | awk '{print $3}'"),
    RamPerc = case string:to_integer(Output) of
                  {TotalMem, _} when is_number(TotalMem),
                                     TotalMem =/= 0 ->
                      Ram/TotalMem;
                  _ ->
                      0
              end,
    round(1024*RamPerc).

kvm_spec(Base, Package, Dataset) ->
    {ok, ImageID} = jsxd:get(<<"uuid">>, Dataset),
    {ok, Ram} = jsxd:get(<<"ram">>, Package),
    {ok, CPUCap} = jsxd:get(<<"cpu_cap">>, Base),
    {ok, DatasetSize} = jsxd:get(<<"image_size">>, Dataset),
    {ok, Q} = jsxd:get(<<"quota">>, Package),
    VCPUs = ceiling(CPUCap/100.0),
    Base1 = jsxd:thread([{set, <<"ram">>, Ram},
                         {set, <<"vcpus">>, VCPUs},
                         {set, <<"brand">>, <<"kvm">>},
                         {set, <<"max_physical_memory">>,
                          Ram + chunter_server:kvm_mem()},
                         {set, [<<"disks">>, 0, <<"boot">>], true},
                         {set, [<<"disks">>, 0, <<"image_size">>], DatasetSize},
                         {set, [<<"disks">>, 0, <<"image_uuid">>], ImageID},
                         {set, [<<"disks">>, 1, <<"boot">>], false},
                         {set, [<<"disks">>, 1, <<"size">>], Q * 1024}],
                        Base),
    perhaps_map(
      Package, Base1,
      [{<<"compression">>, [<<"disks">>, 1, <<"compression">>]},
       {<<"blocksize">>, [<<"disks">>, 1, <<"blocksize">>]}]).

zone_spec(Base0, Package, Dataset, OwnerData) ->
    {ok, ImageID} = jsxd:get(<<"uuid">>, Dataset),
    {ok, Ram} = jsxd:get(<<"ram">>, Package),
    {ok, Quota} = jsxd:get(<<"quota">>, Package),
    Base1 = jsxd:thread([{set, <<"max_physical_memory">>, Ram},
                         {set, <<"brand">>, <<"joyent">>},
                         {set, <<"quota">>, Quota},
                         {set, <<"image_uuid">>, ImageID}],
                        Base0),

    Base2 = perhaps_set(<<"compression">>, Package,
                        [<<"zfs_root_compression">>], Base1),
    Base3 = perhaps_set(<<"compression">>, Package,
                        [<<"zfs_data_compression">>], Base2),
    Base4 = perhaps_set(<<"delegate_dataset">>, OwnerData,
                        [<<"delegate_dataset">>], Base3),
    Base5 = perhaps_set(<<"indestructible_delegated">>, OwnerData,
                        [<<"indestructible_delegated">>], Base4),
    Base6 = perhaps_set(<<"maintain_resolvers">>, OwnerData,
                        [<<"maintain_resolvers">>], Base5),
    case jsxd:get([<<"zone_type">>], Dataset) of
        {ok, <<"lx">>} ->
            lx_spec(Base6, Dataset);
        {ok, <<"docker">>} ->
            docker_spec(Base6, Dataset, OwnerData);
        _ ->
            Base6
    end.

lx_spec(Base, Dataset) ->
    {ok, KVersion} = jsxd:get([<<"kernel_version">>], Dataset),
    jsxd:thread(
      [{delete, <<"dns_domain">>},
       {set, <<"kernel_version">>, KVersion},
       {set, <<"brand">>, <<"lx">>}], Base).

docker_spec(Base, Dataset, OwnerData) ->
    {ok, ImageID} = jsxd:get(<<"uuid">>, Dataset),
    Base1 = lx_spec(Base, Dataset),
    DockerData = jsxd:get([<<"docker">>], [], OwnerData),
    Base2 = jsxd:thread(
              [{set, <<"docker">>, true},
               {set, <<"autoboot">>, false},
               {set, <<"internal_metadata_namespaces">>, [<<"docker">>]},
               {set, <<"init_name">>, <<"/native/usr/vm/sbin/dockerinit">>},
               %% What a hack :/
               {set, [<<"internal_metadata">>, <<"docker:wait_for_attach">>],
                erlang:system_time(milli_seconds) + 60000}
              ], Base1),
    Base3 = encode_docker_metadata(Base2, DockerData),
    %% We we did not have a docker command we see if there is one in the image
    case jsxd:get([<<"docker">>, <<"cmd">>], OwnerData) of
        {ok, _} ->
            Base3;
        _ ->
            {ok, Manifest} = chunter_imgadm:get(ImageID),
            ManifestCmdPath = [<<"manifest">>, <<"tags">>, <<"docker:config">>,
                               <<"Cmd">>],
            case jsxd:get(ManifestCmdPath, Manifest) of
                {ok, Cmd} ->
                    CmdS = jsone:encode(Cmd),
                    jsxd:set([<<"internal_metadata">>,
                              <<"docker:cmd">>], CmdS, Base3);
                _ ->
                    Base3
            end
    end.

encode_docker_metadata(Base, DockerData) ->
    jsxd:fold(fun (K, V, Acc) ->
                      PrefixedName = <<"docker:", K/binary>>,
                      Path = [<<"internal_metadata">>, PrefixedName],
                      jsxd:set(Path, V, Acc)
              end, Base, DockerData).

perhaps_set(Key, Src, Target, Obj) ->
    case jsxd:get(Key, Src) of
        {ok, Value} ->
            jsxd:set(Target, Value, Obj);
        _ ->
            Obj
    end.

perhaps_map(Src, Obj, Mapping) ->
    jsxd:fold(fun(Key, Target, Acc) ->
                      perhaps_set(Key, Src, Target, Acc)
              end, Obj, Mapping).

dflt(undefined, Dflt) ->
    Dflt;
dflt(Value, _Dflt) ->
    Value.
