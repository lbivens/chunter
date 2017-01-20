-module(spec_tests).

-include_lib("eunit/include/eunit.hrl").

-import(chunter_spec, [to_vmadm/3, to_sniffle/1]).

type_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"alias">>, <<"a">>}, {<<"hostname">>, <<"h">>}, {<<"uuid">>, <<"z">>}]),
    In = apply_defaults(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

autoboot_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"alias">>, <<"a">>}, {<<"hostname">>, <<"h">>},
                          {<<"uuid">>, <<"z">>}, {<<"autoboot">>, false}]),
    In0 = apply_defaults(InP, InD, InO),
    In = jsxd:set(<<"autoboot">>, false, In0),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

disk_driver_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>},
                          {<<"disk_driver">>, <<"virtio">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

created_at_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In0 = apply_defaults_kvm(InP, InD, InO),
    In = jsxd:set(<<"created_at">>, 123, In0),
    VMData = jsxd:set(<<"created_at">>, 123, to_vmadm(InP, InD, InO)),
    ?assertEqual(In, to_sniffle(VMData)).


nic_driver_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>},
                          {<<"nic_driver">>, <<"virtio">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

zone_ram_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 1024}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults(InP, InD, InO),
    VMData = to_vmadm(InP, InD, InO),
    VMData1 = jsxd:set(<<"max_physical_memory">>, 1024*1024*1024, VMData),
    ?assertEqual(In, to_sniffle(VMData1)).

kvm_ram_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 1024}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

kvm_cpu_cap1_test() ->
    InP = jsxd:from_list([ {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 100}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

kvm_cpu_cap14_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 140}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO, 140, 2),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

kvm_cpu_cap15_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 150}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO, 150, 2),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

kvm_cpu_cap2_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 200}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO, 200, 2),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

kvm_cpu_cap21_test() ->
    InP = jsxd:from_list([{<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}, {<<"cpu_cap">>, 210}]),
    InD = jsxd:from_list([{<<"type">>, <<"kvm">>}, {<<"image_size">>, 10}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}]),
    In = apply_defaults_kvm(InP, InD, InO, 210, 3),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).


resolver_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>}, {<<"resolvers">>, [<<"8.8.8.8">>]}]),
    In0 = apply_defaults(InP, InD, InO),
    In = jsxd:set(<<"resolvers">>, [<<"8.8.8.8">>], In0),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

ssh_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>},
                          {<<"ssh_keys">>,
                           <<"ssh-rsa">>}]),
    In = apply_defaults(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

passwd_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>},
                          {<<"admin_pw">>, <<"admin">>},
                          {<<"root_pw">>, <<"root">>}]),
    In = apply_defaults(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

metadata_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10}, {<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>},
                          {<<"admin_pw">>, <<"admin">>},
                          {<<"metadata">>, [{<<"key">>, <<"value">>}]}]),
    In = apply_defaults(InP, InD, InO),
    ?assertEqual(In, to_sniffle(to_vmadm(InP, InD, InO))).

nics_test() ->
    InP = jsxd:from_list([{<<"cpu_cap">>, 100}, {<<"uuid">>, <<"p">>}, {<<"quota">>, 10},{<<"ram">>, 0}]),
    InD = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InD1 = jsxd:from_list([{<<"type">>, <<"zone">>}, {<<"uuid">>, <<"d">>}]),
    InO = jsxd:from_list([{<<"uuid">>, <<"z">>},
                          {<<"nics">>, [[{<<"ip">>, <<"127.0.0.1">>},
                                         {<<"nic_tag">>, <<"admin">>}]]}]),
    In = apply_defaults(InP, InD1, InO),
    Expected = to_sniffle(to_vmadm(InP, InD, InO)),
    ?assertEqual(In, Expected).

apply_defaults(InP, InD, InO) ->
    Swap = erlang:max(256, jsxd:get(<<"ram">>, 0, InP)*2),
    {ok, Dataset} = jsxd:get(<<"uuid">>, InD),
    InD1 = jsxd:delete(<<"uuid">>, InD),
    jsxd:thread([{merge, InP},
                 {set, <<"autoboot">>, true},
                 {set, <<"max_swap">>, Swap},
                 {delete, <<"name">>},
                 {set, <<"package">>, <<"p">>},
                 {set, <<"resolvers">>, [<<"8.8.8.8">>, <<"8.8.4.4">>]},
                 {set, <<"networks">>, jsxd:get(<<"nics">>, [], InO)},
                 {set, [<<"networks">>, 0, <<"primary">>], true},
                 {delete, <<"nics">>},
                 {merge, InD1},
                 {set, <<"dataset">>, Dataset},
                 {set, <<"cpu_shares">>, 0},
                 {set, <<"uuid">>, <<"z">>},
                 {set, <<"zfs_io_priority">>, 0}],
                InO).

apply_defaults_kvm(InP, InD, InO) ->
    apply_defaults_kvm(InP, InD, InO, 100, 1).

apply_defaults_kvm(InP, InD, InO, Cap, N) ->
    In0 = apply_defaults(InP, InD, InO),
    jsxd:thread([{set, <<"cpu_cap">>, Cap},
                 {delete, <<"image_size">>},
                 {set, <<"disks">>,
                  [#{<<"boot">> => true,
                     <<"image_size">> => 10,
                     <<"image_uuid">> => <<"d">>},
                   #{<<"boot">> => false,
                     <<"size">> => 10240}]},
                 {set, <<"vcpus">>, N}],
                In0).
