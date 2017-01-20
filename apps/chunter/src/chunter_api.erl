-module(chunter_api).

-export([call/2]).

-define(MAX_MDATA_SIZE, 1024*1024*32).

call(UUID, R = #{<<"action">> := <<"zfs-", _/binary>>}) ->
    check_call(snapshot_api, UUID, R);

call(UUID, R = #{<<"action">> := <<"snapshot-", _/binary>>}) ->
    check_call(snapshot_api, UUID, R);

call(UUID, R = #{<<"action">> := <<"backup-", _/binary>>}) ->
    check_call(backup_api, UUID, R);

call(UUID, R = #{<<"action">> := <<"metadata-", _/binary>>}) ->
    check_call(metadata_api, UUID, R);

call(UUID, R = #{<<"action">> := <<"cluster-", _/binary>>}) ->
    check_call(grouping_api, UUID, R);

call(UUID, R = #{<<"action">> := <<"stack-", _/binary>>}) ->
    check_call(grouping_api, UUID, R);

call(UUID, R = #{<<"action">> := <<"remote-", _/binary>>}) ->
    check_call(remote_api, UUID, R).

check_call(Type, UUID, R) ->
    case enabled(Type) of
        true ->
            call_(UUID, R);
        false ->
            {error, "disabled"}
    end.

call_(UUID, #{<<"action">> := <<"zfs-list">>})->
    case chunter_zfs:list(<<"zones/", UUID/binary>>) of
        {ok, Data} ->
            {ok, #{reply => Data}};
        E ->
            lager:warning("[api] zfs-list(~s) failed: ~p", [UUID, E]),
            {error, "failed!"}
    end;

call_(_UUID, #{<<"action">> := <<"remote-stop">>})->
    {error, "not implemented!"};

call_(_UUID, #{<<"action">> := <<"remote-start">>})->
    {error, "not implemented!"};

call_(UUID, #{<<"action">> := <<"snapshot-create">>,
              <<"comment">> := Comment}) ->
    case ls_vm:snapshot(UUID, Comment) of
        {ok, SUUID} ->
            {ok, #{uuid => SUUID}};
        _ ->
            {error, "failed!"}
    end;

call_(UUID, #{<<"action">> := <<"snapshot-list">>}) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            {ok, ft_vm:snapshots(V)};
        _ ->
            {error, "failed!"}
    end;

call_(UUID, #{<<"action">> := <<"snapshot-get">>,
              <<"uuid">> := SnapID}) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            Snaps = ft_vm:snapshots(V),
            case jsxd:get(SnapID, Snaps) of
                {ok, S} ->
                    {ok, S};
                _ ->
                    {error, "not found"}
            end;
        _ ->
            {error, "failed!"}
    end;

call_(UUID, #{<<"action">> := <<"metadata-get">>}) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            {ok, ft_vm:metadata(V)};
        _ ->
            {error, "failed!"}
    end;

call_(UUID, #{<<"action">> := <<"metadata-set">>,
              <<"data">> := D}) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            Size = byte_size(term_to_binary(ft_vm:metadata(V))) +
                byte_size(term_to_binary(D)),
            case Size > ?MAX_MDATA_SIZE of
                true ->
                    {error, "out of space"};
                false ->
                    ls_vm:set_metadata(UUID, D),
                    {ok, V1} = ls_vm:get(UUID),
                    {ok, ft_vm:metadata(V1)}
            end;
        _ ->
            {error, "failed!"}
    end;

call_(UUID, #{<<"action">> := <<"cluster-get">>}) ->
    case grouping(UUID) of
        {ok, _, G} ->
            {ok, ft_grouping:config(G)};
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"cluster-vms">>}) ->
    case grouping(UUID) of
        {ok, _, G} ->
            {ok, ft_grouping:elements(G)};
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"cluster-set">>,
              <<"data">> := D}) ->
    case grouping(UUID) of
        {ok, GID, G} ->
            set_grouping_config(GID, G, D);
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"stack-get">>}) ->
    case stack(UUID) of
        {ok, _, S} ->
            {ok, ft_grouping:config(S)};
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"stack-vms">>}) ->
    case stack(UUID) of
        {ok, _, G} ->
            {ok, stack_vms(G)};
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"stack-power">>,
              <<"state">> := Action,
              <<"vm">> := VM}) ->
    case stack(UUID) of
        {ok, _, G} ->
            stack_power(VM, Action, G);
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"stack-execute">>,
              <<"command">> := Command,
              <<"vm">> := VM}) ->
    case stack(UUID) of
        {ok, _, G} ->
            stack_execute(VM, Command, G);
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"stack-set">>,
              <<"data">> := D}) ->
    case stack(UUID) of
        {ok, GID, G} ->
            set_grouping_config(GID, G, D);
        E ->
            E
    end;

call_(UUID, #{<<"action">> := <<"backup-list">>}) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            {ok, ft_vm:backups(V)};
        _ ->
            {error, "failed!"}
    end;

call_(UUID, #{<<"action">> := <<"backup-create">>,
              <<"comment">> := Comment,
              <<"delete">> := Delete}) ->
    Opts = case Delete of
               true ->
                   [delete, xml];
               _ ->
                   [xml]
           end,
    case ls_vm:full_backup(UUID, Comment, Opts) of
        {ok, BackupID} ->
            {ok, BackupID};
        _ ->
            {error, "failed!"}
    end;

call_(UUID, #{<<"action">> := <<"backup-create">>,
              <<"comment">> := Comment,
              <<"delete">> := Delete,
              <<"parent">> := Parent}) ->
    Opts = case Delete of
               true ->
                   [delete, xml];
               <<"parent">> ->
                   [{delete, parent}, xml];
               <<"both">> ->
                   [{delete, parent}, delete, xml];
               _ ->
                   [xml]
           end,
    case ls_vm:incremental_backup(UUID, Parent, Comment, Opts) of
        {ok, BackupID} ->
            {ok, BackupID};
        _ ->
            {error, "failed!"}
    end;

call_(_, Cmd) ->
    lager:warning("[api] Unsupported command: ~p", [Cmd]),
    {error, "unsupported"}.

grouping_id(UUID) ->
    case ls_vm:get(UUID) of
        {ok, V} ->
            case ft_vm:groupings(V) of
                [GID] ->
                    {ok, GID};
                _ ->
                    {error, "not in a cluster"}
            end;
        _ ->
            {error, "failed!"}
    end.

grouping(UUID) ->
    case grouping_id(UUID) of
        {ok, GID} ->
            case ls_grouping:get(GID) of
                {ok, G} ->
                    {ok, GID, G};
                _ ->
                    {error, "cluster not found"}
            end;
        E ->
            E
    end.

stack_id(UUID) ->
    case grouping(UUID) of
        {ok, _, G} ->
            case ft_grouping:groupings(G) of
                [SID | _]  ->
                    {ok, SID};
                _ ->
                    {error, "stack not found"}
            end;
        _ ->
            {error, "stack not found"}
    end.

stack(UUID) ->
    case stack_id(UUID) of
        {ok, SID} ->
            case ls_grouping:get(SID) of
                {ok, S} ->
                    {ok, SID, S};
                _ ->
                    {error, "stack not found"}
            end;
        _ ->
            {error, "stack not found"}
    end.

enabled(Action) ->
    application:get_env(chunter, Action, true).

stack_vms(Stack) ->
    Clusters = ft_grouping:elements(Stack),
    maps:from_list([{UUID, grouping_elements(UUID)}
                    || UUID <- Clusters]).

stack_vms_flat(Stack) ->
    Res = [VMs || {_, VMs} <- maps:to_list(stack_vms(Stack))],
    lists:usort(lists:flatten(Res)).

grouping_elements(UUID) ->
    {ok, G} = ls_grouping:get(UUID),
    ft_grouping:elements(G).


set_grouping_config(GID, G, D) ->
    Size = byte_size(term_to_binary(ft_grouping:config(G))) +
        byte_size(term_to_binary(D)),
    case Size > ?MAX_MDATA_SIZE of
        true ->
            {error, "out of space"};
        false ->
            ls_grouping:set_config(GID, D),
            {ok, G1} = ls_grouping:get(GID),
            {ok, ft_grouping:config(G1)}
    end.


stack_power(VM, Action, Stack) ->
    case lists:member(VM, stack_vms_flat(Stack)) of
        true ->
            stack_power(VM, Action);
        false ->
            {error, "VM not part of the stack"}
    end.

stack_execute(VM, Command, Stack) ->
    case lists:member(VM, stack_vms_flat(Stack)) of
        true ->
            stack_execute(VM, Command);
        false ->
            {error, "VM not part of the stack"}
    end.

stack_execute(VM, Command) ->
    case ls_vm:get(VM) of
        {ok, V} ->
            Hv = ft_vm:hypervisor(V),
            {ok, H} = ls_hypervisor:get(Hv),
            {Host, Port} = ft_hypervisor:endpoint(H),
            stack_execute(Host, Port, VM, Command);
        E1 ->
            E1
    end.

stack_execute(Host, Port, VM, Command) ->
    case libchunter:execute(Host, Port, VM, Command, <<>>, fun fold_reply/2) of
        {ok, ExitCode, Reply} ->
            {ok, #{<<"exit_code">> => ExitCode,
                   <<"output">> => Reply}};
        E ->
            E
    end.

fold_reply(Acc, {exit_status, E}) ->
    {ok, E, Acc};
fold_reply(Acc, {data, B}) ->
    <<Acc/binary, B/binary>>;
fold_reply(Acc, _) ->
    Acc.
stack_power(VM, <<"start">>) ->
    ls_vm:start(VM),
    {ok, #{<<"reply">> => <<"starting">>}};
stack_power(VM, <<"stop">>) ->
    ls_vm:stop(VM),
    {ok, #{<<"reply">> => <<"stopping">>}};
stack_power(VM, <<"force-stop">>) ->
    ls_vm:stop(VM, [force]),
    {ok, #{<<"reply">> => <<"stopping">>}};
stack_power(VM, <<"reboot">>) ->
    ls_vm:reboot(VM),
    {ok, #{<<"reply">> => <<"rebooting">>}};
stack_power(VM, <<"force-reboot">>) ->
    ls_vm:reboot(VM, [force]),
    {ok, #{<<"reply">> => <<"rebooting">>}}.
