-module(chunter_snap).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         describe_restore/1,
         download/5,
         download_to_port/6,
         upload/4,
         get/1,
         get_all/2,
         restore_path/3,
         mk_s3_conf/1
        ]).

upload(<<_:1/binary, P/binary>>, VM, SnapID, Options) ->
    Disk = case P of
               %% 42 is the lenght of zone/<uuid>
               <<_:42/binary, "-", Dx/binary>> ->
                   <<"-", Dx/binary>>;
               _ ->
                   <<>>
           end,
    Chunk = case application:get_env(chunter, upload_chunk) of
                undefined ->
                    1048576;
                {ok, S} ->
                    S
            end,
    lager:debug("Starting upload wiht chunk size: ~p.", [Chunk]),
    DfltTarget = <<VM/binary, "/", SnapID/binary, Disk/binary>>,
    Target = proplists:get_value(target, Options, DfltTarget),
    AKey = proplists:get_value(access_key, Options),
    SKey = proplists:get_value(secret_key, Options),
    S3Host = proplists:get_value(s3_host, Options),
    S3Port = proplists:get_value(s3_port, Options),
    Bucket = proplists:get_value(s3_bucket, Options),
    {ok, Upload} = fifo_s3_upload:new(AKey, SKey, S3Host, S3Port, Bucket,
                                      Target),
    Cmd = code:priv_dir(chunter) ++ "/zfs_export.gzip.sh",
    Prt = case proplists:get_value(parent, Options) of
              undefined ->
                  run(Cmd, [P, SnapID]);
              Inc ->
                  backup_update(VM, SnapID, <<"parent">>, Inc, Options),
                  run(Cmd, [P, SnapID, Inc])
          end,
    backup_update(VM, SnapID, <<"state">>, <<"uploading">>, Options),
    backup_update(VM, SnapID, [<<"files">>, Target, <<"size">>], 0, Options),
    Ctx = crypto:hash_init(sha),
    upload_to_cloud(VM, SnapID, Prt, Upload, <<>>, Chunk, 0, Ctx, Target,
                    Options).


%% Wish we could match on binary AccIn in the haed sadly erlang doesn't allow
%% that :/
upload_to_cloud(VM, SnapID, Port, Upload, AccIn, Chunk, Size, Ctx, Target,
                Options) when byte_size(AccIn) >= Chunk ->
    <<MB:Chunk/binary, Acc/binary>> = AccIn,
    case fifo_s3_upload:part(Upload, binary:copy(MB)) of
        ok ->
            lager:debug("Uploading: ~p MB.", [round(Size/1024/1024)]),
            update_size(VM, SnapID, Target, Size, Options),
            upload_to_cloud(VM, SnapID, Port, Upload, Acc, Chunk,
                            Size, Ctx, Target, Options);
        {error, E} ->
            fail_upload(VM, SnapID, Upload, Options, E),
            {error, 3, E}
    end;

upload_to_cloud(VM, SnapID, Port, Upload, AccIn, Chunk, Size, Ctx, Target,
                Options) ->
    receive
        {Port, {data, Data}} ->
            Size1 = Size + byte_size(Data),
            Ctx1 = crypto:hash_update(Ctx, Data),
            upload_to_cloud(VM, SnapID, Port, Upload,
                            <<AccIn/binary, Data/binary>>, Chunk, Size1,
                            Ctx1, Target, Options);
        {Port, {exit_status, 0}} ->
            R = case AccIn of
                    <<>> ->
                        ok;
                    _ ->
                        fifo_s3_upload:part(Upload, binary:copy(AccIn))
                end,
            case R of
                ok ->
                    fifo_s3_upload:done(Upload),
                    lager:debug("Upload complete: ~p MB.",
                                [round(Size/1024/1024)]),
                    update_size(VM, SnapID, Target, Size, Options),
                    Digest = base16:encode(crypto:hash_final(Ctx)),
                    backup_update(VM, SnapID,
                                  [<<"files">>, Target, <<"sha1">>],
                                  Digest, Options),
                    M = io_lib:format("Uploaded ~s with a total size of"
                                      " done: ~p", [VM, Size]),

                    {ok, list_to_binary(M), Digest};
                {error, E} ->
                    fail_upload(VM, SnapID, Upload, Options, E),
                    {error, 2, E}
            end;
        {Port, {exit_status, S}} ->
            fail_upload(VM, SnapID, Upload, Options, S),
            {error, S, <<"upload failed">>}
    end.
fail_upload(VM, SnapID, Upload, Options, Error) ->
    fifo_s3_upload:abort(Upload),
    lager:error("Upload error: ~p", [Error]),
    backup_update(VM, SnapID, <<"state">>, <<"failed">>, Options).

update_size(VM, SnapID, Target, Size, Options) ->
    backup_update(VM, SnapID,
                  [<<"files">>, Target, <<"size">>],
                  Size, Options).


download(<<_:1/binary, P/binary>>, VM, SnapID, SHA1, Options) ->
    Disk = case P of
               %% 42 is the lenght of zone/<uuid>
               <<_:42/binary, "-", Dx/binary>> ->
                   <<"-", Dx/binary>>;
               _ ->
                   <<>>
           end,
    AKey = proplists:get_value(access_key, Options),
    SKey = proplists:get_value(secret_key, Options),
    S3Host = proplists:get_value(s3_host, Options),
    S3Port = proplists:get_value(s3_port, Options),
    Bucket = proplists:get_value(s3_bucket, Options),
    Target = <<VM/binary, "/", SnapID/binary, Disk/binary>>,
    Chunk = case application:get_env(chunter, download_chunk) of
                undefined ->
                    1048576;
                {ok, S} ->
                    S
            end,
    lager:debug("Starting download wiht chunk size: ~p.", [Chunk]),
    {ok, Download} = fifo_s3_download:new(AKey, SKey, S3Host, S3Port, Bucket,
                                          Target, [{chunk_size, Chunk}]),
    Cmd = code:priv_dir(chunter) ++ "/zfs_import.gzip.sh",
    Prt = run(Cmd, [P, SnapID]),
    Ctx = crypto:hash_init(sha),
    download_to_port(Prt, Download, undefined, SHA1, Ctx, 0).

download_to_port(Prt, Download, Lock, SHA1, Ctx, I) ->
    case Lock of
        undefined ->
            ok;
        _ ->
            chunter_lock:lock(Lock)
    end,
    case fifo_s3_download:get(Download) of
        {ok, done} ->
            lager:debug("Download complete: ~p.", [I]),
            case base16:encode(crypto:hash_final(Ctx)) of
                Digests when Digests == SHA1 ->
                    port_close(Prt),
                    lager:info("[download] Download valid with ~s", [Digests]),
                    {ok, done};
                Digests when SHA1 == <<>> ->
                    lager:warning("[download] No hash provided so we assume ~s"
                                  " to be correct.", [Digests]),
                    port_close(Prt),
                    {ok, done};
                Digests ->
                    lager:error("[download] Corrupted got ~p but expected ~s",
                                [Digests, SHA1]),
                    port_close(Prt),
                    {error, 0, corrupted}
            end;
        {ok, Data} ->
            Ctx1 = crypto:hash_update(Ctx, Data),
            lager:debug("Download part: ~p.", [I]),
            port_command(Prt, Data),
            download_to_port(Prt, Download, Lock, SHA1, Ctx1, I+1);
        {error, E} ->
            port_close(Prt),
            lager:error("Import error: ~p", [I, E]),
            {error, 0, E}
    end.

describe_restore([{local, U} | R]) ->
    lager:debug("[restore] Using local snapshot ~s", [U]),
    describe_restore(R);
describe_restore([{full, U, SHAs} | R]) ->
    lager:debug("[restore] Using full backup ~s(~p)", [U, SHAs]),
    describe_restore(R);
describe_restore([{incr, U, SHAs} | R]) ->
    lager:debug("[restore] incremental update to ~s(~p)", [U, SHAs]),
    describe_restore(R);
describe_restore([]) ->
    ok.

get(UUID) ->
    Cmd = "/usr/sbin/zfs list -rpH -t snapshot zones/" ++ binary_to_list(UUID),
    Res = os:cmd(Cmd),
    Ls = [L || L <- re:split(Res, "\n"), L =/= <<>>],
    Ls1 = [re:run(L, ".*@(.*?)\t.*", [{capture, [1], binary}]) || L <- Ls],
    [M || {match, [M]} <- Ls1].

%% @doc Sum up the sizes of the original snapshot and the size of disks for
%% KVM machines.
get_all(VM, Spec) ->
    Disks = jsxd:get(<<"disks">>, [], Spec),
    Disks1 = [jsxd:get(<<"path">>, <<"">>, D) || D <- Disks],
    Disks2 = [D || <<_:15/binary, D/binary>> <- Disks1],
    Lines = lists:foldl(
              fun(Disk, LAcc) ->
                      LAcc ++ lines(Disk)
              end,
              lines("zones/" ++ binary_to_list(VM)),
              Disks2),
    Snaps = [{lists:last(re:split(Name, "@")),
              list_to_integer(binary_to_list(Size))}
             || [Name, Size, _, _, _] <- Lines],
    lists:foldl(fun({ID, S}, [{ID, SA} | A]) ->
                        [{ID, SA + S} | A];
                   (S, A) ->
                        [S | A]
                end, [], lists:sort(Snaps)).


lines(Disk) when is_binary(Disk) ->
    lines(binary_to_list(Disk));

lines(Disk) ->
    Cmd = "/usr/sbin/zfs list -r -t snapshot -pH " ++ Disk,
    lager:debug("Getting snapshots: ~s", [Cmd]),
    Data = os:cmd(Cmd),
    [re:split(L, "\t") || L <-re:split(Data, "\n"),
                          L =/= <<>>].


restore_path(Target, Remote, Local) ->
    restore_path(Target, Remote, Local, []).

restore_path(Target, Remote, Local, Path) ->
    case lists:member(Target, Local) of
        true ->
            {ok, [{local, Target} | Path]};
        false ->
            case jsxd:get(Target, Remote) of
                {ok, Snap} ->
                    restore_snap(Target, Remote, Local, Path, Snap);
                _ ->
                    {error, nopath}
            end
    end.

restore_snap(Target, Remote, Local, Path, Snap) ->
    Files = jsxd:get(<<"files">>, [], Snap),
    SHAs = [{ID, jsxd:get(<<"sha1">>, <<>>, V)}
            || {ID, V} <- Files],
    case jsxd:get(<<"parent">>, Snap) of
        {ok, Parent} ->
            restore_path(Parent, Remote, Local,
                         [{incr,  Target, SHAs} | Path]);
        _ ->
            {ok, [{full, Target, SHAs} | Path]}
    end.

mk_s3_conf(Options) ->
    AKey = proplists:get_value(access_key, Options),
    SKey = proplists:get_value(secret_key, Options),
    S3Host = proplists:get_value(s3_host, Options),
    S3Port = proplists:get_value(s3_port, Options),
    fifo_s3:make_config(AKey, SKey, S3Host, S3Port).

%%    backup_update(VM, SnapID,
%%                  [<<"files">>, Target, <<"size">>],
%%                  Size, Options).

backup_update(VM, SnapID, K, V, Opts) when is_list(K) ->
    case proplists:get_value(quiet, Opts, false) of
        false ->
            Event = proplists:get_value(event, Opts, <<"backup">>),
            ls_vm:set_backup(VM, [{[SnapID | K], V}]),
            Data = jsxd:set(K, V, #{}),
            libhowl:send(VM,
                         #{<<"event">> => Event,
                           <<"data">> => #{
                               <<"action">> => <<"update">>,
                               <<"data">> => Data,
                               <<"uuid">> => SnapID}});
        true ->
            ok
    end;

backup_update(VM, SnapID, K, V, Opts) ->
    backup_update(VM, SnapID, [K], V, Opts).


-ifdef(TEST).
restore_path_test() ->
    Local = [<<"b">>, <<"c">>],
    Remote = [
              {<<"a">>, [{<<"parent">>, <<"c">>}]},
              {<<"d">>, [{<<"parent">>, <<"e">>}]},
              {<<"e">>, [{<<"parent">>, <<"f">>}]},
              {<<"f">>, []},
              {<<"g">>, [{<<"parent">>, <<"h">>}]}
             ],
    {ok, ResA} = restore_path(<<"a">>, Remote, Local),
    {ok, ResB} = restore_path(<<"b">>, Remote, Local),
    {ok, ResD} = restore_path(<<"d">>, Remote, Local),
    ResG = restore_path(<<"g">>, Remote, Local),

    ?assertEqual([{local, <<"b">>}], ResB),
    ?assertEqual([{local, <<"c">>}, {incr, <<"a">>, []}], ResA),
    ?assertEqual([{full, <<"f">>, []}, {incr, <<"e">>, []},
                  {incr, <<"d">>, []}], ResD),
    ?assertEqual({error, nopath}, ResG),
    ok.

-endif.

run(Cmd, Args) ->
    lager:debug("Running ZFS command: ~s ~p", [Cmd, Args]),
    open_port({spawn_executable, Cmd},
              [{args, Args}, use_stdio, binary,
               stderr_to_stdout, exit_status, stream]).
