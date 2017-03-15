-module(chunter_zfs).

-export([
         snapshot/2,
         snapshot/3,
         destroy_snapshot/2,
         destroy_snapshot/3,
         destroy/1,
         destroy/2,
         rollback/2,
         rollback/3,
         list/1
        ]).

-ignore_xref([
         snapshot/2,
         snapshot/3,
         destroy_snapshot/2,
         destroy_snapshot/3,
         destroy/1,
         destroy/2,
         rollback/2,
         rollback/3,
         list/1
        ]).

destroy(Path) ->
    destroy(Path, []).
destroy(Path, Args) ->
    zfs("destroy", build_opts(Args, fun destroy_opt_to_char/1), Path).

destroy_snapshot(Path, SnapID) ->
    destroy_snapshot(Path, SnapID, []).
destroy_snapshot(Path, SnapID, Args) ->
    P = <<Path/binary, "@", SnapID/binary>>,
    destroy(P, Args).

list(Path) ->
    zfs("list", [r, {t, "all"}], Path).

snapshot(Path, SnapID) ->
    snapshot(Path, SnapID, []).

snapshot(Path, SnapID, Args) ->
    P = <<Path/binary, "@", SnapID/binary>>,
    zfs("snapshot", build_opts(Args, fun snapshot_opt_to_char/1), P).

rollback(Path, SnapID) ->
    rollback(Path, SnapID, []).

rollback(Path, SnapID, Args) ->
    P = <<Path/binary, "@", SnapID/binary>>,
    zfs("rollback", build_opts(Args, fun rollback_opt_to_char/1), P).

rollback_opt_to_char(force) -> f;
rollback_opt_to_char(f) -> f;
rollback_opt_to_char(recurseive) -> r;
rollback_opt_to_char(r) -> r;
rollback_opt_to_char(recurseive_clones) -> 'R';
rollback_opt_to_char('R') -> 'R'.

snapshot_opt_to_char(recurseive) -> r;
snapshot_opt_to_char(r) -> r.

destroy_opt_to_char(defer) -> d;
destroy_opt_to_char(d) -> d;
destroy_opt_to_char(force) -> f;
destroy_opt_to_char(f) -> f;
destroy_opt_to_char(recurseive) -> r;
destroy_opt_to_char(r) -> r;
destroy_opt_to_char(recurseive_dependants) -> 'R';
destroy_opt_to_char('R') -> 'R';
destroy_opt_to_char(dry_run) -> n;
destroy_opt_to_char(n) -> n;
destroy_opt_to_char(C) -> opt_to_char(C).




zfs(Cmd, [], Target) ->
    zfs([Cmd, Target]);
zfs(Cmd, Args, Target) ->
    zfs([Cmd | Args] ++ [Target]).

zfs(Args) ->
    ZFS = case chunter_utils:system() of
              S when S =:= omnios; S =:= solaris; s =:= smartos ->
                  "/usr/sbin/zfs";
              freebsd ->
                  "/sbin/zfs"
          end,
    lager:debug("ZFS: ~s ~p", [ZFS, Args]),
    fifo_cmd:run(ZFS, Args).

build_opts([], _) ->
    [];
build_opts(Opts, F) ->
    [F(O) || O <- Opts].

opt_to_char(p) -> p;
opt_to_char(parsable) -> p;
opt_to_char(v) -> v;
opt_to_char(verbose) -> v.


