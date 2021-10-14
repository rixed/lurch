(* Simple HTPP server that servers spa.js, and convert any query into a
 * postgresql query and return the result. Yes of course this is secure.
 * Here is only the cmd line processing. *)
open Batteries
open Cmdliner

open LurchServerLib

module Api = LurchApiTypes
module CGroup = LurchServerCGroup
module Command = LurchServerCommand
module Db = LurchServerDb
module Httpd = LurchServerHttpd
module Lang = LurchCommandLanguage
module Version = LurchVersion

let () =
  Printexc.register_printer (function
    | Lang.Invalid_expression s ->
        Some ("Invalid S-Expression: "^ Lang.string_of_sexpr s)
    | Lang.Extraneous_expressions i ->
        Some ("Extraneous expressions at offset "^ string_of_int i)
    | _ ->
        None)

let dbg =
  let env = Term.env_info "LURCH_DEBUG" in
  Arg.(value (flag (info ~env ~doc:"increase verbosity." [ "d" ; "debug" ])))

let chroot_prefix =
  let env = Term.env_info "LURCH_CHROOTS" in
  let i = Arg.info ~env ~doc:"Where to create chroots." ~docv:"PATH"
                   [ "chroots" ] in
  Arg.(value (opt string !LurchServerChroot.chroot_prefix i))

let busybox =
  let env = Term.env_info "BUSYBOX" in
  let i = Arg.info ~env ~doc:"Where to find statically linked busybox."
                   ~docv:"PATH" [ "busybox" ] in
  Arg.(value (opt string !LurchServerChroot.busybox i))

let log_dir =
  let env = Term.env_info "LURCH_LOG_DIR" in
  let i = Arg.info ~env ~doc:"Where to create log files." ~docv:"PATH"
                   [ "log-dir" ] in
  Arg.(value (opt string !LurchServerLib.log_dir i))

let conninfo =
  let env = Term.env_info "LURCH_DB" in
  let doc = "Conninfo string to Postgresql database." in
  let i = Arg.info ~env ~doc [ "db" ] in
  let def = "host=localhost port=5433 dbname=lurch password=secret user=postgres" in
  Arg.(value (opt string def i))

let run_id =
  let i = Arg.info ~doc:"Run number." [] in
  Arg.(required (pos 0 (some int) None i))

let program =
  let i = Arg.info ~doc:"Program name." [] in
  Arg.(required (pos 0 (some string) None i))

let spawn_rate_limit =
  let env = Term.env_info "LURCH_SPAWN_RATE_LIMIT" in
  let doc = "Max number of allowed spawns per minutes." in
  let i = Arg.info ~env ~doc [ "max-spawn-per-min" ; "spawn-rate-limit" ] in
  Arg.(value (opt int !Command.max_spawn_per_min i))

let loop =
  let doc = "Keep executing the programs forever." in
  let i = Arg.info ~doc [ "loop" ] in
  Arg.(value (flag i))

let cgroup_version =
  let env = Term.env_info "CGROUP_VERSION" in
  let doc = "What version of cgroup to use (1 or 2)." in
  let i = Arg.info ~env ~doc [ "cgroup-version" ] in
  Arg.(value (opt int !CGroup.version i))

let cgroup_mount_point =
  let env = Term.env_info "CGROUP_MOUNT_POINT" in
  let doc = "Where is cgroup/cgroup2 fs mounted." in
  let i = Arg.info ~env ~doc [ "cgroup-mount-point" ] in
  Arg.(value (opt string !CGroup.mount_point i))

let cgi dbg conninfo log_dir () =
  LurchServerLib.log_dir := log_dir ;
  Db.conninfo := conninfo ;
  Printexc.record_backtrace true ;
  init_log dbg false ;
  Httpd.serve () ;
  Db.close ()

let start dbg conninfo program_name () =
  Db.conninfo := conninfo ;
  init_log dbg true ;
  let creator_user = current_unix_user () in
  let program = Db.Program.get program_name in
  let run_id = Db.Run.insert ~creator_user program.Api.Program.command.id in
  log.info "Program %s started as run #%d." program_name run_id ;
  Db.close ()

let step dbg conninfo chroot_prefix busybox log_dir spawn_rate_limit loop
         cgroup_version cgroup_mount_point () =
  LurchServerChroot.chroot_prefix := chroot_prefix ;
  LurchServerChroot.busybox := busybox ;
  CGroup.version := cgroup_version ;
  CGroup.mount_point := cgroup_mount_point ;
  Command.max_spawn_per_min := spawn_rate_limit ;
  Db.conninfo := conninfo ;
  init_log dbg true ;
  Command.step loop ;
  Db.close ()

let exec dbg conninfo run_id chroot_prefix busybox log_dir cgroup_version
         cgroup_mount_point () =
  LurchServerChroot.chroot_prefix := chroot_prefix ;
  LurchServerChroot.busybox := busybox ;
  LurchServerLib.log_dir := log_dir ;
  CGroup.version := cgroup_version ;
  CGroup.mount_point := cgroup_mount_point ;
  Db.conninfo := conninfo ;
  init_log ~with_time:false dbg true ;
  log.debug "Executing run#%d" run_id ;
  Command.exec run_id ;
  Db.close ()

let export dbg conninfo program_name () =
  Db.conninfo := conninfo ;
  init_log ~with_time:false dbg true ;
  let program = Db.Program.get program_name in
  print_string (Lang.string_of_command program.Api.Program.command) ;
  print_newline () ;
  Db.close ()

let import dbg conninfo program_name overwrite () =
  Db.conninfo := conninfo ;
  init_log ~with_time:false dbg true ;
  let command = IO.(read_all stdin) |> Lang.command_of_string in
  let program = Api.Program.{ name = program_name ; command ; created = 0. } in
  Db.Program.insert ~overwrite program ;
  Db.close ()

let programs dbg conninfo () =
  Db.conninfo := conninfo ;
  init_log ~with_time:false dbg true ;
  Printf.printf "# name\tlast run\tlast start\tlast stop\tlast exit code\n" ;
  let or_null conv = function
    | None -> "n.a."
    | Some v -> conv v in
  Db.ListPrograms.get () |>
  Enum.iter (fun p ->
    Printf.printf "%s\t%s\t%s\t%s\t%s\n"
      p.Api.ListPrograms.name
      (or_null string_of_int p.last_run)
      (or_null string_of_date p.last_start)
      (or_null string_of_date p.last_stop)
      (or_null string_of_int p.last_exit_code)) ;
  Db.close ()

let runs dbg conninfo program limit () =
  Db.conninfo := conninfo ;
  init_log ~with_time:false dbg true ;
  Printf.printf "# run\tcreated\tstarted\tstopped\texit code\t\
                 cpu usr\tsys\tram usr\tsys\n" ;
  let or_null conv = function
    | None -> "n.a."
    | Some v -> conv v in
  Db.ListPastRuns.get ~program ~limit () |>
  Enum.iter (fun r ->
    let open Api in
    let stats = RunStats.aggr r.ListPastRuns.stats_self r.stats_desc in
    Printf.printf "%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
      r.top_run
      (string_of_date r.created)
      (or_null string_of_date r.started)
      (or_null string_of_date r.stopped)
      (or_null string_of_int r.exit_code)
      (or_null string_of_secs stats.cpu_usr)
      (or_null string_of_secs stats.cpu_sys)
      (or_null string_of_mem stats.mem_usr)
      (or_null string_of_mem stats.mem_sys)) ;
  Db.close ()

let logs dbg conninfo run follow () =
  Db.conninfo := conninfo ;
  init_log ~with_time:false dbg true ;
  Printf.printf "# run\ttime\n" ;  (* thus the initial prev_fd is 1 *)
  let rec loop offset =
    let count, _ =
      Db.ListLogLines.get ~offset ?limit:None ~run |>
      Enum.fold (fun (count, prev_fd) l ->
        if prev_fd <> l.Api.LogLine.fd then IO.flush_all () ;
        Printf.(if l.fd = 1 then printf else eprintf) "%d\t%s\t%s\n"
          l.run
          (string_of_date l.time)
          l.line ;
        count + 1, l.fd
      ) (0, 1) in
    if follow then (
      IO.flush_all () ;
      Unix.sleepf 0.2 ;
      loop (offset + count)
    ) in
  loop 0 ;
  Db.close ()

let default =
  let sdocs = Manpage.s_common_options in
  let doc = "You rang?" in
  let version = Version.release_tag in
  Term.((ret (const (`Help (`Pager, None)))),
        info "lurch" ~version ~doc ~sdocs)

(* The CGI handler *)
let cgi =
  Term.(
    (const cgi $ dbg $ conninfo $ log_dir),
    info ~doc:"Answer an http query." "cgi")

(* Start a program, handy in cron tabs *)
let start =
  Term.(
    (const start $ dbg $ conninfo $ program),
    info ~doc:"Start a program." "start")

(* Perform a single step of any pending run: *)
let step =
  Term.(
    (const step
      $ dbg $ conninfo $ chroot_prefix $ busybox $ log_dir $ spawn_rate_limit
      $ loop $ cgroup_version $ cgroup_mount_point),
    info ~doc:"Perform the next step(s) of execution." "step")

(* Allows to execute internal commands as a separate process: *)
let exec =
  Term.(
    (const exec
      $ dbg $ conninfo $ run_id $ chroot_prefix $ busybox $ log_dir
      $ cgroup_version $ cgroup_mount_point),
    info ~doc:"Execute given run id and quit. \
               Not supposed to be called directly."
         "_exec")

(* Import/Export of programs makes it easier to use revision control tools
 * on them: *)

let export =
  Term.(
    (const export $ dbg $ conninfo $ program),
    info ~doc:"Dump a program as an s-expression that can be later imported."
         "export")

let overwrite =
  let i = Arg.info ~doc:"Overwrite preexisting program with the same name, \
                         if any." [ "o" ; "overwrite" ] in
  Arg.(value (flag i))

let import =
  Term.(
    (const import $ dbg $ conninfo $ program $ overwrite),
    info ~doc:"Read a program from stdin as an s-expression."
         "import")

(* Displaying internal states from the command line: *)

let programs =
  Term.(
    (const programs $ dbg $ conninfo),
    info ~doc:"List all defined programs." "programs")

let num_runs =
  let i = Arg.info ~doc:"Number of runs to display." [ "c" ; "count" ] in
  Arg.(value (opt int 16 i))

let runs =
  Term.(
    (const runs $ dbg $ conninfo $ program $ num_runs),
    info ~doc:"List past runs." "runs")

let run_id =
  let i = Arg.info ~doc:"The run number whose logs to print." [] in
  Arg.(required (pos 0 (some int) None i))

let follow =
  let i = Arg.info ~doc:"Wait for new logs to display." [ "f" ] in
  Arg.(value (flag i))

let logs =
  Term.(
    (const logs $ dbg $ conninfo $ run_id $ follow),
    info ~doc:"Display the logs of a given run." "logs")

let () =
  match Term.eval_choice default
          [ cgi ; start ; step ; exec ; export ; import ; programs ; runs ;
            logs ] with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok f ->
      f () ;
      exit 0
