(* Simple HTPP server that servers spa.js, and convert any query into a
 * postgresql query and return the result. Yes of course this is secure.
 * Here is only the cmd line processing. *)
open Cmdliner

open LurchServerLib

module Api = LurchApiTypes
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
  Arg.(value (opt int !LurchServerCommand.max_spawn_per_min i))

let loop =
  let doc = "Keep executing the programs forever." in
  let i = Arg.info ~doc [ "loop" ] in
  Arg.(value (flag i))

let cgi dbg conninfo log_dir () =
  LurchServerLib.log_dir := log_dir ;
  Printexc.record_backtrace true ;
  init_log dbg false ;
  Db.init conninfo ;
  Httpd.serve () ;
  Db.close ()

let start dbg conninfo program_name () =
  init_log dbg true ;
  let creator_user =
    let pw = Unix.(getpwuid (getuid ())) in
    pw.Unix.pw_name in
  Db.init conninfo ;
  let program = Db.Program.get program_name in
  let run_id = Db.Run.insert ~creator_user program.Api.Program.command.id in
  log.info "Program %s started as run #%d." program_name run_id ;
  Db.close ()

let step dbg conninfo chroot_prefix busybox log_dir spawn_rate_limit loop () =
  LurchServerChroot.chroot_prefix := chroot_prefix ;
  LurchServerChroot.busybox := busybox ;
  LurchServerCommand.max_spawn_per_min := spawn_rate_limit ;
  init_log dbg true ;
  Db.init conninfo ;
  Command.step loop ;
  Db.close ()

let exec dbg conninfo run_id chroot_prefix busybox log_dir () =
  LurchServerChroot.chroot_prefix := chroot_prefix ;
  LurchServerChroot.busybox := busybox ;
  LurchServerLib.log_dir := log_dir ;
  init_log ~with_time:false dbg true ;
  Db.init conninfo ;
  log.debug "Executing run#%d" run_id ;
  Command.exec run_id ;
  Db.close ()

let export dbg conninfo program_name () =
  init_log ~with_time:false dbg true ;
  Db.init conninfo ;
  let program = Db.Program.get program_name in
  print_string (Lang.string_of_command program.Api.Program.command) ;
  print_newline () ;
  Db.close ()

let import dbg conninfo program_name () =
  init_log ~with_time:false dbg true ;
  let command = BatIO.(read_all stdin) |> Lang.command_of_string in
  let program = Api.Program.{ name = program_name ; command ; created = 0. } in
  Db.init conninfo ;
  Db.Program.insert program ;
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
      $ loop),
    info ~doc:"Perform the next step(s) of execution." "step")

(* Allows to execute internal commands as a separate process: *)
let exec =
  Term.(
    (const exec $ dbg $ conninfo $ run_id $ chroot_prefix $ busybox $ log_dir),
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

let import =
  Term.(
    (const import $ dbg $ conninfo $ program),
    info ~doc:"Read a program from stdin as an s-expression."
         "import")

let () =
  match Term.eval_choice default
          [ cgi ; start ; step ; exec ; export ; import ] with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok f ->
      f () ;
      exit 0
