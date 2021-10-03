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
                   [ "chroots "] in
  Arg.(value (opt string !LurchServerChroot.chroot_prefix i))

let busybox =
  let env = Term.env_info "BUSYBOX" in
  let i = Arg.info ~env ~doc:"Where to find statically linked busybox."
                   ~docv:"PATH" [ "busybox"] in
  Arg.(value (opt string !LurchServerChroot.busybox i))

let conninfo =
  let env = Term.env_info "LURCH_DB" in
  let doc = "Conninfo string to join Postgresql database." in
  let i = Arg.info ~env ~doc [ "db" ] in
  let def = "host=localhost port=5433 dbname=lurch password=secret user=postgres" in
  Arg.(value (opt string def i))

let run_id =
  let i = Arg.info ~doc:"Run number." [] in
  Arg.(required (pos 0 (some int) None i))

let program =
  let i = Arg.info ~doc:"Program name." [] in
  Arg.(required (pos 0 (some string) None i))

let cgi dbg conninfo () =
  Printexc.record_backtrace true ;
  init_log dbg false ;
  Db.init conninfo ;
  Httpd.serve () ;
  Db.close ()

let start dbg conninfo program_name () =
  init_log dbg true ;
  Db.init conninfo ;
  let program = Db.Program.get program_name in
  let run_id = Db.Run.insert program.Api.Program.command.id in
  log.info "Program %s started as run #%d." program_name run_id ;
  Db.close ()

let step dbg conninfo chroot_prefix busybox () =
  LurchServerChroot.chroot_prefix := chroot_prefix ;
  LurchServerChroot.busybox := busybox ;
  init_log dbg true ;
  Db.init conninfo ;
  Command.step () ;
  Db.close ()

let exec dbg conninfo run_id chroot_prefix busybox () =
  LurchServerChroot.chroot_prefix := chroot_prefix ;
  LurchServerChroot.busybox := busybox ;
  init_log ~with_time:false dbg true ;
  Db.init conninfo ;
  log.debug "Executing run#%d" run_id ;
  Command.exec run_id ;
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
    (const cgi $ dbg $ conninfo),
    info ~doc:"Answer an http query." "cgi")

(* Start a program, handy in cron tabs *)
let start =
  Term.(
    (const start $ dbg $ conninfo $ program),
    info ~doc:"Start a program." "start")

(* Perform a single step of any pending run: *)
let step =
  Term.(
    (const step $ dbg $ conninfo $ chroot_prefix $ busybox),
    info ~doc:"Perform the next step of execution." "step")

(* Allows to execute internal commands as a separate process: *)
let exec =
  Term.(
    (const exec $ dbg $ conninfo $ run_id $ chroot_prefix $ busybox),
    info ~doc:"execute given run id and quit. \
               Not supposed to be called directly." 
         "_exec")

let () =
  match Term.eval_choice default [ cgi ; start ; step ; exec ] with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok f ->
      f () ;
      exit 0
