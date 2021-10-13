(* Simple HTTP service *)
open Batteries

open LurchServerLib
module Api = LurchApiTypes
module Db = LurchServerDb

let get_opt_param params name parse =
  List.assoc_opt name params |> Option.map parse

exception Missing_parameter of string
exception No_such_resource

let get_param params name parse =
  try List.assoc name params |> parse
  with Not_found -> raise (Missing_parameter name)

let list_past_runs program oldest_top_run limit =
  Db.ListPastRuns.get ?program ?oldest_top_run ?limit () |>
  Array.of_enum |>
  Api.ListPastRuns.array_to_json_buffer |>
  print_data

let list_programs () =
  Db.ListPrograms.get () |>
  Array.of_enum |>
  Api.ListPrograms.array_to_json_buffer |>
  print_data

let get_program name =
  Db.Program.get name |>
  Api.Program.to_json_buffer |>
  print_data

let del_program name =
  Db.Program.delete name

(* Programs submitted via the GUI must not execute any non-isolated binary: *)
let rec check_isolation cmd =
  match cmd.Api.Command.operation with
  | Api.Command.Nop _ | Isolate _ | Spawn _ | Break _ | Approve _ | Pause _
  | Wait _ ->
      ()
  | Chroot _ | Docker _ ->
      (* Because we do not recurse into Isolate *)
      assert false
  | Exec _ ->
      failwith "Programs are not allowed to execute any binary unless isolated"
  | Let { subcommand }
  | Retry { subcommand }
  | ForLoop { subcommand } ->
      check_isolation subcommand
  | Sequence { subcommands } ->
      List.iter check_isolation subcommands
  | If { condition ; consequent ; alternative } ->
      check_isolation condition ;
      check_isolation consequent ;
      check_isolation alternative

let save_program prev_name program =
  let program = Api.Program.of_json_string program in
  if program.name = "" then failwith "Programs must have a name" ;
  check_isolation program.command ;
  (match prev_name with
  | None
  | Some "" ->
      Db.Program.insert program
  | Some prev_name ->
      Db.Program.update program prev_name) ;
  get_program program.name

let get_logs ?offset ?limit ~run =
  Db.ListLogLines.get ?offset ?limit ~run |>
  Array.of_enum |>
  Api.LogLine.array_to_json_buffer |>
  print_data

let get_run run_id =
  Db.Run.get run_id |>
  Api.Run.to_json_buffer |>
  print_data

let cancel_run run_id =
  Db.Run.cancel run_id

(* Record the run in the DB but does not start any process just yet.
 * Leave this to `lurch step`. *)
let start_program name creator_user =
  let program = Db.Program.get name in
  Db.Run.insert ~creator_user program.Api.Program.command.id |>
  get_run

let approve run_id msg =
  Db.Approval.insert run_id msg

let set_variable run_id value =
  Db.LetValue.insert run_id value

let serve () =
  let debug_to_stderr = true in
  let debug_to_file = Some (!log_dir ^"/httpd.log") in
  let params = Cgi.parse_args () in
  let get_param n = get_param params n
  and get_opt_param n = get_opt_param params n in
  log.debug "PATH_INFO = %a" (List.print String.print) Cgi.path_info ;
  log.debug "URL = %s" (Cgi.this_url ()) ;
  (try
    (match get_param "p" identity with
    | "list_past_runs" ->
        let program = get_opt_param "program" identity
        and oldest_top_run = get_opt_param "oldest_top_run" int_of_string
        and limit = get_opt_param "limit" int_of_string in
        list_past_runs program oldest_top_run limit
    | "list_programs" ->
        list_programs ()
    | "get_program" ->
        let name = get_param "program" identity in
        get_program name
    | "save_program" ->
        let program = Cgi.read_body () in
        let prev_name = get_opt_param "prev_name" identity in
        save_program prev_name program
    | "del_program" ->
        let name = get_param "program" identity in
        del_program name
    | "get_logs" ->
        (* We might ask for all the logs for this top_run, or all the logs for
         * a particular run, both with the run parameter. *)
        let run = get_param "run" int_of_string
        and offset = get_opt_param "offset" int_of_string
        and limit = get_opt_param "limit" int_of_string in
        get_logs ?offset ?limit ~run
    | "get_run" ->
        let run_id = get_param "id" int_of_string in
        get_run run_id
    | "cancel_run" ->
        let run_id = get_param "id" int_of_string in
        cancel_run run_id
    | "start_program" ->
        let name = get_param "program" identity in
        let creator_user =
          try Sys.getenv "REMOTE_USER"
          with Not_found ->
            failwith "Unidentified users are not allowed to start programs" in
        start_program name creator_user
    | "approve" ->
        let run_id = get_param "run" int_of_string
        and msg = get_param "message" identity in
        approve run_id msg
    | "set_variable" ->
        let run_id = get_param "run" int_of_string
        and value = get_param "value" identity in
        set_variable run_id value
    | p ->
        raise No_such_resource) ;
    Cgi.header ~status:200 () ;
    Printf.printf "%s" (IO.close_out output)
  with
    | No_such_resource ->
        let err_msg = "No such resource" in
        Cgi.header ~status:404 ~err_msg () ;
        print_string "You rang?"
    | Missing_parameter p ->
        let err_msg = "Missing parameter" in
        Cgi.header ~status:400 ~err_msg () ;
        print_string ("Missing parameter "^ p)
    | e ->
        let err_msg = "Internal error" in
        Cgi.header ~err_msg ~status:500 () ;
        Printf.printf "%s\n%s"
          (Printexc.to_string e)
          (Printexc.get_backtrace ())) ;
  ignore_exceptions (fun () ->
    (* Note: It's OK to call IO.close_out several times: *)
    let msg = IO.close_out log_text in
    let len = String.length msg in
    if debug_to_stderr then
      Printf.eprintf "%s\n" msg ;
    match debug_to_file with
    | None -> ()
    | Some fname ->
        let open Unix in
        let fd = openfile fname [O_WRONLY; O_APPEND; O_CREAT; O_CLOEXEC] 0o644 in
        single_write_substring fd (msg ^ "\n") 0 (len + 1) |> ignore ;
        close fd) ()
