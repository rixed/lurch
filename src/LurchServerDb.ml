open Batteries
open Postgresql

open LurchServerLib
module Api = LurchApiTypes
module Lang = LurchCommandLanguage

let () =
  Printexc.register_printer (function
    | Error e ->
        Some (string_of_error e)
    | _ ->
        None)

let cnx = ref None
let conninfo = ref ""

let init conninfo_ =
  log.debug "Connecting to DB using: %s" conninfo_ ;
  conninfo := conninfo_

let connect () =
  cnx := Some (new connection ~conninfo:!conninfo ())

let rec get_cnx () =
  match !cnx with
  | Some c -> c
  | None ->
      connect () ;
      get_cnx ()

let close () =
  match !cnx with
  | Some c ->
      c#finish ;
      cnx := None
  | None -> ()

(* Quote SQL keywords (not strings, for this use [escape_string]. *)
let sql_quote s =
  "\""^ s ^"\"" (* FIXME: escape double quotes from s *)

let or_null conv = function
  | None -> null
  | Some v -> conv v

(* Quoting a value for textual representation of an array value.
 * "The array output routine will put double quotes around element values if
 * they are empty strings, contain curly braces, delimiter characters, double
 * quotes, backslashes, or white space, or match the word NULL. Double quotes
 * and backslashes embedded in element values will be backslash-escaped. For
 * numeric data types it is safe to assume that double quotes will never
 * appear, but for textual data types one should be prepared to cope with
 * either the presence or absence of quotes." -- postgresql manual *)
let sql_array_quote s =
  let need_quotes =
    s = "" || s = "NULL" ||
    string_exists (fun c -> c = '"' || c = '{' || c = '}' || c = ',' ||
                            c = '\\' || c = ' ') s in
  if need_quotes then
    (* Actually escape + quote *)
    let s = String.nreplace ~str:s ~sub:"\\" ~by:"\\\\" in
    let s = String.nreplace ~str:s ~sub:"\"" ~by:"\\\"" in
    "\""^ s ^"\""
  else
    s

let sql_array_unquote s =
  (* Because empty strings are quoted: *)
  assert (String.length s > 0) ;
  if s.[0] = '"' then (
    (* Actually unquote: *)
    assert (String.length s >= 2 && s.[String.length s - 1] = '"') ;
    let s = String.sub s 1 (String.length s - 2) in
    let s = String.nreplace ~str:s ~sub:"\\\"" ~by:"\"" in
    let s = String.nreplace ~str:s ~sub:"\\\\" ~by:"\\" in
    s
  ) else
    s

let sql_of_string_array a =
  (* TODO: BatArray.join *)
  "{"^ String.join "," (Array.to_list (Array.map sql_array_quote a)) ^"}"

let list = function
  | "" | "{}" ->
      []
  | s ->
      assert (String.length s >= 2) ;
      (* Remove the curly braces: *)
      assert (s.[0] = '{' && s.[String.length s - 1] = '}') ;
      let s = String.sub s 1 (String.length s - 2) in
      String.split_on_char ',' s |>
      List.map sql_array_unquote

let array =
  Array.of_list % list

let bool_of_string = function
  | "t" -> true
  | "f" -> false
  | s -> failwith ("Cannot convert to bool: "^ String.quote s)

module Command =
struct
  let rec get id =
    let cnx = get_cnx () in
    let tables = [| "command_isolate" ; "command_chroot" ; "command_docker" ;
                    "command_exec" ; "command_approve" ; "command_sequence" ;
                    "command_retry" ; "command_if" ; "command_nop" ;
                    "command_pause" ; "command_let" |] in
    let operation =
      array_find_mapi (fun i ->
        let params = [| string_of_int id |] in
        let res =
          cnx#exec ~expect:[Tuples_ok] ~params
            (* Postgres does not accept parameters in place of table names *)
            ("select * from "^ sql_quote tables.(i) ^" where command = $1") in
        if res#ntuples <> 1 then None else (
          log.debug "Got command tuple %a"
            (Array.print String.print) (res#get_tuple 0) ;
          let getv = res#getvalue 0 in
          let getn conv i =
            if res#getisnull 0 i then None else Some (conv (getv i)) in
          Some (match i with
          | 0 ->
              Api.Command.Isolate
                { builder = get (int_of_string (getv 1)) ;
                  subcommand = get (int_of_string (getv 2)) }
          | 1 ->
              Api.Command.Chroot
                { template = getv 1 }
          | 2 ->
              Api.Command.Docker
                { image = getv 1 }
          | 3 ->
              Api.Command.Exec
                { pathname = getv 1 ;
                  args = array (getv 2) ;
                  env = array (getv 3) ;
                  timeout = getn float_of_string 4 }
          | 4 ->
              Api.Command.Approve {
                subcommand = get (int_of_string (getv 1)) ;
                timeout = getn float_of_string 2 ;
                comment = getv 3 ;
                autosuccess = Lang.sql_bool_of_string (getv 4) }
          | 5 ->
              Api.Command.Sequence
                { subcommands = List.map (get % int_of_string) (list (getv 1)) }
          | 6 ->
              Api.Command.Retry
                { subcommand = get (int_of_string (getv 1)) ;
                  up_to = int_of_string (getv 2) }
          | 7 ->
              Api.Command.If
                { condition = get (int_of_string (getv 1)) ;
                  consequent = get (int_of_string (getv 2)) ;
                  alternative = get (int_of_string (getv 3)) }
          | 8 ->
              Api.Command.Nop { exit_code = int_of_string (getv 1) }
          | 9 ->
              Api.Command.Pause
                { subcommand = get (int_of_string (getv 1)) ;
                  duration = float_of_string (getv 2) }
          | 10 ->
              Api.Command.Let {
                subcommand = get (int_of_string (getv 1)) ;
                var_name = getv 2 ;
                default = getv 3 ;
                comment = getv 4 }
          | _ ->
              assert false)
        )
      ) tables in
    Api.Command.{ operation ; id }

  (* Returns the command id: *)
  let rec insert_or_update c =
    let cnx = get_cnx () in
    let table, field_params =
      match c.Api.Command.operation with
      | Nop { exit_code } ->
          "command_nop",
          [| "exit_code", string_of_int exit_code |]
      | Isolate { builder ; subcommand } ->
          "command_isolate",
          [| "builder", insert_or_update builder ;
             "subcommand", insert_or_update subcommand |]
      | Chroot { template } ->
          "command_chroot",
          [| "template", template |]
      | Docker { image } ->
          "command_docker",
          [| "image", image |]
      | Exec { pathname ; args ; env ; timeout } ->
          "command_exec",
          [| "pathname", pathname ;
             "args", sql_of_string_array args ;
             "env", sql_of_string_array env ;
             "timeout", or_null string_of_float timeout |]
      | Approve { subcommand ; timeout ; comment ; autosuccess } ->
          "command_approve",
          [| "subcommand", insert_or_update subcommand ;
             "timeout", or_null string_of_float timeout ;
             "comment", comment ;
             "autosuccess", Lang.sql_string_of_bool autosuccess |]
      | Let { var_name ; default ; subcommand ; comment } ->
          "command_let",
          [| "subcommand", insert_or_update subcommand ;
             "var_name", var_name ;
             "default_value", default ;
             "comment", comment |]
      | Sequence { subcommands } ->
          let ids = List.map insert_or_update subcommands in
          "command_sequence",
          [| "subcommands", "{"^ String.join "," ids ^"}" |]
      | Retry { subcommand ; up_to } ->
          "command_retry",
          [| "subcommand", insert_or_update subcommand ;
             "up_to", string_of_int up_to |]
      | If { condition ; consequent ; alternative } ->
          "command_if",
          [| "condition", insert_or_update condition ;
             "consequent", insert_or_update consequent ;
             "alternative", insert_or_update alternative |]
      | Pause { duration ; subcommand } ->
          "command_pause",
          [| "duration", string_of_float duration ;
             "subcommand", insert_or_update subcommand |]
    in
    let params = Array.map snd field_params in
    let command_id =
      if c.id > 0 then
        "(select "^ string_of_int c.id ^" as id)"
      else
        "(insert into command default values returning id)" in
    let req =
      "with new_command as "^ command_id
      ^ " insert into "^ sql_quote table ^" (command"
      ^ Array.fold_left (fun r (n, _) -> r ^","^ sql_quote n)
                        "" field_params
      ^ ") values ((select id from new_command)"
      ^ Array.fold_lefti (fun r i _ -> r ^",$"^ string_of_int (i + 1))
                         "" field_params
      ^ ") returning command" in
    log.debug "Inserting a command" ;
    let res = cnx#exec ~expect:[Tuples_ok] ~params req in
    res#getvalue 0 0

  let insert c =
    insert_or_update c |> int_of_string
end

(* Sum the stats of every descendant of the given run (excluding that run) *)
module DescStats =
struct
  let rec get id =
    let cnx = get_cnx () in
    let params =
      [| string_of_int id |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "select id, cpu_usr, cpu_sys, mem_usr, mem_sys
         from run
         where id <> parent_run and parent_run = $1" in
    let getv conv i j = conv (res#getvalue i j) in
    let tot = ref Api.RunStats.none in
    for i = 0 to res#ntuples - 1 do
      let desc = get (getv int_of_string i 0) in
      tot := Api.RunStats.add !tot desc
    done ;
    !tot
 end

module Run =
struct
  let rec get id =
    let cnx = get_cnx () in
    let params =
      [| string_of_int id |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "select \
           r.command, \
           coalesce(r.top_run, r.id) as top_run, \
           coalesce(r.parent_run, r.id) as parent_run, \
           p.name, \
           extract(epoch from r.created), \
           extract(epoch from r.started), \
           extract(epoch from r.stopped), \
           r.cgroup, r.pid, r.exit_code, \
           r.cpu_usr, r.cpu_sys, r.mem_usr, r.mem_sys, \
           array(select id from run where parent_run = r.id order by id) \
             as children, \
           w.message, c.path, d.instance, d.docker_id, v.value \
         from run r \
         join run rtop on rtop.id = coalesce(r.top_run, r.id) \
         left outer join program p on p.command = rtop.command \
         left outer join approved w on w.run = r.id \
         left outer join chroot_path c on c.run = r.id \
         left outer join docker_instance d on d.run = r.id \
         left outer join let_value v on v.run = r.id \
         where r.id = $1" in
    if res#ntuples <> 1 then
      failwith ("Cannot find a unique run with id "^ string_of_int id) ;
    log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple 0) ;
    let getv conv j = conv (res#getvalue 0 j) in
    let getn conv j = if res#getisnull 0 j then None else Some (getv conv j) in
    Api.Run.{
      id ;
      command = Command.get (getv int_of_string 0) ;
      top_run = getv int_of_string 1 ;
      parent_run = getv int_of_string 2 ;
      program = getn identity 3 ;
      created = getv float_of_string 4 ;
      started = getn float_of_string 5 ;
      stopped = getn float_of_string 6 ;
      cgroup = getn identity 7 ;
      pid = getn int_of_string 8 ;
      exit_code = getn int_of_string 9 ;
      stats_self =
        { cpu_usr = getn float_of_string 10 ;
          cpu_sys = getn float_of_string 11 ;
          mem_usr = getn int_of_string 12 ;
          mem_sys = getn int_of_string 13 } ;
      stats_desc = DescStats.get id ;
      children = array (getv identity 14) |>
                 Array.map (get % int_of_string) ;
      confirmation_msg = getn identity 15 ;
      chroot_path = getn identity 16 ;
      docker_instance = getn identity 17 ;
      docker_id = getn identity 18 ;
      var_value = getn identity 19 ;
      (* Populated independently as we do want to load logs on demand only: *)
      logs = [||] }

  let insert ?top_run ?parent_run cmd_id =
    let cnx = get_cnx () in
    let params =
      [| string_of_int cmd_id ;
         or_null string_of_int top_run ;
         or_null string_of_int parent_run |] in
    log.debug "Inserting run for command %d" cmd_id ;
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "insert into run (command, top_run, parent_run) \
         values ($1, $2, $3) returning id" in
    res#getvalue 0 0 |> int_of_string

  let start ?cgroup ?pid run_id =
    let cnx = get_cnx () in
    let params =
      [| string_of_int run_id ;
         or_null identity cgroup ;
         or_null string_of_int pid |] in
    log.debug "Starting run %d" run_id ;
    try
      cnx#exec ~expect:[Command_ok] ~params
        "update run set started = now(), cgroup = $2, pid = $3 \
         where id = $1" |>
      ignore
    with e ->
      Printf.sprintf "Cannot start run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith

  let stop run_id ?cpu_usr ?cpu_sys ?mem_usr ?mem_sys
           exit_code =
    let cnx = get_cnx () in
    let params =
      [| string_of_int run_id ;
         string_of_int exit_code ;
         or_null string_of_float cpu_usr ;
         or_null string_of_float cpu_sys ;
         or_null string_of_int mem_usr ;
         or_null string_of_int mem_sys |] in
    log.debug "Stopping run %d with exit_code %d, \
               CPU consumption: %a usr + %a sys, \
               MEM consumption: %a ram+swp + %a kernel"
      run_id exit_code
      (Option.print Float.print) cpu_usr
      (Option.print Float.print) cpu_sys
      (Option.print Int.print) mem_usr
      (Option.print Int.print) mem_sys ;
    try
      cnx#exec ~expect:[Command_ok] ~params
        "update run set stopped = now(), \
                        exit_code = $2, \
                        cpu_usr = $3, cpu_sys = $4, \
                        mem_usr = $5, mem_sys = $6 \
         where id = $1" |>
      ignore
    with e ->
      Printf.sprintf "Cannot stop run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith

  (* Return possibly very long list of old unstarted top runs *)
  let get_old_unstarted ~older_than =
    let cnx = get_cnx () in
    let params = [| string_of_float older_than |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "select id from run \
         where top_run is null and started is null and \
               now() - created > make_interval(0, 0, 0, 0, 0, 0, $1)" in
    Enum.init res#ntuples (fun i ->
      int_of_string (res#getvalue i 0))

  let expire run_id =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id |] in
    log.debug "Expiring run %d" run_id ;
    try
      (* Note: Leave this run alone if it has started *)
      cnx#exec ~expect:[Command_ok] ~params (
        "update run set started = now(), stopped = now(), exit_code = "
          ^ string_of_int Api.ExitStatus.expired ^
        "where id = $1 and started is null") |>
      ignore
    with e ->
      Printf.sprintf "Cannot expire run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith

  let cancel run_id =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id |] in
    log.debug "Cancelling run %d" run_id ;
    try
      cnx#exec ~expect:[Command_ok] ~params (
        "update run set stopped = now(), exit_code = "
          ^ string_of_int Api.ExitStatus.cancelled ^
        ", started = coalesce(stopped, now()) \
         where id = $1 and stopped is null") |>
      ignore
    with e ->
      Printf.sprintf "Cannot cancel run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith

end

module ListLogLines =
struct
  (* [run] might be the id of the top run we want all logs for, or an
   * individual run. *)
  let get ?offset ?limit ~run =
    let cnx = get_cnx () in
    let params =
      [| string_of_int run ;
         string_of_int (offset |? 0) ;
         (* [limit NULL] is equivalent to no limit: *)
         Option.map_default string_of_int null limit |] in
    let res =
      (* This awful query to circumvent the query planer insisting on seq-scanning
       * the logline table regardless of stats: *)
      cnx#exec ~expect:[Tuples_ok] ~params
        "select run, command, fd, \
                extract(epoch from time), line \
         from list_loglines \
         where run in (select id from run where id = $1 or top_run = $1) \
         order by time \
         offset $2 limit $3" in
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Api.LogLine.{
        run = getv int_of_string 0 ;
        command = getv int_of_string 1 ;
        fd = getv int_of_string 2 ;
        time = getv float_of_string 3 ;
        line = getv identity 4 })
end

module LogLine =
struct
  let insert run_id fd line =
    let cnx = get_cnx () in
    let params =
      [| string_of_int run_id ;
         string_of_int fd ;
         line |] in
    (* Best effort *)
    try
      log.debug "Inserting Logline %S for run %d" line run_id ;
      cnx#exec ~expect:[Command_ok] ~params
        "insert into logline (run, fd, line) values ($1, $2, $3)" |>
      ignore
    with e ->
      Printf.eprintf "Cannot insert a log line for run %d: %s\n" run_id
        (Printexc.to_string e)
end

module ChrootPath =
struct
  let get run_id =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "select path from chroot_path where run = $1" in
    if res#ntuples <> 1 then
      failwith ("Cannot find a unique chroot_path for run "^ params.(0)) ;
    log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple 0) ;
    res#getvalue 0 0

  let insert run_id path =
    let cnx = get_cnx () in
    let params =
      [| string_of_int run_id ; path |] in
    try
      log.debug "Inserting chroot path %S for run %d" path run_id ;
      cnx#exec ~expect:[Command_ok] ~params
        "insert into chroot_path (run, path) values ($1, $2)" |>
      ignore
    with e ->
      Printf.sprintf "Cannot insert a chroot_path for run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith
end

module DockerInstance =
struct
  let get run_id =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "select instance from docker_instance where run = $1" in
    if res#ntuples <> 1 then
      failwith ("Cannot find a unique docker_instance for run "^ params.(0)) ;
    log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple 0) ;
    res#getvalue 0 0

  let insert run_id instance docker_id =
    let cnx = get_cnx () in
    let params =
      [| string_of_int run_id ; instance ; docker_id |] in
    try
      log.debug "Inserting docker_instance %S for run %d" instance run_id ;
      cnx#exec ~expect:[Command_ok] ~params
        "insert into docker_instance (run, instance, docker_id) values \
         ($1, $2, $3)" |>
      ignore
    with e ->
      Printf.sprintf "Cannot insert a docker_instance for run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith
end

module Program =
struct
  let get_ field value =
    let cnx = get_cnx () in
    let params = [| value |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        ("select name, extract(epoch from created), command \
          from program \
          where "^ sql_quote field ^" = $1 and deleted < created") in
    if res#ntuples <> 1 then
      failwith ("Cannot find a unique program with "^ field ^" = "^ value) ;
    log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple 0) ;
    Api.Program.{
      name = res#getvalue 0 0 ;
      created = float_of_string (res#getvalue 0 1) ;
      command = Command.get (int_of_string (res#getvalue 0 2)) }

  let get = get_ "name"
  let of_command cmd = get_ "command" (string_of_int cmd.Api.Command.id)

  let insert p =
    let cnx = get_cnx () in
    let command = Command.insert_or_update p.Api.Program.command in
    let params = [| p.Api.Program.name ; command |] in
    log.debug "Inserting program %S" p.name ;
    cnx#exec ~expect:[Command_ok] ~params
      "insert into program (name, command) values ($1, $2)" |>
    ignore

  let delete prev_name =
    let cnx = get_cnx () in
    let params = [| prev_name |] in
    log.debug "Deleting program %S" prev_name ;
    cnx#exec ~expect:[Command_ok] ~params
      "update program set deleted = now() where name = $1" |>
    ignore

  let update p prev_name =
    let cnx = get_cnx () in
    let command = Command.insert_or_update p.Api.Program.command in
    let params = [| p.Api.Program.name ; command ; prev_name |] in
    log.debug "Updating program %S" prev_name ;
    cnx#exec ~expect:[Command_ok] ~params
      "update program set name = $1, command = $2 where name = $3" |>
    ignore
end

(*
 * Views
 *)

(*
 * ListPastRuns
 *
 * List past runs of programs, paginated.
 * Navigation:
 * - to the detail of a given program (to edit it) or run (to view the logs)
 * - create a new program
 * - run a program
 *)
module ListPastRuns =
struct
  (* [oldest_top_run] is for pagination: return only runs which top_run is
   * _stricly_ older than that will be returned. *)
  (* TODO: prepare stmt *)
  let get ?program ?oldest_top_run ?(limit=20) () =
    let cnx = get_cnx () in
    let params = [| string_of_int limit |] in
    let params =
      if oldest_top_run = None then params else
        Array.append params [| string_of_int (Option.get oldest_top_run) |] in
    let params =
      if program = None then params else
        Array.append params [| Option.get program |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        ("select name, top_run, \
                 extract(epoch from created), \
                 extract(epoch from started), \
                 extract(epoch from stopped), \
                 exit_code, \
                 cpu_usr, cpu_sys, mem_usr, mem_sys \
         from list_past_runs \
         where true " ^
         (if oldest_top_run = None then "" else
          "and started < (select started from list_past_runs \
                          where top_run = $2) ") ^
         (if program = None then "" else
           ("and name = $"^ string_of_int (Array.length params) ^ " ")) ^
         "order by started desc limit $1") in
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      let getn conv j = if res#getisnull i j then None else Some (getv conv j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      let top_run = getv int_of_string 1 in
      Api.ListPastRuns.{
        name = getv identity 0 ;
        top_run ;
        created = getv float_of_string 2 ;
        started = getn float_of_string 3 ;
        stopped = getn float_of_string 4 ;
        exit_code = getn int_of_string 5 ;
        stats_self =
          { cpu_usr = getn float_of_string 6 ;
            cpu_sys = getn float_of_string 7 ;
            mem_usr = getn int_of_string 8 ;
            mem_sys = getn int_of_string 9 } ;
        stats_desc = DescStats.get top_run })
end

(*
 * List all defined programs, as well as their last run status.
 *)
module ListPrograms =
struct
  (* Returns *all* programs with info about their last run *)
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select name, \
                last_run, \
                extract(epoch from last_start), \
                extract(epoch from last_stop),
                last_exit_code \
         from list_programs order by name" in
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      let getn conv j = if res#getisnull i j then None else Some (getv conv j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Api.ListPrograms.{
        name = getv identity 0 ;
        last_run = getn int_of_string 1 ;
        last_start = getn float_of_string 2 ;
        last_stop = getn float_of_string 3 ;
        last_exit_code = getn int_of_string 4 })
end

(*
 * Command states / tools for the stepper.
 *)
module ListRunningSequences =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select id, exit_codes, step_count, all_success \
         from list_running_sequences" in
    log.debug "%d sequences are unfinished." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Api.ListRunningSequences.{
        run = Run.get (getv int_of_string 0) ;
        exit_codes = array (getv identity 1) |>
                     Array.map int_of_string ;
        step_count = getv int_of_string 2 ;
        all_success = getv bool_of_string 3 })
end

module ListRunningPauses =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run, duration, subcommand from list_running_pauses" in
    log.debug "%d pauses are unfinished." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Api.ListRunningPauses.{
        run = Run.get (getv int_of_string 0) ;
        duration = getv float_of_string 1 ;
        subcommand = getv int_of_string 2 })
end

module ListRunningIfs =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run, condition, consequent, alternative, \
                condition_run, consequent_run, alternative_run \
         from list_running_ifs order by created" in
    log.debug "%d ifs are unfinished." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      let getn conv j = if res#getisnull i j then None else Some (getv conv j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Api.ListRunningIfs.{
        run = Run.get (getv int_of_string 0) ;
        condition = getv int_of_string 1 ;
        consequent = getv int_of_string 2 ;
        alternative = getv int_of_string 3 ;
        condition_run = Option.map Run.get (getn int_of_string 4) ;
        consequent_run = Option.map Run.get (getn int_of_string 5) ;
        alternative_run = Option.map Run.get (getn int_of_string 6) })
end

module ListWaitingTerminals =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select id from list_waiting_terminals order by created" in
    log.debug "%d terminals are waiting to start." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Run.get (int_of_string (res#getvalue i 0)))
end

module Approval =
struct
  let insert run_id msg =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id ; msg |] in
    try
      cnx#exec ~expect:[Command_ok] ~params
        "insert into approved (run, message) values ($1, $2)" |>
      ignore
    with e ->
      Printf.sprintf "Cannot confirm run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith
end

module LetValue =
struct
  let insert run_id value =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id ; value |] in
    try
      cnx#exec ~expect:[Command_ok] ~params
        "insert into let_value (run, value) values ($1, $2)" |>
      ignore
    with e ->
      Printf.sprintf "Cannot set variable value for run %d to %S: %s\n"
        run_id value (Printexc.to_string e) |>
      failwith
end

module ListPendingLets =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run, subrun, subcommand from list_pending_lets" in
    log.debug "%d lets are waiting." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      log.debug "Got tuple %a" (Array.print String.print) (res #get_tuple i) ;
      let getv conv j = conv (res#getvalue i j) in
      let getn conv j = if res#getisnull i j then None else Some (getv conv j) in
      Api.ListPendingLets.{
        run = Run.get (getv int_of_string 0) ;
        subrun = Option.map Run.get (getn int_of_string 1) ;
        subcommand = getv int_of_string 2 })
end

module ListPendingApprovals =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run, extract(epoch from time), autosuccess \
        from list_pending_approvals order by time" in
    log.debug "%d approvals are waiting." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      log.debug "Got tuple %a" (Array.print String.print) (res #get_tuple i) ;
      Api.ListPendingApprovals.{
        run = Run.get (int_of_string (res#getvalue i 0)) ;
        time = if res#getisnull i 1 then None else
                 Some (float_of_string (res#getvalue i 1)) ;
        autosuccess = Lang.sql_bool_of_string (res#getvalue i 2) })
end

module ListPendingIsolations =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run from list_pending_isolations order by created" in
    log.debug "%d isolation commands are waiting." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      log.debug "Got tuple %a" (Array.print String.print) (res #get_tuple i) ;
      Run.get (int_of_string (res#getvalue i 0)))
end

module FindRun =
struct
  let get ~parent_run ~cmd =
    let cnx = get_cnx () in
    let params =
      [| string_of_int parent_run ;
         string_of_int cmd |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "select id from run where parent_run = $1 and command = $2" in
    assert (res#ntuples = 1) ;
    Run.get (int_of_string (res#getvalue 0 0))
end

module ListObsoleteRuns =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select id from list_obsolete_runs" in
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      getv int_of_string 0)
end
