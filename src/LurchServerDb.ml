open Batteries
open Postgresql

open LurchServerLib
module Api = LurchApiTypes

(*$inject
  open Batteries *)

let () =
  Printexc.register_printer (function
    | Error e ->
        Some (string_of_error e)
    | _ ->
        None)

let cnx = ref None
let conninfo = ref ""

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

let sql_of_string_list conv l =
  "{"^ String.join "," (List.map conv l) ^"}"

let sql_of_string_array conv =
  sql_of_string_list conv % Array.to_list

let list conv = function
  | "" | "{}" ->
      []
  | s ->
      assert (String.length s >= 2) ;
      (* Remove the outer curly braces: *)
      assert (s.[0] = '{' && s.[String.length s - 1] = '}') ;
      let s = String.sub s 1 (String.length s - 2) in
      (* Split on all outer comas (not those within curly braces!), while
       * also skipping comas in strings: *)
      let rec loop lst in_string depth start i =
        if i >= String.length s then (
          assert (not in_string) ;
          assert (depth = 0) ;
          String.sub s start (i - start) :: lst
        ) else match s.[i] with
        | '"' when not in_string ->
            loop lst true depth start (i + 1)
        | '"' when in_string ->
            loop lst false depth start (i + 1)
        | ',' when not in_string && depth = 0 ->
            let lst = String.sub s start (i - start) :: lst in
            loop lst false depth (i + 1) (i + 1)
        | '{' when not in_string ->
            loop lst false (depth + 1) start (i + 1)
        | '}' when not in_string ->
            assert (depth > 0) ;
            loop lst false (depth - 1) start (i + 1)
        | _ ->
            loop lst in_string depth start (i + 1) in
      loop [] false 0 0 0 |>
      List.fold_left (fun lst s ->
        conv (sql_array_unquote s) :: lst
      ) []

(*$= list & ~printer:dump
  ["foo"] (list identity "{foo}")
  ["foo" ; "bar"] (list identity "{foo,bar}")
  ["{foo,bar}"] (list identity "{{foo,bar}}")
  ["-i" ; "-e" ; "s/^AC_INIT(ramen, .+)$/AC_INIT(ramen, ${revision})"] \
    (list identity "{-i,-e,\"s/^AC_INIT(ramen, .+)$/AC_INIT(ramen, ${revision})\"}")
*)

let get_env s =
  list identity s |>
  List.map (fun a ->
    match list identity a with
    | [ n ; v ] -> (n, v)
    | _ -> assert false)

(*$= get_env & ~printer:dump
  [ "FOO", "Bar" ] (get_env "{{FOO,Bar}}")
*)

let array conv =
  Array.of_list % (list conv)

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
                    "command_pause" ; "command_let" ; "command_spawn" ;
                    "command_for_loop" ; "command_break" ; "command_wait" |] in
    let operation =
      array_find_mapi (fun table_num ->
        let params = [| string_of_int id |] in
        let res =
          cnx#exec ~expect:[Tuples_ok] ~params
            (* Postgres does not accept parameters in place of table names *)
            ("select * from "^ sql_quote tables.(table_num) ^
             " where command = $1") in
        if res#ntuples <> 1 then None else (
          log.debug "Got command tuple %a"
            (Array.print String.print) (res#get_tuple 0) ;
          let getv = res#getvalue 0 in
          let getn conv i =
            if res#getisnull 0 i then None else Some (conv (getv i)) in
          Some (match table_num with
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
                { working_dir = getv 1 ;
                  pathname = getv 2 ;
                  args = array identity (getv 3) ;
                  env = array identity (getv 4) ;
                  timeout = getn float_of_string 5 }
          | 4 ->
              Api.Command.Approve {
                timeout = getn float_of_string 1 ;
                comment = getv 2 ;
                autosuccess = Api.sql_bool_of_string (getv 3) }
          | 5 ->
              Api.Command.Sequence
                { subcommands = List.map (get % int_of_string)
                                         (list identity (getv 1)) }
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
                { duration = float_of_string (getv 1) }
          | 10 ->
              Api.Command.Let {
                subcommand = get (int_of_string (getv 1)) ;
                var_name = getv 2 ;
                default = getv 3 ;
                comment = getv 4 }
          | 11 ->
              Api.Command.Spawn {
                program = getv 1 ;
                version = getn int_of_string 2 }
          | 12 ->
              Api.Command.ForLoop {
                var_name = getv 1 ;
                values = array identity (getv 2) ;
                subcommand = get (int_of_string (getv 3)) }
          | 13 ->
              Api.Command.Break {
                depth = int_of_string (getv 1) }
          | 14 ->
              Api.Command.Wait {
                minute = list int_of_string (getv 1) ;
                hour = list int_of_string (getv 2) ;
                mday = list int_of_string (getv 3) ;
                month = list int_of_string (getv 4) ;
                wday = list int_of_string (getv 5) }
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
      | Exec { working_dir ; pathname ; args ; env ; timeout } ->
          "command_exec",
          [| "working_dir", working_dir ;
             "pathname", pathname ;
             "args", sql_of_string_array sql_array_quote args ;
             "env", sql_of_string_array sql_array_quote env ;
             "timeout", or_null string_of_float timeout |]
      | Approve { timeout ; comment ; autosuccess } ->
          "command_approve",
          [| "timeout", or_null string_of_float timeout ;
             "comment", comment ;
             "autosuccess", Api.sql_string_of_bool autosuccess |]
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
      | Pause { duration } ->
          "command_pause",
          [| "duration", string_of_float duration |]
      | Wait { minute ; hour ; mday ; month ; wday } ->
          "command_wait",
          [| "minute", sql_of_string_list string_of_int minute ;
             "hour", sql_of_string_list string_of_int hour ;
             "mday", sql_of_string_list string_of_int mday ;
             "month", sql_of_string_list string_of_int month ;
             "wday", sql_of_string_list string_of_int wday |]
      | Spawn { program ; version } ->
          "command_spawn",
            [| "program", program ;
               "version", or_null string_of_int version |]
      | ForLoop { var_name ; values ; subcommand } ->
          "command_for_loop",
          [| "var_name", var_name ;
             "values",  sql_of_string_array sql_array_quote values ;
             "subcommand", insert_or_update subcommand |]
      | Break { depth } ->
          "command_break", [| "depth", string_of_int depth |]
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
      (* FIXME: Have a recursive query from within postgres! *)
      cnx#exec ~expect:[Tuples_ok] ~params
        "select id, cpu_usr, cpu_sys, mem_usr, mem_sys from run
         where parent_run = $1" in
    let tot = ref Api.RunStats.none in
    for i = 0 to res#ntuples - 1 do
      let getv conv j = conv (res#getvalue i j) in
      let getn conv j = if res#getisnull i j then None else Some (getv conv j) in
      let self =
        Api.RunStats.make (getn float_of_string 1) (getn float_of_string 2)
                          (getn float_of_string 3) (getn float_of_string 4) in
      let desc = get (getv int_of_string 0) in
      tot := Api.RunStats.(aggr !tot (aggr self desc))
    done ;
    (* Hide the fact that this is a sum: *)
    { !tot with
      num_cpu_usr = 1 ;
      num_cpu_sys = 1 ;
      num_mem_usr = 1 ;
      num_mem_sys = 1 }
 end

module Run =
struct
  let rec get id =
    let cnx = get_cnx () in
    let params =
      [| string_of_int id |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "with recursive var_bindings as ( \
           select r.id, r.parent_run, b.var_name, b.value
           from
             run r \
             left outer join run_bindings b on b.id  = r.id \
           where r.id = $1 \
           union \
           select r2.id, r2.parent_run, b2.var_name, b2.value \
           from \
             run r2 \
             left outer join run_bindings b2 on b2.id = r2.id \
             inner join var_bindings vb on r2.id = vb.parent_run \
         ) \
         select \
           r.command, \
           coalesce(r.top_run, r.id) as top_run, \
           coalesce(r.parent_run, r.id) as parent_run, \
           p.name, \
           p.version, \
           extract(epoch from r.created), \
           extract(epoch from r.started), \
           extract(epoch from r.stopped), \
           r.cgroup, r.pid, r.exit_code, \
           r.cpu_usr, r.cpu_sys, r.mem_usr, r.mem_sys, \
           array(select id from run where parent_run = r.id order by id) \
             as children, \
           w.message, w.approved_by, \
           c.path, d.instance, d.docker_id, \
           v.value, v.set_by, \
           array(select array[var_name, value] from var_bindings \
                 where var_name is not null) \
             as environment \
         from run r \
         join run rtop on rtop.id = coalesce(r.top_run, r.id) \
         join program p on p.command = rtop.command \
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
      program = getv identity 3 ;
      version = getv int_of_string 4 ;
      created = getv float_of_string 5 ;
      started = getn float_of_string 6 ;
      stopped = getn float_of_string 7 ;
      cgroup = getn identity 8 ;
      pid = getn int_of_string 9 ;
      exit_code = getn int_of_string 10 ;
      stats_self =
        Api.RunStats.make (getn float_of_string 11) (getn float_of_string 12)
                          (getn float_of_string 13) (getn float_of_string 14) ;
      stats_desc = DescStats.get id ;
      children = array identity (getv identity 15) |>
                 Array.map (get % int_of_string) ;
      approval_msg = getn identity 16 ;
      approved_by = getn identity 17 ;
      chroot_path = getn identity 18 ;
      docker_instance = getn identity 19 ;
      docker_id = getn identity 20 ;
      var_value = getn identity 21 ;
      var_set_by = getn identity 22 ;
      env = get_env (getv identity 23) ;
      (* Populated independently as we do want to load logs on demand only: *)
      logs = [||] }

  let insert ?top_run ?parent_run ?creator_user ?creator_run cmd_id =
    let cnx = get_cnx () in
    let params =
      [| string_of_int cmd_id ;
         or_null string_of_int top_run ;
         or_null string_of_int parent_run ;
         or_null string_of_int creator_run ;
         or_null identity creator_user |] in
    log.debug "Inserting run for command %d" cmd_id ;
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        "insert into run (command, top_run, parent_run, creator_run, \
                          creator_user) \
         values ($1, $2, $3, $4, $5) returning id" in
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

  let stop run_id ?stats exit_code =
    let cnx = get_cnx () in
    let params =
      match stats with
      | Some stats ->
          [| string_of_int run_id ;
             string_of_int exit_code ;
             or_null string_of_float stats.Api.RunStats.cpu_usr ;
             or_null string_of_float stats.cpu_sys ;
             or_null Int64.(to_string % of_float) stats.mem_usr ;
             or_null Int64.(to_string % of_float) stats.mem_sys |]
      | None ->
          [| string_of_int run_id ;
             string_of_int exit_code ;
             null ; null ; null ; null |] in
    log.debug "Stopping run %d with exit_code %d" run_id exit_code ;
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
        "select instance, docker_id from docker_instance where run = $1" in
    if res#ntuples <> 1 then
      failwith ("Cannot find a unique docker_instance for run "^ params.(0)) ;
    log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple 0) ;
    res#getvalue 0 0, res#getvalue 0 1

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
  (* Only select the latest version.
   * TODO: make it possible to select and display a former version. *)
  let get name =
    let cnx = get_cnx () in
    let params = [| name |] in
    let res =
      cnx#exec ~expect:[Tuples_ok] ~params
        ("select name, version, extract(epoch from created), command \
          from program \
          where name = $1 and deleted < created \
          order by version desc limit 1") in
    if res#ntuples <> 1 then
      failwith ("Cannot find program named "^ name) ;
    log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple 0) ;
    let getv conv j = conv (res#getvalue 0 j) in
    Api.Program.{
      name = getv identity 0 ;
      version = getv int_of_string 1 ;
      created = getv float_of_string 2 ;
      command = Command.get (getv int_of_string 3) }

  let insert p =
    let cnx = get_cnx () in
    let command = Command.insert_or_update p.Api.Program.command in
    let params = [| p.name ; command |] in
    log.debug "Inserting program %S" p.name ;
    cnx#exec ~expect:[Command_ok] ~params
      ("insert into program (name, command) values ($1, $2)") |>
    ignore

  let delete prev_name =
    let cnx = get_cnx () in
    let params = [| prev_name |] in
    log.debug "Deleting program %S" prev_name ;
    cnx#exec ~expect:[Command_ok] ~params
      "update program set deleted = now() where name = $1" |>
    ignore

  (* Program are versioned, an update just creates a new version, which
   * happen automatically in the DB when a program with the same name is
   * inserted. If the name is different we just insert that as a new
   * program. So [prev_name] is actually not needed. *)
  let update p _prev_name =
    insert p
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
        ("select name, version, top_run, \
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
      let top_run = getv int_of_string 2 in
      Api.ListPastRuns.{
        name = getv identity 0 ;
        version = getv int_of_string 1 ;
        top_run ;
        created = getv float_of_string 3 ;
        started = getn float_of_string 4 ;
        stopped = getn float_of_string 5 ;
        exit_code = getn int_of_string 6 ;
        stats_self =
          Api.RunStats.make (getn float_of_string 7) (getn float_of_string 8)
                            (getn float_of_string 9) (getn float_of_string 10) ;
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
                version, \
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
        last_run_version = getv int_of_string 1 ;
        last_run = getn int_of_string 2 ;
        last_start = getn float_of_string 3 ;
        last_stop = getn float_of_string 4 ;
        last_exit_code = getn int_of_string 5 })
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
        exit_codes = array identity (getv identity 1) |>
                     Array.map int_of_string ;
        step_count = getv int_of_string 2 ;
        all_success = getv bool_of_string 3 })
end

module ListRunningForLoops =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select id, exit_codes from list_running_for_loops" in
    log.debug "%d for loops are unfinished." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Api.ListRunningForLoops.{
        run = Run.get (getv int_of_string 0) ;
        exit_codes = array identity (getv identity 1) |>
                     Array.map int_of_string })
end

module ListRunningPauses =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run, duration from list_running_pauses" in
    log.debug "%d pauses are unfinished." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      log.debug "Got tuple %a" (Array.print String.print) (res#get_tuple i) ;
      Api.ListRunningPauses.{
        run = Run.get (getv int_of_string 0) ;
        duration = getv float_of_string 1 })
end

module ListRunningWaits =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run, minute, hour, mday, month, wday
         from list_running_waits" in
    log.debug "%d waits are unfinished." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      Api.ListRunningWaits.{
        run = Run.get (getv int_of_string 0) ;
        minute = getv (list int_of_string) 2 ;
        hour = getv (list int_of_string) 3 ;
        mday = getv (list int_of_string) 4 ;
        month = getv (list int_of_string) 5 ;
        wday = getv (list int_of_string) 6 })
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
  let insert run_id msg user =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id ; msg ; user |] in
    try
      cnx#exec ~expect:[Command_ok] ~params
        "insert into approved (run, message, approved_by) values ($1, $2, $3)" |>
      ignore
    with e ->
      Printf.sprintf "Cannot confirm run %d: %s\n" run_id
        (Printexc.to_string e) |>
      failwith
end

module LetValue =
struct
  let insert run_id value user =
    let cnx = get_cnx () in
    let params = [| string_of_int run_id ; value ; user |] in
    try
      cnx#exec ~expect:[Command_ok] ~params
        "insert into let_value (run, value, set_by) values ($1, $2, $3)" |>
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
        "select run, value, subrun, subcommand from list_pending_lets" in
    log.debug "%d lets are waiting." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      log.debug "Got tuple %a" (Array.print String.print) (res #get_tuple i) ;
      let getv conv j = conv (res#getvalue i j) in
      let getn conv j = if res#getisnull i j then None else Some (getv conv j) in
      Api.ListPendingLets.{
        run = Run.get (getv int_of_string 0) ;
        value = getn identity 1 ;
        subrun = Option.map Run.get (getn int_of_string 2) ;
        subcommand = getv int_of_string 3 })
end

module ListPendingApprovals =
struct
  let get () =
    let cnx = get_cnx () in
    let res =
      cnx#exec ~expect:[Tuples_ok]
        "select run, extract(epoch from time), timeout, autosuccess \
        from list_pending_approvals order by time" in
    log.debug "%d approvals are waiting." res#ntuples ;
    Enum.init res#ntuples (fun i ->
      let getv conv j = conv (res#getvalue i j) in
      let getn conv j = if res#getisnull i j then None else Some (getv conv j) in
      log.debug "Got tuple %a" (Array.print String.print) (res #get_tuple i) ;
      Api.ListPendingApprovals.{
        run = getv (Run.get % int_of_string) 0 ;
        time = getn float_of_string 1 ;
        timeout = getn float_of_string 2 ;
        autosuccess = getv Api.sql_bool_of_string 3 })
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
