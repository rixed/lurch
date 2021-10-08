let to_json_buffer to_json x =
  let b = Buffer.create 10_000 in
  to_json b x ;
  b

let to_json_string to_json x =
  to_json_buffer to_json x |>
  Buffer.contents

let int_to_json_buffer d =
  let b = Buffer.create 20 in
  Buffer.add_string b (string_of_int d) ;
  b

module Command =
struct
  type operation =
    | Nop of { exit_code : int }
    | Isolate of
        { builder : t ; subcommand : t }
    | Chroot of
        { template : string }
    | Docker of
        { image : string }
    | Exec of
        { pathname : string ;
          (* [pathname] will be prepended automatically. This is unavoidable
           * since when isolating with docker we actually do not perform the
           * execve directly. *)
          args : string array ;
          env : string array ;
          timeout : float option }
    | Approve of
        { subcommand : t ; timeout : float option ; comment : string ;
          autosuccess : bool }
    | Let of
        { subcommand : t ; var_name : string ; default : string ;
          comment : string }
    | Sequence of
        { subcommands : t list }
    | Retry of
        { subcommand : t ; up_to : int }
    | If of
        { condition : t ; consequent : t ; alternative : t }
    | Pause of
        { duration : float ; subcommand : t }
    | Spawn of
        { (* Better than taking the run_id of the program to spawn, storing
             its name allow to make use of variable expansion. Also survive
             program changes (the table of programs is append-only): *)
          program : string }
    [@@deriving json]

  and t = { id : int ; operation : operation }
    [@@deriving json]

  let rec fold f u cmd =
    match cmd.operation with
    | Nop _ | Chroot _ | Docker _ | Exec _ | Spawn _ ->
        f u cmd
    | Isolate { subcommand }
    | Approve { subcommand }
    | Let { subcommand }
    | Retry { subcommand }
    | Pause { subcommand } ->
        let u = fold f u subcommand in
        f u cmd
    | Sequence { subcommands } ->
        let u = List.fold_left (fold f) u subcommands in
        f u cmd
    | If { condition ; consequent ; alternative } ->
        let u = fold f (fold f (fold f u condition) consequent) alternative in
        f u cmd

  let iter f op = fold (fun () -> f) () op

  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]

  let name_of_operation = function
    | Nop _ -> "no-op"
    | Isolate _ -> "isolate"
    | Chroot _ -> "chroot"
    | Docker _ -> "docker"
    | Exec _ -> "exec"
    | Approve _ -> "approve"
    | Let _ -> "let"
    | Sequence _ -> "sequence"
    | Retry _ -> "retry"
    | If _ -> "if"
    | Pause _ -> "pause"
    | Spawn _ -> "spawn"

  let rec string_of_operation =
    let or_null conv = function
      | None -> "null"
      | Some v -> conv v in
    let join conv lst =
      List.fold_left (fun s x ->
        if s = "" then conv x else s ^", "^ conv x
      ) "" lst in
    function
    | Nop _ ->
        "Nop"
    | Isolate { builder ; subcommand } ->
        "Isolate(builder:"^ to_string builder ^
        ", subcommand:"^ to_string subcommand ^")"
    | Chroot { template } ->
        "Chroot(template:"^ template ^")"
    | Docker { image } ->
        "Docker(image:"^ image ^")"
    | Exec { pathname ; args ; env ; timeout } ->
        "Exec(pathname:"^ pathname ^", args:…, env:…, timeout:"^
        or_null string_of_float timeout ^")"
    | Approve { subcommand ; timeout ; comment ; autosuccess } ->
        "Approve(subcommand:"^ to_string subcommand ^", timeout:"^
        or_null string_of_float timeout ^ ",comment:"^ comment ^", autosuccess:"^
        string_of_bool autosuccess ^")"
    | Let { var_name ; default ; subcommand ; comment } ->
        "Let(var_name:"^ var_name ^", default:"^ default ^
        "subcommand:"^ to_string subcommand ^", comment:"^ comment ^")"
    | Sequence { subcommands } ->
        "Sequence("^ join to_string subcommands ^")"
    | Retry { subcommand ; up_to } ->
        "Retry(subcommand:"^ to_string subcommand ^", up_to:"^
        string_of_int up_to ^")"
    | If { condition ; consequent ; alternative } ->
        "If(condition:"^ to_string condition ^
        ", consequent:"^ to_string consequent ^
        ", alternative:"^ to_string alternative ^")"
    | Pause { duration ; subcommand } ->
        "Pause(duration:"^ string_of_float duration ^", subcommand:"^
        to_string subcommand ^")"
    | Spawn { program } ->
        "Spawn(program:"^ program ^")"

  and to_string t =
    "{id:"^ string_of_int t.id ^ " operation:"^ string_of_operation t.operation ^"}"
end

module Status =
struct
  type t = int
end

module Program =
struct
  type t =
    { name : string ;
      created : float ;
      command : Command.t }
    [@@deriving json]

  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]
end

module LogLine =
struct
  type t =
    { (* The run of the actual command (may be a subcommand if not
       * the same as top_run) *)
      run : int ;
      command : int ;
      fd : int ;
      time : float ;
      line : string }
    [@@deriving json]

  type ts = t array
    [@@deriving json]

  let array_to_json_string = to_json_string ts_to_json
  let array_to_json_buffer = to_json_buffer ts_to_json
  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]
  let array_of_json_string : string -> t array = [%of_json: t array]
end

module ExitStatus =
struct
  type t =
    | Completed
    | Failed of int
    | Interrupted of int (* signal number *)
    | CouldNotStart
    (* Runs cancelled when the stepper start because they were too old. Not
     * to be confused with timed out runs, which are TimedOut: *)
    | Expired
    | Cancelled
    | TimedOut

  (* Those are specific to lurch: *)
  let expired = -129
  let cancelled = -130
  let timed_out = -131

  let of_code code =
    if code = expired then Expired else
    if code = cancelled then Cancelled else
    if code = timed_out then TimedOut else
    if code < 0 && code >= -128 then Interrupted (-code) else
    if code = 127 then CouldNotStart else
    if code = 0 then Completed else
    if code > 0 then Failed code else
    assert false

  let to_string = function
    | Completed -> "Completed"
    | Failed e -> "Exited with code "^ string_of_int e
    | Interrupted s -> "Interrupted by signal "^ string_of_int s
    | CouldNotStart -> "Could not start"
    | Expired -> "Expired"
    | Cancelled -> "Cancelled"
    | TimedOut -> "Timed out"

  type err_level = Ok | Warn | Err

  let err_level = function
    | Completed -> Ok
    | Failed _ | CouldNotStart | Expired | TimedOut -> Err
    | Interrupted _ | Cancelled -> Warn
end

module RunStats =
struct
  type t =
    { cpu_usr : float option ;
      cpu_sys : float option ;
      mem_usr : int option ;
      mem_sys : int option }
    [@@deriving json]

  let none =
    { cpu_usr = None ;
      cpu_sys = None ;
      mem_usr = None ;
      mem_sys = None }

  let add a b =
    let do_op op a b =
      match a, b with
      | Some a, Some b -> Some (op a b)
      | Some a, None | None, Some a -> Some a
      | None, None -> None in
    let (+) = do_op (+)
    and (+.) = do_op (+.) in
    { cpu_usr = a.cpu_usr +. b.cpu_usr ;
      cpu_sys = a.cpu_sys +. b.cpu_sys ;
      mem_usr = a.mem_usr + b.mem_usr ;
      mem_sys = a.mem_sys + b.mem_sys }

  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]
end

module Run =
struct
  type t =
    { id : int ;
      command : Command.t ;
      top_run : int ; (* Maybe self - aka NULL in the DB *)
      parent_run : int ; (* Maybe self - aka NULL in the DB *)
      program : string option ;
      created : float ;
      started : float option ;
      stopped : float option ;
      cgroup : string option ;
      pid : int option ;
      exit_code : int option ;
      (* Stats: *)
      stats_self : RunStats.t ;
      stats_desc : RunStats.t ;
      (* Children are reified here because it makes displaying runs much
       * easier for the client: *)
      children : t array ;
      (* Environment in which subcommands are run, aka list of all set
       * variables: *)
      env : (string * string) list ;
      (* Initially empty, populated by a specific call "get_logs": *)
      logs : LogLine.t array ;
      (* This depends on the command. They are *not* updated when the
       * server insert a new confirmation etc, but that's OK because they
       * are used on the client only: *)
      confirmation_msg : string option ;
      chroot_path : string option ;
      docker_instance : string option ;
      docker_id : string option ;
      var_value : string option }
    [@@deriving json]

  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]

  (* Expand in string s any "${VAR}" by its value: *)
  let var_expand env s =
    (* [i] scans [s] and [j] scans within "${VAR}". Returns the new string *)
    let rec repl1 i j s =
      if i + j >= String.length s then s else
      match s.[i + j] with
      | '$' ->
          repl1 (i + j) 1 s
      | '{' when j = 1 ->
          repl1 i 2 s
      | '}' when j >= 2 ->
          let var_name = String.sub s (i + 2) (j - 2) in
          let s, i =
            let e = i + j + 1 in
            match List.assoc var_name env with
            | exception Not_found ->
                (* ignore it then *)
                s, e
            | v ->
                String.sub s 0 i ^ v ^ String.sub s e (String.length s - e),
                i + String.length v in
          repl1 i 0 s
      | _ when j >= 2 ->
          repl1 i (j + 1) s
      | _ ->
          repl1 (i + 1) 0 s in
    let rec loop s =
      let s' = repl1 0 0 s in
      if s == s' then s else loop s' in
    loop s

  (*$< Run *)

  (*$= var_expand & ~printer:BatPervasives.identity
    "" (var_expand [] "")
    "foo" (var_expand [] "foo")
    "foo" (var_expand [ "bar", "baz" ] "foo")
    "fooglopbaz" (var_expand [ "bar", "glop" ] "foo${bar}baz")
    "foo${zoo}baz" (var_expand [ "bar", "glop" ] "foo${zoo}baz")
    "foo${bar" (var_expand [ "bar", "glop" ] "foo${bar")
    "foo!baz" (var_expand [ "bar", "glop" ; "pas_glop", "!" ] "foo${pas_${bar}}baz")
  *)

  (*$>*)
end

(* Views *)

module ListPastRuns =
struct
  type t =
    { name : string ;
      top_run : int ;
      created : float ;
      started : float option ;
      stopped : float option ;
      exit_code : int option ;
      stats_self : RunStats.t ;
      stats_desc : RunStats.t }
    [@@deriving json]

  type ts = t array
    [@@deriving json]

  let array_to_json_string = to_json_string ts_to_json
  let array_to_json_buffer = to_json_buffer ts_to_json
  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]
  let array_of_json_string : string -> t array = [%of_json: t array]
end

module ListPrograms =
struct
  type t =
    { name : string ;
      last_run : int option ;
      last_start : float option ;
      last_stop : float option ;
      last_exit_code : int option }
    [@@deriving json]

  type ts = t array
    [@@deriving json]

  let array_to_json_string = to_json_string ts_to_json
  let array_to_json_buffer = to_json_buffer ts_to_json
  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]
  let array_of_json_string : string -> t array = [%of_json: t array]
end

module ListRunningSequences =
struct
  type t =
    { run : Run.t ;
      exit_codes : int array ;
      step_count : int ;
      all_success : bool }
end

module ListRunningPauses =
struct
  type t =
    { run : Run.t ;
      duration : float ;
      subcommand : int }
end

module ListRunningIfs =
struct
  type t =
    { run : Run.t ;
      condition : int ;
      consequent : int ;
      alternative : int ;
      condition_run : Run.t option ;
      consequent_run : Run.t option ;
      alternative_run : Run.t option }
end

module ListPendingApprovals =
struct
  type t =
    { run : Run.t ;
      time : float option ; (* None if still unconfirmed *)
      autosuccess : bool }
end

module ListPendingLets =
struct
  type t =
    { run : Run.t ;
      subrun : Run.t option ;
      subcommand : int }
end
