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
    | Nop
    | Isolate of
        { builder : t ; subcommand : t }
    | Chroot of
        { template : string }
    | Docker of
        { image : string }
    | Shell of
        { line : string ; timeout : float option }
    | Approve of
        { subcommand : t ; timeout : float option ; comment : string ;
          autosuccess : bool }
    | Sequence of
        { subcommands : t list }
    | Retry of
        { subcommand : t ; up_to : int }
    | Try of
        { subcommand : t ; on_failure : t }
    | Pause of
        { duration : float ; subcommand : t }
    [@@deriving json]

  and t = { id : int ; operation : operation }
    [@@deriving json]

  let rec fold f u cmd =
    match cmd.operation with
    | Nop | Chroot _ | Docker _ | Shell _ ->
        f u cmd
    | Isolate { subcommand }
    | Approve { subcommand }
    | Retry { subcommand }
    | Pause { subcommand } ->
        let u = fold f u subcommand in
        f u cmd
    | Sequence { subcommands } ->
        let u = List.fold_left (fold f) u subcommands in
        f u cmd
    | Try { subcommand ; on_failure } ->
        let u = f (fold f u subcommand) on_failure in
        f u cmd

  let iter f op = fold (fun () -> f) () op

  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]

  let name_of_operation = function
    | Nop -> "nop"
    | Isolate _ -> "isolate"
    | Chroot _ -> "chroot"
    | Docker _ -> "docker"
    | Shell _ -> "shell"
    | Approve _ -> "approve"
    | Sequence _ -> "sequence"
    | Retry _ -> "retry"
    | Try _ -> "try"
    | Pause _ -> "pause"
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
      fd : int ; time : float ; line : string }
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
      (* Children are reified here because it makes displaying runs much
       * easier for the client: *)
      children : t array ;
      (* Initially empty, populated by a specific call "get_logs": *)
      logs : LogLine.t array ;
      (* This depends on the command. They are *not* updated when the
       * server insert a new confirmation etc, but that's OK because they
       * are used on the client only: *)
      confirmation_msg : string option ;
      chroot_path : string option ;
      docker_instance : string option ;
      docker_id : string option }
    [@@deriving json]

  let to_json_string = to_json_string to_json
  let to_json_buffer = to_json_buffer to_json
  let of_json_string : string -> t = [%of_json: t]
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
      cpu_usr : float option ;
      cpu_sys : float option ;
      mem_usr : int option ;
      mem_sys : int option ;
      exit_code : int option }
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

module ListPendingApprovals =
struct
  type t =
    { run : Run.t ;
      time : float option ; (* None if still unconfirmed *)
      message : string ;
      autosuccess : bool }
end
