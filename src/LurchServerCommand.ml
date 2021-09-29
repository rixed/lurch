open Batteries

open LurchApiTypes
open LurchServerLib

module Api = LurchApiTypes
module CGroup = LurchServerCGroup
module Chroot = LurchServerChroot
module Db = LurchServerDb
module Docker = LurchServerDocker
module Files = LurchServerFiles
module Lang = LurchCommandLanguage

let lurch = ref (Sys.argv.(0))

(*
 * Run and Monitor a Subprocess
 *
 * Commands are unix processes.
 * We fork another process to monitor them and report their output and final
 * status into the DB.
 *)

(* Monitor the given command, write its stats, output logs and exit time into
 * the database and then quit. *)
let do_monitor_run isolation_run cgroup pid stdout_r stderr_r cmd_timeout run_id =
  let open Legacy.Unix in
  (* We record in the DB the fd number the monitored process wrote into, not the
   * one we read from: *)
  let int_of_monitored_fd fd =
    let fd = int_of_fd fd in
    if fd = int_of_fd stdout_r then 1 else
    if fd = int_of_fd stderr_r then 2 else
    invalid_arg ("int_of_monitored_fd for "^ string_of_int fd) in
  log.info "Monitoring fds %d (for its %d) and %d (for its %d)"
    (int_of_fd stdout_r) (int_of_monitored_fd stdout_r)
    (int_of_fd stderr_r) (int_of_monitored_fd stderr_r) ;
  (* We need a buffer per file_descr: *)
  let buffers = Hashtbl.create 5 in
  let get_buffer fd =
    let fd = int_of_fd fd in
    try Hashtbl.find buffers fd
    with Not_found ->
      let buf_ofs = Bytes.create 10_000, 0 in
      Hashtbl.add buffers fd buf_ofs ;
      buf_ofs in
  let save_buffer fd buf ofs =
    let fd = int_of_fd fd in
    Hashtbl.replace buffers fd (buf, ofs) in
  (* fd must be readable. Returns true if the file is not closed *)
  let read_lines fd =
    let buf, ofs = get_buffer fd in
    (* Save the line starting at 0 and ending at ofs: *)
    let save_line ofs =
      if ofs > 0 then (
        let line = Bytes.sub_string buf 0 ofs in
        Db.LogLine.insert run_id (int_of_monitored_fd fd) line
      ) in
    let may_save_lines ofs sz =
      (* While there is a newline in the newly read chars, save and scroll.
       * Return the new offset. *)
      let rec loop last i =
        if i >= last then last else (
          if Bytes.get buf i = '\n' then (
            save_line i ;
            let src = i + 1 in
            let new_len = last - src in
            Bytes.blit buf src buf 0 new_len ;
            loop new_len 0
          ) else loop last (i + 1)
        ) in
      loop (ofs + sz) ofs in
    let truncate () =
      assert (Bytes.length buf > 3) ;
      for i = 1 to 3 do
        Bytes.(set buf (length buf - i) '.')
      done in
    let rec loop ofs =
      let rem = Bytes.length buf - ofs in
      let ofs =
        if rem <= 0 then (
          truncate () ;
          save_line ofs ;
          0
        ) else ofs in
      let sz = read fd buf ofs (Bytes.length buf - ofs) in
      log.debug "Read %d bytes from fd %d (monitored fd %d)"
        sz (int_of_fd fd) (int_of_monitored_fd fd) ;
      if sz = 0 then (
        save_line ofs ;
        false
      ) else (
        let ofs = may_save_lines ofs sz in
        save_buffer fd buf ofs ;
        true
      ) in
    loop ofs in
  let check_termination fds =
    match waitpid [WNOHANG] pid with
    | exception Unix_error (EINTR, _, _) -> ()
    | 0, _ -> ()
    | _, status ->
        log.info "Monitored process %a" print_exit_status status ;
        let exit_status =
          match status with
          | WEXITED s -> s
          | WSIGNALED s -> -s
          | WSTOPPED s -> -s (* Should not be reported to us though *)
        in
        List.iter (ignore % read_lines) fds ;
        (* Collect cgroup data: *)
        let cpu_usr, cpu_sys = CGroup.cpuacct_read cgroup in
        let mem_usr, mem_sys = CGroup.memory_read cgroup in
        Db.Run.stop run_id exit_status
                    ?cpu_usr ?cpu_sys ?mem_usr ?mem_sys ;
        (* Also deletes the cgroup: *)
        CGroup.remove cgroup ;
        Printf.printf "All good, quitting.\n" ;
        exit 0 in
  let check_outputs = function
    | [] -> []
    | fds ->
        let select_timeout = 1. in  (* Do a waitpid once in a while *)
        (match select fds [] [] select_timeout with
        | exception Unix_error (EINTR, _, _) ->
            fds
        | readables, _, _ ->
            List.fold_left (fun fds fd ->
              if List.mem fd readables then
                if read_lines fd then
                  fd :: fds
                else fds
              else fd :: fds
            ) [] fds)
  in
  let rec loop fds =
    check_termination fds ;
    let fds = check_outputs fds in
    loop fds in
  loop [ stdout_r ; stderr_r ]

(* Lookup the isolation level brought by the closest parent of [run]: *)
let rec lookup_isolation run =
  if run.Api.Run.parent_run = run.id then
    (* No parent *)
    None
  else
    let parent = Db.Run.get run.Api.Run.parent_run in
    match parent.Api.Run.command.operation with
    | Isolate _ ->
        if Array.length parent.children = 2 then (
          (* If there is only one children then it means we _are_ the
           * builder: *)
          (* The first child is supposed to be the builder: *)
          Some parent.children.(0)
        ) else (
          (* Checking the above assumption: *)
          assert (Array.length parent.children = 1) ;
          lookup_isolation parent
        )
    | _ ->
        lookup_isolation parent

(* Called just before to execve: *)
let isolate isolation_run args () =
  match isolation_run with
  | None ->
      args,
      (* Take PATH from current environment: *)
      [| "PATH="^ (try Sys.getenv "PATH"
                   with Not_found -> "/usr/bin:/bin") ;
         "USER="^ !user |]
  | Some Api.Run.{
      command = { operation = Chroot { template ; _ } ; _ } ; id } ->
      Chroot.prepare_exec template id args
  | Some Api.Run.{
      command = { operation = Docker { image ; _ } ; _ } ; id } ->
      Docker.prepare_exec image id args
  | _ -> assert false

let start_process run cmd_timeout args =
  assert (Array.length args > 0) ;
  (* Isolation: most of the time, commands must run within a container of
   * some sort (chroot, docker instance...
   * First, lookup the chain of parents to find the isolation mechanism
   * in use: *)
  let isolation_run = lookup_isolation run in
  let log_file =
    "/tmp/lurch/monitor_run."^ string_of_int run.Api.Run.id ^".log" in
  Files.mkdir_all ~is_file:true log_file ;
  Db.close () ; (* Before forking, will be reopened as needed *)
  let pid = clean_fork () in
  if pid = 0 then (
    let open Legacy.Unix in
    setsid () |> ignore ;
    let null = openfile "/dev/null" [O_RDONLY] 0 in
    dup2 null stdin ;
    close null ;
    let out = openfile log_file [O_WRONLY; O_APPEND; O_CREAT] 0o644 in
    dup2 out stdout ;
    dup2 out stderr ;
    close out ;
    log.info "Start and monitor run %d" run.id ;
    try
      (* Pipes to read monitored process output: *)
      let stdout_r, stdout_w = pipe ~cloexec:false () in
      let stderr_r, stderr_w = pipe ~cloexec:false () in
      let cgroup, pid =
        CGroup.exec (isolate isolation_run args) stdin stdout_w stderr_w in
      close stdout_w ;
      close stderr_w ;
      log.info "Started process has pid %d" pid ;
      Db.Run.start run.id ~cgroup ~pid ;
      do_monitor_run isolation_run cgroup pid stdout_r stderr_r cmd_timeout run.id
    with e ->
      let line = "Cannot start subprocess: "^ Printexc.to_string e in
      log.error "%s" line ;
      Db.Run.start run.id ;
      Db.Run.stop run.id 127 ;
      Db.LogLine.insert run.id 2 line
  ) else (
    log.info "Started run %d monitor as pid %d, logs in %s"
      run.id pid log_file
  )

(*
 * This is executed by `lurch step` to actually run the commands.
 * This is typically run by a separate cron and takes what command must be
 * started from the database and write result back into the database.
 *)

(* The timeout to use for every "internal" commands: *)
let command_timeout = ref (Some 600.)

let start_terminal run =
  match run.Api.Run.command.operation with
  | Nop ->
      failwith "Asked to exec the Nop command!"
  | Shell { line ; timeout } ->
      let args = [| "/bin/sh"; "-c"; line |] in
      start_process run timeout args
  | GitClone { url ; revision ; directory } ->
      let revision = revision |? "master"
      and directory = directory |? "clone" in
      (* FIXME: wont work if revision is a hash: *)
      let args =
        (* Leave it to /bin/sh to figure out the paths! *)
        [| "/bin/sh" ; "-c" ;
           "git clone --depth 1 --shallow-submodules --no-tags \
            --branch "^ shell_quote revision ^" "^
            shell_quote url ^" "^ shell_quote directory |] in
      start_process run !command_timeout args
  | Chroot _ | Docker _ ->
      (* Run the creation of the isolation layer as any other
       * command, so it can itself be isolated.
       * Since it runs with start_process, it will be stopped
       * as soon as it ends, and the subcommand will start
       * after that. In other words, its duration and cpu/mem
       * metrics only record the duration of its creation. *)
      let args =
        (* Leave it to /bin/sh to figure out the paths! *)
        [| "/bin/sh" ; "-c" ;
           shell_quote !lurch ^" _exec "^
           (if !is_debug then "--debug " else "") ^
           string_of_int run.id |] in
      start_process run !command_timeout args
  (* Those are not terminals: *)
  | Isolate _
  | Wait _
  | Sequence _
  | Retry _
  | Try _ ->
      invalid_arg "Command.start_terminal: not a terminal"

let finish_run run exit_status =
  let line =
    Printf.sprintf "%s %s."
      (Api.Command.name_of_operation run.Api.Run.command.Api.Command.operation)
      (if exit_status = 0 then "completed" else "failed") in
  Db.LogLine.insert run.id 1 line ;
  Db.Run.stop run.id exit_status

let step_waiting () =
  Db.ListWaitingTerminals.get () |>
  Enum.iter (fun run ->
    log_exceptions (fun () -> start_terminal run))

(* Read the database looking for commands that should proceed: *)
let step_sequences () =
  Db.ListRunningSequences.get () |>
  Enum.iter (fun seq ->
    log_exceptions (fun () ->
      let subcommands =
        match seq.ListRunningSequences.run.command.operation with
        | Sequence { subcommands ; _ } -> Array.of_list subcommands
        | _ -> assert false in
      if not seq.all_success then (
        log.info "Sequence #%d failed." seq.run.id ;
        finish_run seq.run 1
      ) else if seq.step_count >= Array.length subcommands then (
        log.info "Sequence #%d finished." seq.run.id ;
        finish_run seq.run 0
      ) else (
        log.info "Sequence #%d can proceed to step #%d."
          seq.run.id seq.step_count ;
        let line = Printf.sprintf "Executing command #%d of sequence #%d"
          seq.step_count seq.run.command.id in
        if seq.step_count = 0 then Db.Run.start seq.run.id ;
        Db.LogLine.insert seq.run.id 1 line ;
        let run_id =
          Db.Run.insert ~top_run:seq.run.top_run ~parent_run:seq.run.id
                        subcommands.(seq.step_count).Api.Command.id in
        log.debug "Created new run#%d" run_id)))

let finish_as_subrun run subrun =
  match subrun.Api.Run.exit_status with
  | Some exit_status ->
      log.info "Subcommand running as #%d finished with status %d"
        subrun.id exit_status ;
      finish_run run exit_status
  | None ->
      log.info "Must wait for subcommand, still running as #%d" subrun.id

let step_confirmations () =
  Db.ListPendingConfirmations.get () |>
  Enum.iter (fun confirm ->
    log_exceptions (fun () ->
      (* If we have a subrun already, just wait for it to complete: *)
      if confirm.Api.ListPendingConfirmations.run.Api.Run.children <> [||]
      then (
        assert (Array.length confirm.run.children = 1) ;
        finish_as_subrun confirm.run confirm.run.children.(0)
      ) else (
        let subcommand, timeout =
          match confirm.run.command.Command.operation with
          | Wait { subcommand ; timeout } ->
              subcommand, timeout
          | _ -> assert false in
        let unblock exit_status =
          (* Start the subcommand before stopping the wait so this is less of
           * a problem to die here: *)
          let line = Printf.sprintf "%s. Proceeding to subcommand"
            (if exit_status >= 0 then "Confirmed" else "Timing out") in
          Db.LogLine.insert confirm.run.id 1 line ;
          let run_id = Db.Run.insert ~top_run:confirm.run.top_run
                                     ~parent_run:confirm.run.id
                                     subcommand.Api.Command.id in
          log.debug "Created a new run#%d" run_id ;
        in
        match confirm.run.started, timeout, confirm.time with
        | None, _, _ ->
            log.debug "Starting run#%d for wait_confirmation" confirm.run.id ;
            Db.Run.start confirm.run.id
        | Some started, Some timeout, None ->
            let age = Unix.gettimeofday () -. started in
            if age > timeout then (
              log.info "Timing out wait for confirmation #%d after %fs"
                confirm.run.id age ;
              unblock (-1) ;
            ) else (
              log.debug "Waiting longer for the confirmation (timeout in %fs)."
                (timeout -. age)
            )
        | Some _, None, None ->
            (* Just wait *)
            log.debug "Waiting for the confirmation."
        | Some _, _, Some t ->
            log.info "Confirmation #%d received, proceeding to next step."
              confirm.run.id ;
            unblock 0
      )))

(* The isolation commands can take a while so we run them asynchronously
 * and wait for their specific entry in their additional tables to proceed
 * to the subcommand: *)
let step_isolation () =
  Db.ListPendingIsolations.get () |>
  Enum.iter (fun isolate ->
    log_exceptions (fun () ->
      let builder_cmd, subcommand =
        match isolate.Api.Run.command.operation with
        | Isolate { builder ; subcommand } ->
            builder, subcommand
        | _ -> assert false in
      match isolate.Api.Run.children with
      | [| |] ->
        (* No builder yet: start one *)
        let builder_run = Db.Run.insert ~top_run:isolate.top_run
                                        ~parent_run:isolate.id
                                        builder_cmd.Api.Command.id |>
                          Db.Run.get in
        start_terminal builder_run ;
        Db.Run.start isolate.id
      | [| builder |] ->
          (match builder.Api.Run.exit_status with
          | None ->
              (* Wait for the builder to finish before starting the
               * subcommand: *)
              log.debug "Waiting longer for the isolation builder \
                         (run #%d) to complete."
                builder.id
          | Some 0 ->
              let line =
                Printf.sprintf "Isolation builder (run #%d) succeeded."
                  builder.id in
              Db.LogLine.insert isolate.id 1 line ;
              (* Start the subcommand *)
              let run_id = Db.Run.insert ~top_run:isolate.top_run
                                         ~parent_run:isolate.id
                                         subcommand.Api.Command.id in
              log.debug "Created a new run#%d" run_id
          | Some status ->
              let line =
                Printf.sprintf "Isolation builder (run #%d) failed with \
                                status %d."
                  builder.Api.Run.id status in
              Db.LogLine.insert isolate.id 1 line ;
              Db.Run.stop isolate.id status)
      | [| isolation_run ; subrun |] ->
          let stop_with_stats isolate status =
            (* Use the stats of the docker container started by isolation_run
             * as the stats of the subrun: *)
            let cpu_usr, cpu_sys, mem_usr, mem_sys =
              try
                match isolation_run with
                | Api.Run.{
                    command = { operation = Docker _ } ;
                    docker_id = Some docker_id ; _ } ->
                    Docker.read_stats docker_id
                | _ ->
                    raise Exit
              with _ ->
                None, None, None, None in
            Db.Run.stop isolate.Api.Run.id status
                        ?cpu_usr ?cpu_sys ?mem_usr ?mem_sys
          in
          (match subrun.Api.Run.exit_status with
          | None ->
              log.debug "Waiting longer for the isolated subcommand (run #%d) \
                         to complete."
                subrun.id
          | Some 0 ->
              let line =
                Printf.sprintf "Isolated subcommand (run #%d) succeeded."
                  subrun.id in
              Db.LogLine.insert isolate.id 1 line ;
              stop_with_stats isolate 0
          | Some status ->
              let line =
                Printf.sprintf "Isolated subcommand (run #%d) failed \
                                with status %d."
                  subrun.id status in
              Db.LogLine.insert isolate.id 2 line ;
              stop_with_stats isolate status)
      | _ -> assert false))

(* Get the list of old unstarted top runs and delete them if there are too
 * many *)
let expire_old_runs () =
  let one_day = 24. *. 3600. in
  let olds = Db.Run.get_old_unstarted ~older_than:one_day in
  if not (Enum.is_empty olds) then (
    log.warning "Expiring old unstarted runs." ;
    Enum.iter (fun run_id ->
      Db.LogLine.insert run_id 2 "Tool many old runs, expiring" ;
      Db.Run.expire run_id
    ) olds
  )

let step () =
  (* Timeout unstarted top run commands, as there could be tons of old ones if the
   * stepper was not run for long *)
  expire_old_runs () ;
  (* TODO: also timeout running commands *)
  step_sequences () ;
  (* Check if some waits have been confirmed: *)
  step_confirmations () ;
  (* Check if some isolation layer have to be build: *)
  step_isolation () ;
  (* Start all waiting terminals: *)
  step_waiting ()

(* Executes directly the given run. The run is supposed to be an internal
 * command, such as chroot creation. It is monitored already by another
 * process. Both this ont and that one are forked off by `lurch step`. *)
let exec run_id =
  let run = Db.Run.get run_id in
  match run.Api.Run.command.operation with
  | Chroot { template } ->
      Chroot.create run.id template
  | Docker { image } ->
      Docker.create run.id image
  | _ ->
      Printf.sprintf "Cannot exec_ command %s"
        (Lang.string_of_command run.command) |>
      failwith
