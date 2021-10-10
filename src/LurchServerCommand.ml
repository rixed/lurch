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
let do_monitor_run isolation_run cgroup pid stdout_r stderr_r timeout run_id =
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
        let exit_code =
          match status with
          | WEXITED s -> s
          | WSIGNALED s -> to_unix_signal s
          | WSTOPPED s -> to_unix_signal s (* Should not be reported to us though *)
        in
        List.iter (ignore % read_lines) fds ;
        (* Collect cgroup data: *)
        let cpu_usr, cpu_sys = CGroup.cpuacct_read cgroup in
        let mem_usr, mem_sys = CGroup.memory_read cgroup in
        Db.Run.stop run_id exit_code
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
            ) [] fds) in
  let check_cancellation =
    let last_check = ref 0. in
    fun () ->
      (* From time to time, also check in the DB that this process has not been
       * cancelled. If so, kill it. *)
      let delay_before_hard_kill = 30. in
      let check_every = 10. in
      let now = Unix.gettimeofday () in
      if now -. !last_check > check_every then (
        last_check := now ;
        let run = Db.Run.get run_id in
        match run.Api.Run.stopped with
        | Some stopped ->
            let signal, signame =
              if now -. stopped >= delay_before_hard_kill
              then Sys.sigkill, "kill" else Sys.sigterm, "term" in
            log.warning "Signaling pid %d (from cancelled run #%d) with signal %s."
              pid run.id signame ;
            Unix.kill pid signal
        | None ->
            ()
      ) in
  let rec loop fds =
    check_cancellation () ;
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

let default_env =
  (* Take PATH and USER from current environment: *)
  [| "PATH="^ (try Sys.getenv "PATH"
               with Not_found -> "/usr/bin:/bin") ;
     "USER="^ !user |]

(* Called just before to execve: *)
let isolate isolation_run pathname args env () =
  match isolation_run with
  | None ->
      pathname, args, Array.append default_env env
  | Some Api.Run.{
      command = { operation = Chroot { template ; _ } ; _ } ; id } ->
      Chroot.prepare_exec template id pathname args env
  | Some Api.Run.{
      command = { operation = Docker { image ; _ } ; _ } ; id } ->
      Docker.prepare_exec image id pathname args env
  | _ -> assert false

let start_process pathname ~args ?(env=[||]) ?timeout run =
  (* Isolation: most of the time, commands must run within a container of
   * some sort (chroot, docker instance...)
   * First, lookup the chain of parents to find the isolation mechanism
   * in use: *)
  let isolation_run = lookup_isolation run in
  let log_file =
    !log_dir ^"/monitor_run."^ string_of_int run.Api.Run.id ^".log" in
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
        CGroup.exec (isolate isolation_run pathname args env) stdin stdout_w stderr_w in
      close stdout_w ;
      close stderr_w ;
      log.info "Started process has pid %d" pid ;
      Db.Run.start run.id ~cgroup ~pid ;
      do_monitor_run isolation_run cgroup pid stdout_r stderr_r timeout run.id
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

(* Terminals are unstarted commands that need no input from the user or
 * otherwise and therefore can be executed as soon as they are created. *)
let start_terminal run =
  match run.Api.Run.command.operation with
  | Nop { exit_code } ->
      let line = "No-operation, returning "^ string_of_int exit_code in
      Db.LogLine.insert run.id 1 line ;
      Db.Run.start run.id ;
      Db.Run.stop run.id exit_code ;
  | Exec { pathname ; args ; env ; timeout } ->
      let pathname = Api.Run.var_expand run.env pathname
      and args = Array.map (Api.Run.var_expand run.env) args
      and env = Array.map (Api.Run.var_expand run.env) env in
      start_process pathname ~args ~env ?timeout run
  | Chroot _ | Docker _ ->
      (* Run the creation of the isolation layer as any other
       * command, so it can itself be isolated.
       * Since it runs with start_process, it will be stopped
       * as soon as it ends, and the subcommand will start
       * after that. In other words, its duration and cpu/mem
       * metrics only record the duration of its creation. *)
      let args =
        (* Leave it to /bin/sh to figure out the paths! *)
        [| "-c" ;
           shell_quote !lurch ^" _exec "^
           (if !is_debug then "--debug " else "") ^
           string_of_int run.id |] in
      let env =
        [| "BUSYBOX="^ !Chroot.busybox ;
           "LURCH_CHROOTS="^ !Chroot.chroot_prefix ;
           "LURCH_LOG_DIR="^ !log_dir |] in
      start_process "/bin/sh" ~args ~env ~timeout:!command_timeout run
  | Spawn { program } ->
      let program = Api.Run.var_expand run.env program in
      Db.Run.start run.id ;
      let exit_code =
        match Db.Program.get program with
        | exception Failure m ->
            let line = "Cannot spawn program "^ program ^": "^ m in
            Db.LogLine.insert run.id 2 line ;
            1
        | program ->
            let subrun = Db.Run.insert ~creator_run:run.id program.command.id in
            let line = "Spawning program "^ program.name ^" as run #"^
                       string_of_int subrun in
            Db.LogLine.insert run.id 1 line ;
            0 in
      Db.Run.stop run.id exit_code
  | Break { depth } ->
      Db.Run.start run.id ;
      let rec loop run depth =
        if depth >= 0 then (
          log.debug "breaking out of run #%d, parent_run is #%d"
            run.Api.Run.id run.parent_run ;
          if run.parent_run <> run.id then (
            let parent = Db.Run.get run.parent_run in
            Db.LogLine.insert parent.Api.Run.id 1 "Breaking out" ;
            (* Whatever the parent is, terminate it (with success status): *)
            Db.Run.stop parent.id 0 ;
            let depth =
              match parent.command with
              | Api.Command.{ operation = ForLoop _ } -> depth - 1
              | _ -> depth in
            loop parent depth
          ) (* else this command has been stopped already and we can end here *)
        )
      in
      loop run depth ;
      Db.Run.stop run.id 0
  (* Those are not terminals: *)
  | Isolate _
  | Approve _
  | Let _
  | Sequence _
  | Retry _
  | Pause _
  | Wait _
  | If _
  | ForLoop _ ->
      invalid_arg "start_terminal: not a terminal"

let finish_run run exit_code =
  let line =
    Printf.sprintf "%s %s."
      (Api.Command.name_of_operation run.Api.Run.command.Api.Command.operation)
      (if exit_code = 0 then "completed" else "failed") in
  Db.LogLine.insert run.id 1 line ;
  Db.Run.stop run.id exit_code

let step_pauses () =
  let now = Unix.gettimeofday () in
  Db.ListRunningPauses.get () |>
  Enum.iter (fun pause ->
    log_exceptions (fun () ->
      match pause.Api.ListRunningPauses.run.started with
      | Some started ->
          (match pause.subrun with
          | None ->
              if now -. started >= pause.duration then (
                let subrun =
                  Db.Run.insert ~top_run:pause.run.top_run ~parent_run:pause.run.id
                                pause.subcommand in
                let line = "Pause is over, starting subcommand as #"^
                             string_of_int subrun in
                Db.LogLine.insert pause.run.id 1 line)
          | Some Api.Run.{ id ; exit_code = Some exit_code } ->
              let line = "Subcommand #"^ string_of_int id ^" exited" in
              Db.LogLine.insert pause.run.id 1 line ;
              Db.Run.stop pause.run.id exit_code
          | _ ->
              (* Because of list_running_pauses: *)
              assert false)
      | None ->
          let line = "Pause for "^ string_of_float pause.duration ^"s" in
          Db.LogLine.insert pause.run.id 1 line ;
          Db.Run.start pause.run.id))

let step_waits () =
  let tm = Unix.(localtime (time ())) in
  Db.ListRunningWaits.get () |>
  Enum.iter (fun wait ->
    log_exceptions (fun () ->
      if wait.Api.ListRunningWaits.run.Api.Run.started = None then
        Db.Run.start wait.run.id ;
      match wait.subrun with
      | None ->
          let time_match cur exp =
            exp = [] || List.mem cur exp in
          if time_match tm.Unix.tm_min wait.minute &&
             time_match tm.Unix.tm_hour wait.hour &&
             time_match tm.Unix.tm_mday wait.mday &&
             time_match tm.Unix.tm_mon wait.month &&
             time_match tm.Unix.tm_wday wait.wday then (
            let subrun =
              Db.Run.insert ~top_run:wait.run.top_run ~parent_run:wait.run.id
                            wait.subcommand in
            let line = "Reached time specifications, starting subcommand as #"^
                         string_of_int subrun in
            Db.LogLine.insert wait.run.id 1 line)
      | Some Api.Run.{ exit_code = Some exit_code } ->
          let line = "Subcommand finished with exit_code "^
                       string_of_int exit_code in
          Db.LogLine.insert wait.run.id 1 line ;
          Db.Run.stop wait.run.id exit_code
      | Some _ ->
          (* Because of list_pending_lets definition: *)
          assert false))

let step_waiting () =
  Db.ListWaitingTerminals.get () |>
  Enum.iter (fun run ->
    log_exceptions (fun () -> start_terminal run))

let step_conditionals () =
  Db.ListRunningIfs.get () |>
  Enum.iter (fun if_ ->
    log_exceptions (fun () ->
      (* If the condition haven't started, start it: *)
      match if_.Api.ListRunningIfs.condition_run with
      | None ->
          Db.Run.start if_.run.id ;
          let cond_run =
            Db.Run.insert ~top_run:if_.run.top_run ~parent_run:if_.run.id
                          if_.condition in
          let line = "Starting condition as #"^ string_of_int cond_run in
          Db.LogLine.insert if_.run.id 1 line
      | Some cond_run ->
          (match cond_run.Api.Run.exit_code with
          | None ->
              log.debug "Waiting for the end of the condition"
          | Some exit_code ->
              let subcommand, subcommand_run =
                if exit_code = 0 then if_.consequent, if_.consequent_run
                                 else if_.alternative, if_.alternative_run in
              (match subcommand_run with
              | None ->
                  log.info "New run for subcommand %d" subcommand ;
                  let subrun =
                    Db.Run.insert ~top_run:if_.run.top_run ~parent_run:if_.run.id
                                  subcommand in
                  let line = "Starting subcommand as #"^ string_of_int subrun in
                  Db.LogLine.insert if_.run.id 1 line
              | Some subrun ->
                  (match subrun.Api.Run.exit_code with
                  | None ->
                      log.debug "Waiting for the end of the subcommand"
                  | Some exit_code ->
                      let line = "Ending if with exit_code "^ string_of_int exit_code in
                      Db.LogLine.insert if_.run.id 1 line ;
                      Db.Run.stop if_.run.id exit_code)))))

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
        let exit_code =
          try Array.find (fun c -> c <> 0) seq.exit_codes
          with Not_found ->
            log.error "Unsuccessful sequence with no error exit code?" ;
            1 in
        log.info "Sequence #%d failed, exit_code is %d." seq.run.id exit_code ;
        finish_run seq.run exit_code
      ) else if seq.step_count >= Array.length subcommands then (
        log.info "Sequence #%d finished." seq.run.id ;
        finish_run seq.run 0
      ) else (
        log.info "Sequence #%d can proceed to step #%d."
          seq.run.id seq.step_count ;
        let line = Printf.sprintf "Executing command #%d of sequence #%d"
          seq.step_count seq.run.command.id in
        Db.LogLine.insert seq.run.id 1 line ;
        if seq.step_count = 0 then Db.Run.start seq.run.id ;
        let run_id =
          Db.Run.insert ~top_run:seq.run.top_run ~parent_run:seq.run.id
                        subcommands.(seq.step_count).Api.Command.id in
        log.debug "Created new run #%d" run_id)))

(* For each ongoing for loop, get the list of past subcommand runs so we know
 * when to end and what value the variable should hold: *)
let step_for_loops () =
  Db.ListRunningForLoops.get () |>
  Enum.iter (fun loop ->
    log_exceptions (fun () ->
      if loop.Api.ListRunningForLoops.run.Api.Run.started = None then
        Db.Run.start loop.run.id ;
      let i = Array.length loop.exit_codes in
      (match loop.run.command.Api.Command.operation with
      | ForLoop { var_name ; values ; subcommand } ->
          if i > 0 && loop.exit_codes.(i-1) <> 0 then (
            let last_exit_code = loop.exit_codes.(i-1) in
            let line =
              "Exiting loop with exit code "^ string_of_int last_exit_code in
            Db.LogLine.insert loop.run.id 1 line ;
            Db.Run.stop loop.run.id last_exit_code
          ) else if i >= Array.length values then (
            let line = "Finished loop" in
            Db.LogLine.insert loop.run.id 1 line ;
            Db.Run.stop loop.run.id 0
          ) else (
            (* Start the next iteration (which will make this run disappear
             * from the list_running_for_loops view: *)
            let var_value = Api.Run.var_expand loop.run.env values.(i) in
            let line =
              Printf.sprintf "Starting iteration #%d of for loop (%s=%S)"
                (i+1) var_name var_value in
            Db.LogLine.insert loop.run.id 1 line ;
            let run_id =
              Db.Run.insert ~top_run:loop.run.top_run ~parent_run:loop.run.id
                            subcommand.id in
            log.debug "Created a new run #%d" run_id
          )
      | _ ->
          assert false)))

let finish_as_subrun run subrun =
  match subrun.Api.Run.exit_code with
  | Some exit_code ->
      log.info "Subcommand running as #%d finished with status %d"
        subrun.id exit_code ;
      finish_run run exit_code
  | None ->
      log.info "Must wait for subcommand, still running as #%d" subrun.id

let step_approvals () =
  Db.ListPendingApprovals.get () |>
  Enum.iter (fun approve ->
    log_exceptions (fun () ->
      (* If we have a subrun already, just wait for it to complete: *)
      if approve.Api.ListPendingApprovals.run.Api.Run.children <> [||]
      then (
        assert (Array.length approve.run.children = 1) ;
        finish_as_subrun approve.run approve.run.children.(0)
      ) else (
        let subcommand, timeout =
          match approve.run.command.Command.operation with
          | Approve { subcommand ; timeout } ->
              subcommand, timeout
          | _ -> assert false in
        let unblock timed_out =
          let proceed = not timed_out || approve.autosuccess in
          (* Start the subcommand before stopping the wait so this is less of
           * a problem to die here: *)
          let line = Printf.sprintf "%s. %s subcommand"
            (if timed_out then "Timing out" else "Confirmed")
            (if proceed then "Proceeding to" else "Cancelling") in
          Db.LogLine.insert approve.run.id 1 line ;
          if proceed then (
            let run_id = Db.Run.insert ~top_run:approve.run.top_run
                                       ~parent_run:approve.run.id
                                       subcommand.Api.Command.id in
            log.debug "Created a new run #%d" run_id
          ) else (
            Db.Run.stop approve.run.id Api.ExitStatus.timed_out
          ) in
        match approve.run.started, timeout, approve.time with
        | None, _, _ ->
            log.debug "Starting run #%d for wait_confirmation" approve.run.id ;
            Db.Run.start approve.run.id
        | Some started, Some timeout, None ->
            let age = Unix.gettimeofday () -. started in
            if age > timeout then (
              log.info "Timing out wait for confirmation #%d after %fs"
                approve.run.id age ;
              unblock true
            ) else (
              log.debug "Waiting longer for the confirmation (timeout in %fs)."
                (timeout -. age)
            )
        | Some _, None, None ->
            (* Just wait *)
            log.debug "Waiting for the confirmation."
        | Some _, _, Some t ->
            log.info "Confirmation #%d received, proceeding to next step."
              approve.run.id ;
            unblock false
      )))

let step_lets () =
  Db.ListPendingLets.get () |>
  Enum.iter (fun Api.ListPendingLets.{ run ; subrun ; subcommand } ->
    log_exceptions (fun () ->
      match subrun with
      | None ->
          Db.Run.start run.id ;
          (* No builder yet: start one *)
          let subrun = Db.Run.insert ~top_run:run.top_run
                                     ~parent_run:run.id
                                     subcommand in
          let line =
            "Starting let-binding subcommand as #"^ string_of_int subrun in
          Db.LogLine.insert run.id 1 line
      | Some Api.Run.{ exit_code = Some exit_code } ->
          let line =
            "Stopping let-binding with exit code "^ string_of_int exit_code in
          Db.LogLine.insert run.id 1 line ;
          Db.Run.stop run.id exit_code
      | Some _ ->
          (* Because of list_pending_lets definition: *)
          assert false))

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
          (match builder.Api.Run.exit_code with
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
              log.debug "Created a new run #%d" run_id
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
          (match subrun.Api.Run.exit_code with
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

(* If a run has been cancelled then all it's children must also be cancelled. *)
let propagate_cancellations () =
  Db.ListObsoleteRuns.get () |>
  Enum.iter (fun run_id ->
    log.warning "Propagate cancellation to run #%d" run_id ;
    Db.Run.cancel run_id)

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
  (* Propagate run cancellation down the command tree, down to killing started
   * processes: *)
  propagate_cancellations () ;
  (* TODO: also timeout running commands *)
  step_sequences () ;
  (* Execute conditionals: *)
  step_conditionals () ;
  (* Check if some waits have been confirmed: *)
  step_approvals () ;
  (* Implement the let bindings: *)
  step_lets () ;
  (* Check if some isolation layer have to be build: *)
  step_isolation () ;
  (* Check if some pauses are over: *)
  step_pauses () ;
  (* Check is some waits are over: *)
  step_waits () ;
  (* Handle for loops *)
  step_for_loops () ;
  (* Start all waiting terminals: *)
  step_waiting ()

(* Executes directly the given run. The run is supposed to be an internal
 * command, such as chroot creation. It is monitored already by another
 * process. Both this ont and that one are forked off by `lurch step`. *)
let exec run_id =
  let run = Db.Run.get run_id in
  match run.Api.Run.command.operation with
  | Chroot { template } ->
      let template = Api.Run.var_expand run.env template in
      Chroot.create run.id template
  | Docker { image } ->
      let image = Api.Run.var_expand run.env image in
      Docker.create run.id image
  | _ ->
      Printf.sprintf "Cannot exec_ command %s"
        (Lang.string_of_command run.command) |>
      failwith
