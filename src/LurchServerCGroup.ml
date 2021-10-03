(* Utilities to manage cgroups (v1) *)
open Batteries

module Api = LurchApiTypes
module Lang = LurchCommandLanguage

open LurchServerLib

let write_into ~fname s =
  let fd = Unix.openfile fname [O_WRONLY; O_APPEND] 0o644 in
  write_or_fail ~fname fd s ;
  close_or_ignore ~fname fd

let cgroup_uniq_name () =
  Printf.sprintf "lurch/%d.%s" (Unix.getpid ()) (random_string ())

(* Make a command run inside a new accounting cgroup, which name is returned
 * alongside the new command: *)
let wrap cmd =
  let cgroup = cgroup_uniq_name () in
  let cmd =
    Array.append
      [| "systemd-run" ; "--slice="^ cgroup |]
      cmd in
  cgroup, cmd

let controllers = [ "memory" ; "cpuacct" ]

let all_controllers () =
  IO.to_string
    (List.print ~first:"" ~sep:"," ~last:"" String.print) controllers

(* Create a new accounting cgroup and return its name *)
let create () =
  let cgroup = cgroup_uniq_name () in
  let cmd = "cgcreate -a "^ !user ^" -t "^ !user ^
            " -g "^ all_controllers () ^":"^ cgroup in
  system_or_fail cmd ;
  log.info "Created cgroup %S" cgroup ;
  cgroup

let remove cgroup =
  let cmd = "cgdelete "^ all_controllers () ^":"^ cgroup in
  system_or_fail cmd ;
  log.info "Deleted cgroup %S" cgroup

let cgroup_dir controller cgroup =
  "/sys/fs/cgroup/"^ controller ^"/"^ cgroup ^"/"

let cgroup_file controller cgroup file =
  cgroup_dir controller cgroup ^ file

(* Returns both the pid and the cgroup name: *)
let exec isolate its_stdin its_stdout its_stderr =
  let cgroup = create () in
  let pid = clean_fork () in
  if pid = 0 then (
    try
      (* Connect stdin/out/err: *)
      let open Legacy.Unix in
      dup2 its_stdin stdin ;
      dup2 its_stdout stdout ;
      dup2 its_stderr stderr ;
      for i = 3 to 255 do
        try close (fd_of_int i)
        with Unix.(Unix_error (EBADF, _, _)) -> ()
      done ;
      (* Child: Enter the new cgroup before anything else: *)
      let pid = string_of_int (Unix.getpid ()) in
      List.iter (fun controller ->
        let fname = cgroup_file controller cgroup "tasks" in
        write_into ~fname pid
      ) controllers ;
      log.debug "Entered cgroup %s" cgroup ;
      (* From there, the actual isolation technique will dictate what to do
       * before execing the subcommand. *)
      let pathname, args, env = isolate () in
      log.debug "Going to execve %s %a with env %a into cgroup %s"
        pathname
        (Array.print String.print) args
        (Array.print String.print) env
        cgroup ;
      flush_all () ;
      let argv = Array.append [| pathname |] args in
      execve pathname argv env
    with e ->
      Printf.eprintf "Cannot execve: %s\n%!" (Printexc.to_string e) ;
      sys_exit 127
  ) else
    (* Parent: return pid and cgroup *)
    cgroup, pid

let secs_of_ticks t =
  let user_hz = 100. in
  float_of_int t /. user_hz

let cpuacct_read cgroup =
  let fname = cgroup_file "cpuacct" cgroup "cpuacct.stat" in
  let usr = ref None and sys = ref None in
  File.lines_of fname |> Enum.iter (fun line ->
    match String.split_on_char ' ' line with
    | [ "user" ; v ] -> usr := Some (int_of_string v)
    | [ "system" ; v ] -> sys := Some (int_of_string v)
    | _ -> log.warning "Cannot parse %S from %s" line fname) ;
  Option.map secs_of_ticks !usr,
  Option.map secs_of_ticks !sys

let memory_read cgroup =
  let acct_from_file n =
    let fname = cgroup_file "memory" cgroup n in
    try
      File.with_file_in fname (fun ic ->
        Some (IO.read_line ic |> int_of_string))
    with e ->
      log.error "Cannot read memory accounting from %s: %s"
        fname (Printexc.to_string e) ;
      None in
  (try acct_from_file "memory.memsw.max_usage_in_bytes"
  with _ -> acct_from_file "memory.max_usage_in_bytes"),
  acct_from_file "memory.kmem.max_usage_in_bytes"
