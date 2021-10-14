(* Utilities to manage cgroups (v1) *)
open Batteries

module Api = LurchApiTypes

open LurchServerLib

let version = ref 1
let mount_point = ref "/sys/fs/cgroup"

let write_into ~fname s =
  let fd = Unix.openfile fname [O_WRONLY; O_APPEND] 0o644 in
  write_or_fail ~fname fd s ;
  close_or_ignore ~fname fd

let cgroup_uniq_name () =
  Printf.sprintf "lurch/%d.%s" (Unix.getpid ()) (random_string ())

let controllers = [ "memory" ; "cpuacct" ]

let all_controllers () =
  IO.to_string
    (List.print ~first:"" ~sep:"," ~last:"" String.print) controllers

(* Create a new accounting cgroup and return its name *)
let create () =
  let cgroup = cgroup_uniq_name () in
  let cmd =
    if !version = 1 then
      "cgcreate -a "^ !user ^" -t "^ !user ^
      " -g "^ all_controllers () ^":"^ cgroup
    else
      (* In theory we should create a subtree 'lurch' first, enable cpu and
       * memory controlers in cgroup.subtree_control there, then create a
       * subtree per cgroup. But parent cgroup wil already have those
       * controler enabled so... *)
      let p = !mount_point ^"/"^ cgroup in
      "mkdir -p "^ p ^" && \
       chown "^ !user ^" "^ p in
  system_or_fail cmd ;
  log.info "Created cgroup %S" cgroup ;
  cgroup

let remove cgroup =
  let cmd =
    if !version = 1 then
      "cgdelete "^ all_controllers () ^":"^ cgroup
    else
      let p = !mount_point ^"/"^ cgroup in
      "rmdir "^ p in
  system_or_fail cmd ;
  log.info "Deleted cgroup %S" cgroup

let cgroup_dir controller cgroup =
  assert (!version = 1) ;
  "/sys/fs/cgroup/"^ controller ^"/"^ cgroup ^"/"

let cgroup_file controller cgroup file =
  assert (!version = 1) ;
  cgroup_dir controller cgroup ^ file

let cgroup2_file cgroup fname =
  assert (!version = 2) ;
  !mount_point ^"/"^ cgroup ^"/"^ fname

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
      if !version = 1 then (
        List.iter (fun controller ->
          let fname = cgroup_file controller cgroup "tasks" in
          write_into ~fname pid
        ) controllers
      ) else (
        let fname = cgroup2_file cgroup "cgroup.procs" in
        write_into ~fname pid
      ) ;
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

let secs_of_usec t =
  float_of_int t /. 1_000_000.

let cpuacct_read cgroup =
  let get_cpu_stats fname n_usr n_sys to_secs =
    let usr = ref None and sys = ref None in
    File.lines_of fname |> Enum.iter (fun line ->
      match String.split_on_char ' ' line with
      | [ n ; v ] when n = n_usr -> usr := Some (int_of_string v)
      | [ n ; v ] when n = n_sys -> sys := Some (int_of_string v)
      | _ -> log.warning "Cannot parse %S from %s" line fname) ;
    Option.map to_secs !usr,
    Option.map to_secs !sys
  in
  if !version = 1 then (
    let fname = cgroup_file "cpuacct" cgroup "cpuacct.stat" in
    get_cpu_stats fname "user" "system" secs_of_ticks
  ) else (
    let fname = cgroup2_file cgroup "cpu.stat" in
    get_cpu_stats fname "user_usec" "system_usec" secs_of_usec
  )

let memory_read cgroup =
  if !version = 1 then (
    let acct_from_file n =
      let fname = cgroup_file "memory" cgroup n in
      try
        File.with_file_in fname (fun ic ->
          Some (IO.read_line ic |> Int64.of_string))
      with e ->
        log.error "Cannot read memory accounting from %s: %s"
          fname (Printexc.to_string e) ;
        None in
    let usr = acct_from_file "memory.memsw.max_usage_in_bytes" in
    let usr =
      if usr <> None then usr else acct_from_file "memory.max_usage_in_bytes" in
    usr,
    acct_from_file "memory.kmem.max_usage_in_bytes"
  ) else (
    (* Try to distinguish between user and system: *)
    let is_usr = function
      | "anon" | "anon_thp" -> true
      | _ -> false
    and is_sys = function
      | "file" | "kernel_stack" | "pagetables" | "percpu" | "sock" | "shmem"
      | "file_mapped" | "file_dirty" | "file_writeback" | "swapcached"
      | "file_thp" | "shmem_thp" | "inactive_anon" | "active_anon"
      | "inactive_file" | "active_file" | "unevictable" | "slab" -> true
      | _ -> false in
    try
      let fname = cgroup2_file cgroup "memory.stat" in
      let usr, sys =
        File.lines_of fname |> Enum.fold (fun (usr, sys as prev) line ->
          match String.split_on_char ' ' line with
          | [ n ; v ] ->
              let v = try Int64.of_string v with _ -> 0L in
              if is_usr n then
                Int64.add usr v, sys
              else if is_sys n then
                usr, Int64.add sys v
              else
                prev
          | _ ->
              prev
        ) (0L, 0L) in
      Some usr, Some sys
    with _ -> None, None
  )
