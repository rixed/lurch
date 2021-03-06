open Batteries

open LurchServerLib

module Api = LurchApiTypes
module CGroup = LurchServerCGroup
module Db = LurchServerDb
module Files = LurchServerFiles
module Secrets = LurchServerSecrets

(* Create chroot, populate them with basic utilities, and wrap command lines so that
 * they run inside it: *)

(* Create and populate a chroot and return its full path *)
let chroot_prefix = ref "/tmp/lurch/chroots"

let busybox =
  ref (
    match Sys.getenv "HOME" with
    | exception Not_found -> "/usr/bin/busybox"
    | home -> home ^"/bin/busybox")

(* Create a new chroot and record its path in the Db.
 * [isolation_id] identifies the command_chroot run. *)
let create isolation_id ?secrets_dir template =
  let path =
    !chroot_prefix ^"/"^
    string_of_int (int_of_float (Unix.time ())) ^"_"^
    string_of_int (Unix.getpid ()) ^"_"^
    random_string () in
  Files.mkdir_all path ;
  system_or_fail
    ("chown "^ shell_quote (!user ^":") ^" "^ shell_quote path) ;
  let dst = path ^"/"^ !Secrets.mount_point in
  Option.may (fun src ->
    bind_mount ~src ~dst
  ) secrets_dir ;
  finally
    (fun () ->
      Option.may (fun src ->
        umount ~src ~dst
      ) secrets_dir)
    (fun () ->
      match template with
      | "busybox" ->
          log.info "Populating chroot %S with busybox from %S" path !busybox ;
          let bin = path ^"/bin" in
          let bin_busybox = bin ^"/busybox" in
          Files.cp !busybox bin_busybox ;
          system_or_fail ("chmod o+x "^ shell_quote bin_busybox) ;
          Unix.open_process_in (bin_busybox ^" --list") |>
          IO.lines_of |>
          Enum.iter (fun applet ->
            log.debug "Populating chroot with %s" applet ;
            system_or_fail ("ln "^ shell_quote bin_busybox ^" "^
                                   shell_quote (bin ^"/"^ applet)))
      | "buster" ->
          todo "copy the buster chroot"
      | _ ->
          invalid_arg ("Chroot.create "^ template)) () ;
  Db.ChrootPath.insert isolation_id path

(* Just umount the secrets dir, keeping the chroot (for now?) *)
let delete isolation_id ?secrets_dir _template =
  Option.may (fun src ->
    let path = Db.ChrootPath.get isolation_id in
    let dst = path ^"/"^ !Secrets.mount_point in
    umount ~src ~dst
  ) secrets_dir

let env_of_template = function
  | "busybox" ->
      [| "PATH=/bin" ;
         "USER="^ !user |]
  | "buster" ->
      [| "PATH=/usr/local/bin:/usr/bin:/bin" ;
         "USER="^ !user |]
  | s ->
      invalid_arg ("Chroot.environment "^ s)

(* Returns no cgroup, and the execve arguments to run the command: *)
let prepare_exec template isolation_id working_dir pathname args env =
  (* Create the cgroup before entering the chroot! *)
  let path = Db.ChrootPath.get isolation_id in
  let open Legacy.Unix in
  (* Save this before chrooting: *)
  let pwd = getpwnam !user in
  (* Now that we do not need the filesystem for anything any longer,
   * chrooting is possible: *)
  log.debug "Chrooting into %S" path ;
  chroot path ;
  chdir "/" ;
  (* Relinquish all privileges before executing user command: *)
  log.debug "Switching to user %S" !user ;
  setgid pwd.pw_gid ;
  setuid pwd.pw_uid ;
  let env = Array.append (env_of_template template) env in
  working_dir, pathname, args, env

let cgroup ?secrets_dir _isolation_id =
  CGroup.make_adhoc ()
