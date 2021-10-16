open Batteries

open LurchServerLib

module Api = LurchApiTypes
module CGroup = LurchServerCGroup
module Db = LurchServerDb

let instance_prefix = ref "lurch_"
let docker = ref "/usr/bin/docker"

(* Create and run a new docker instance for that image, and save its instance
 * name. *)
let create isolation_id image =
  let instance =
    !instance_prefix ^
    string_of_int (int_of_float (Unix.time ())) ^"_"^
    string_of_int (Unix.getpid ()) ^"_"^
    random_string () in
  (* TODO: add specific envvars to be set in the container? *)
  (* Automatic deletion after 10h (TODO: add the entrypoint and args in the
   * docker command configuration. *)
  let docker_id =
    command_output
      ("docker run -d --init \
         --name "^ shell_quote instance ^" \
         -P -v /var/run/docker.sock:/var/run/docker.sock \
         --rm --entrypoint /bin/sleep "^
         shell_quote image ^" 7200") in
  Db.DockerInstance.insert isolation_id instance docker_id

(* Returns the cgroup used for this instance, and the execve arguments to run
 * the command: *)
let prepare_exec _image isolation_id working_dir pathname args env =
  log.debug "Preparing docker exec" ;
  let instance, _docker_id = Db.DockerInstance.get isolation_id in
  (* [instance] is the friendly name of the container but for finding the
   * effective cgroup we really need the id: *)
  let in_env =
    Array.init (Array.length env * 2) (fun i ->
      if i mod 2 = 0 then "-e" else env.(i/2)) in
  let working_dir =
    if working_dir = "" then [||] else [| "-w" ; working_dir |] in
  let (++) = Array.append in
  let args = [| "exec" |] ++ in_env ++ working_dir ++
             [| instance ; pathname |] ++ args in
  "", !docker, args, [||]

let cgroup isolation_id =
  let _instance, docker_id = Db.DockerInstance.get isolation_id in
  (* We don't really know where this cgroup is in that case. Let's look for
   * it, since it's supposed to exist already: *)
  let possible_cgroups =
    [ "docker/"^ docker_id ;  (* cgroupfs driver *)
      "system.slice/docker-"^ docker_id ^".scope" (* systemd driver *) ] in
  let possible_mount_point =
    [ !CGroup.mount_point ;
      "/sys/fs/cgroup" ] in
  try
    List.find_map (fun mount_point ->
      try
        Some (
          List.find_map (fun cgroup ->
            (* Look for cgroup v1: *)
            let fname = mount_point ^"/memory/"^ cgroup ^"/tasks" in
            if Sys.file_exists fname then
              Some (CGroup.make_external mount_point 1 cgroup)
            else
              (* Look for cgroup v2: *)
              let fname = mount_point ^"/"^ cgroup ^"/cgroup.procs" in
              if Sys.file_exists fname then
                Some (CGroup.make_external mount_point 2 cgroup)
              else
                None
          ) possible_cgroups)
      with Not_found ->
        None
    ) possible_mount_point
  with Not_found ->
    log.warning "Cannot find docker cgroup location for docker Id %S"
      docker_id ;
    CGroup.make_dummy ()

(* TODO: destroy the container (ie. just `docker stop $instance`)
 * Or maybe just after a while. Same goes for chroot, delete after a while. *)
