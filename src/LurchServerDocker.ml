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

(* FIXME: we actually cannot make use of pathname since the whole arguments have
 * to be passed to docker, which does the execve. So probably Exec is too acurate
 * for what we do and we should make it so that args[0] is "automatic". *)
let prepare_exec _image isolation_id _pathname args env =
  let instance = Db.DockerInstance.get isolation_id in
  let in_env =
    Array.init (Array.length env * 2) (fun i ->
      if i mod 2 = 0 then "-e" else env.(i/2)) in
  let (++) = Array.append in
  let args = [| !docker ; "exec" |] ++ in_env ++ [| instance |] ++ args in
  !docker, args, [||]

let read_stats docker_id =
  let cgroup = "docker/" ^ docker_id in
  log.info "Reading docker stats from cgroup %s" cgroup ;
  let cpu_usr, cpu_sys = CGroup.cpuacct_read cgroup in
  let mem_usr, mem_sys = CGroup.memory_read cgroup in
  cpu_usr, cpu_sys, mem_usr, mem_sys

(* TODO: destroy the container (ie. just `docker stop $instance`)
 * Or maybe just after a while. Same goes for chroot, delete after a while. *)
