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
         -P -p 9000:9000 \
         -v /var/run/docker.sock:/var/run/docker.sock \
         --rm --entrypoint /bin/sleep "^
         shell_quote image ^" 3600") in
  Db.DockerInstance.insert isolation_id instance docker_id

let prepare_exec _image isolation_id args =
  let instance = Db.DockerInstance.get isolation_id in
  let args = Array.append [| !docker ; "exec" ; instance |] args in
  let env = [||] in
  args, env

let read_stats docker_id =
  let cgroup = "docker/" ^ docker_id in
  log.info "Reading docker stats from cgroup %s" cgroup ;
  let cpu_usr, cpu_sys = CGroup.cpuacct_read cgroup in
  let mem_usr, mem_sys = CGroup.memory_read cgroup in
  cpu_usr, cpu_sys, mem_usr, mem_sys

(* TODO: destroy the container (ie. just `docker stop $instance`)
 * Or maybe just after a while. Same goes for chroot, delete after a while. *)
