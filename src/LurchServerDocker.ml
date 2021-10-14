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
let prepare_exec _image isolation_id pathname args env =
  log.debug "Preparing docker exec" ;
  let instance, _docker_id = Db.DockerInstance.get isolation_id in
  (* [instance] is the friendly name of the container but for finding the
   * effective cgroup we really need the id: *)
  let in_env =
    Array.init (Array.length env * 2) (fun i ->
      if i mod 2 = 0 then "-e" else env.(i/2)) in
  let (++) = Array.append in
  let args = [| "exec" |] ++ in_env ++ [| instance ; pathname |] ++ args in
  !docker, args, [||]

let cgroup isolation_id =
  let _instance, docker_id = Db.DockerInstance.get isolation_id in
  (* Note: alternatively, some system seams to use "docker-$id": *)
  CGroup.make_external ("docker/"^ docker_id)

(* TODO: destroy the container (ie. just `docker stop $instance`)
 * Or maybe just after a while. Same goes for chroot, delete after a while. *)
