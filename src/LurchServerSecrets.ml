(* Where to find secrets on the host: *)
let basedir = ref "/var/lib/lurch/secrets"

(* Where to bind-mount the secrets in the guest: *)
let mount_point = ref "/run/secrets"


