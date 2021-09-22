# You rang?

A simple open source tool to build automation suitable for automatic tests, deployments, wtv.


## Requirements

1. Run test suite (or any shell command, really) and report status and logs

2. Every action and result must be logged in postgresql

3. Some actions might provide additional data (such as change authors) that must also be saved (in an additional key-value table?)

4. Workflow: must be possible to define an automaton in the conf, and have automatons run in a loop or on demand

5. In any case, any started automation can be stopped at any step (every step has a delay between read-to-start to actually started, stopping implies this delay is infinite). An example of a simple step is "manual acknowledgement".

6. Each automaton steps are a module with an implementation, an output, a name, an identifier to fetch configuration from the DB, and a GUI that implement it at various stages:
   - unconfigured: when it offers to configure (must then save the conf and register itself as a step with a name, plugin name, and conf identifier in exchange for an automaton- step identifier)
   - waiting: part of an automaton that is not running yet (the step is not but maybe the automaton is)
   - running: can then be cancelled

7. DB runs separately


## Technical choices

. postgresql as a DB

. postgrest would have been nice but too many deps

. quick ocaml similar implementation of server/client can also helps with sharing data between the server and the SPA

. js_of_ocaml + a CGI able external HTTP server that also perform authn

. no capability for now. Later use TLS client certs?


## Build

### Requirements

1. Compile yourself a **statically linked** [busybox](https://www.busybox.net/) and store it in your `$HOME/bin`
(Note: look for the `Build static binary` option under `Build options` under the `Settings` option of `make menuconfig`).
It will be used for the chroot containment strategy.

2. cgcreate/cgdelete tools (from Debian package cgroup-tools), and a cgroup lurch given to the user:
    cgcreate -a $user -t $user -g cpuacct,memory:lurch

3. dockerd running on the host with ideally no other VMs (users will have access to them) for the docker containment strategy.

4. an HTTP server able to run CGI scripts (recommended: lighttpd, see a possible configuration in examples/lighttpd.conf)

### Build commands

`./configure && make`


## Run

### The database

Have a look at `db/create` to check/change the settings and then run it (ignore any NOTICE from psql).

### The lurch daemon

`while sudo www/lurch step --debug ; do sleep 10 ; done`

### The WWW CGI interface

Here with lighttpd:

`cd www && lighttpd -f ../examples/lighttpd.conf -D`

You should then be able to access it on port 8081.

It is now a good idea to try to run the three sample programs `test container`, `test chroot` and `test build`.

### Want to run a program regularly?

`while sleep 14400; do www/lurch start 'build+test ramen'; done`

## Notes

Commands must be jailed but yet we do not want to have a real language ; we don't want graphical shell scripts!
So each "command" must be high level enough that's it's easy to manipulate and understand them. So the initial environment must be rich enough. For instance, busybox isolation must
come with all additional tools required to install build environments, test and packages. Other use cases imply the development of new high level modules.
We want to redo the loop-builder, not docker.
So for instance, the program we want to start with do this:

docker pull ramen-dev:latest
docker run --rm ramen-dev:latest check

and that's it.

We want another one that *build* the image, as in `make docker`. This requires to be able to run docker from the chroot. Or, at first, from the server.
In that case, isolation is "none". But still we want to control the environment perfectly for the build to be reproductible and not be dependent on the debian exact version, the opam environment etc, which is hard to achieve on the host target (unless the machine is frozen). But we can have a separate debian chroot, which is easy from any debian host. Let's try to use docker from this chroot. We could then even copy this chroot into another host.
We should be able to reach the host dockerd from the chroot. For this the daemon should listen to more than a unix domain socket. Or just bind mount /var/run into the chroot. And then it works.

Also, redesign the schema to exclude the chroot from runs and isolation from program.
Instead, have dedicated commands to create a chroot, and have them as top-level commands (even easier to reach than the program) and get the isolation type from there, and the actual
details in a table associated with each type of isolation (path to chroot, basically).
Then every command can be wrapped properly. Also, the container is supposed to stay
"up" (ie. uncompressed and with all mounts) for the whole duration of the program, and
then is unmounted (later: have an overlayfs on top of it, and archive the overlay? Or just use
docker which does exactly this. An advantage is that it allows the container to be created
asynchronously, as any running command. The only downside is that isolation within isolation
might not work. But this could be checked at program creation time. Although starting with a seq of 'isolate' commands also make sense. Therefore the isolation command must really be
propagated down the tree of commands (or looked up following the chain of parents).

So, we want an isolation level "docker" of some image, with the config being
the container name. Every command will then be prefixed with 'docker exec -ti
$name'. And from there we can do everything else. To be able to build a docker
image for a program from within such a docker isolation scheme, remember to:
add `-v /var/run/docker.sock:/var/run/docker.sock` so the docker client in
docker image can talk to the docker server in the host.
