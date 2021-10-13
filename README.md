# You rang?

This tool is for solo devs or very small teams willing to automate builds/tests/deployments or anything really,that requires keeping track of past results and occasionally need a human in the loop to validate or give instructions.

Lurch is a way to execute commands in a controlled fashion.
For each command, CPU and RAM resource usage as well as all output are saved in a database.
Apart from executing external commands, the language has a few simple control flow structure, some of which allow to set variables or to wait for user approval before proceeding.

This comes handy to build, test and deploy software, but could be useful for other of your devops need.

User interactions, as well as exploring logs and statistics, happen in a simple web single-page-application.

This is open source, implemented using OCaml and Postgresql, and runs on Linux (it makes use of either chroot or docker to isolate commands and cgroup to measure their resource consumptions).

## Trying it out from the command line

Let's test this tool using the docker image (`rixed/lurch`) and using only the command line at first.

Start by running the docker image. It will need either to access your docker daemon and/or your cgroup (v1 or v2) file system.

Assuming you have the `dockerd` running locally, to make it possible for the lurch instance to access it, it is enough to just mount the dockerd unix socket in the container, with the option `-v /var/run/docker.sock:/var/run/docker.sock`.

For cgroup, let's assume in this test that you are using cgroup v2 (the default on Debian stable) and that the cgroup file system is mounted in `/sys/fs/cgroup` (also the default on Debian stable). Lurch will create cgroups named `lurch/...XYZ...` so the subtree `lurch` need to exist and the cpu and memory controllers need to be activated for this subtree:

```shell
$ sudo mkdir /sys/fs/cgroup/lurch
$ sudo sh -c 'echo +cpu +memory > /sys/fs/cgroup/lurch/cgroup.subtree_control'
```

Lurch itself will just use those controllers to gather stats, but you are free to configure further restrictions if you wish.

You will then have to mount this cgroup subtree in the lurch container and indicate it's location (as well as the cgroup version to use), with the additional options:
`-v /sys/fs/cgroup/lurch:/lurch/cgroup/lurch -e CGROUP_VERSION=2 -e CGROUP_MOUNT_POINT=/lurch/cgroup`.

Finally, we will also need `-P` to use the HTTP GUI later-on, so that the full docker command line is:

```shell
$ docker run -v /var/run/docker.sock:/var/run/docker.sock \
             -v /sys/fs/cgroup/lurch:/lurch/cgroup/lurch \
             -e CGROUP_VERSION=2 -e CGROUP_MOUNT_POINT=/lurch/cgroup \
             --name lurch-demo -P --detach rixed/lurch
```

This will create and run the Postgresql database, the HTTP service for the GUI and the lurch command executor.
Now we are ready to give it some work to do.

There are really only three simple concepts to grasps: _programs_, which are composed of _commands_, and _runs_, which are program current or past execution.

The database come populated with a few programs that you can list with the `programs` subcommand:

```shell
$ docker exec lurch-demo lurch programs | column -t -s $'\t'
# name          last run  last start  last stop  last exit code
test build      n.a.      n.a.        n.a.       n.a.
test chroot     n.a.      n.a.        n.a.       n.a.
test container  n.a.      n.a.        n.a.       n.a.
```

The command composing a program can be displayed as a compact s-expression with the `export` subcommand:

```shell
$ docker exec lurch-demo lurch export 'test container'
(isolate
  (docker "rixed/lurch-docker-in-docker")
  (exec "/usr/bin/docker"
    ("ps")
    () null))
```

Which translates into: within the docker image "rixed/lurch-docker-in-docker", execute the binary "/usr/bin/docker" with the single argument "ps", an empty environment and no timeout.

That's a lot of docker instances, so let's look closer:
if we run this program, the lurch executor running within the `lurch-demo` container will first use the `docker` command (that's installed as part of the `rixed/lurch` image) to communicate with you local docker daemon to have it download the `rixed/lurch-docker-in-docker` image (should be instant, given it's the base image for `rixed/lurch`), and then execute `/usr/bin/docker ps` from that image, which will again connect to your local docker daemon and display the list of instances running on your local host, thus proving that the docker daemon can be accessed from within the docker container.

Similarly here is the 'test chroot' program:

```shell
$ docker exec lurch-demo lurch export 'test chroot'
(isolate
  (chroot "busybox")
  (exec "/bin/ls"
    ("/bin")
    () null))
```

This time, instead of using a docker image this program starts by creating a new chroot environment, populating it with busybox, and check the presence of the busybox tools by executing `/bin/ls /bin`.

Those programs are very basic but make sure the basic assumptions to run more complex programs are met.

The third and last demo program is marginally more useful, as it download, compile and test lurch itself:

```shell
$ docker exec lurch-demo lurch export 'test build'
...TODO...
```

This program checks that lurch itself can be build on Debian stable and that the test suite succeeds.

Let's write another very simple program and load it (notice the `-i` option to docker, without which lurch won't be given an stdin!) :

```shell
$ docker exec -i lurch-demo lurch import hello << EOF
(isolate
  (chroot "busybox")
  (sequence
    (exec "/bin/sleep" ("10") () null)
    (exec "/bin/echo" ("hello world!") () null)))
EOF
```

(Don't worry if you can't quite grok the s-expressions. The web GUI offers a simpler way to read and edit programs.)

Let's now run that program:

```shell
$ docker exec lurch-demo lurch start hello
Program hello started as run #1.
```

About 10 seconds later, that run should have ended:

```shell
$ docker exec lurch-demo lurch runs hello | column -t -s $'\t'
# run  created              started              stopped              exit code  cpu usr  sys       ram usr    sys
1      2021-10-13T17:37:14  2021-10-13T17:37:15  2021-10-13T17:37:29  0          0.430348  2.430358  1MiB32KiB  16MiB527KiB592.
```

The logs for that run can be obtained as well:

```shell
$ docker exec lurch-demo lurch logs 1
# run   time
2       2021-10-13T17:37:15     Populating chroot "/tmp/lurch/chroots/1634146635_85_YlGxbWQC" with busybox from "/usr/local/bin/busybox"
1       2021-10-13T17:37:17     Isolation builder (run #2) succeeded.
3       2021-10-13T17:37:18     Executing command #0 of sequence #15
3       2021-10-13T17:37:28     Executing command #1 of sequence #15
5       2021-10-13T17:37:28     hello world!
3       2021-10-13T17:37:29     sequence completed.
1       2021-10-13T17:37:29     Isolated subcommand (run #3) succeeded.
```

The first column indicate the run identifier of the command outputting that line.
Indeed, `1` was the run number of the outermost command composing the _hello_ program, but as most commands are composed of subcommands, and each subcommands are run (and monitored) independently, then each is also assigned a unique run identifier.
For instance, the run number 2 was the `(chroot "busybox")` and the run number 5 was the `echo`. We can drill down into individual runs:

```shell
$ docker exec lurch-demo lurch logs 5
# run   time
5       2021-10-13T17:37:28     hello world!
```

You should now be familiar enough with the basic concepts, which are really quite trivial.

## The Command Language

Let's review the few commands and keywords available, still in s-expression syntax.

### Values

`null` can be used in a few place where parameters are optional, such as the timeout of an `exec` command.

Strings are double quoted.

Booleans are the symbols `t` and `f`.

### (no-op n)

Does nothing, has an exit code of _n_.

### (isolate container-builder subcommand)

Build some sort of container, then runs the subcommand in it.
It's exit code is that of the subcommand, unless the container failed to build.

The only valid container-builders are `chroot` and `docker`.

The `exec` command is invalid unless within an `isolate` subcommand.

### (chroot "template")

Built a chroot and populate it with the given template.

The only valid valid template is for now the string "busybox", which installs busybox in the chroot.

The isolated command will be run in that chroot.

### (docker "image")

Pull an image and runs it, assuming there is a `/bin/sleep` command in it.

The isolated command will be run in that container.

### (exec "path" ("arg1" "arg2"...) ("VAR1=VAL1" "VAR2=VAL2"...) timeout)

Execute the given path with the given arguments and environment.
The optional timeout is a floating point in seconds, or `null`.

Timed-out command exits with a specific exit code.

### (approve timeout "comment" default)

Wait for the user's approval. The confirmation dialog in the GUI will display the comment,
and will optionally timeout, defaulting to proceeding ('t') or cancelling ('f')
the execution.

### (let var-name "default" "comment" body)

Define a variable with the given name and default value.
The actual value will have to be entered by the user in the GUI, which will display the comment.

Within the body (any subcommand), all occurrences of "${var-name}" in the strings (for example, in the arguments for an `exec` command) will be replaced by the value.

### (sequence subcommand1 subcommand2...)

Executes the subcommands in order, one after the other, unless one of them has a non zero exit code.

The exit-code of the sequence is that of the last executed subcommand.

### (retry subcommand n)

Retry the given subcommand up to n times (an integer), or until it exits with 0.

### (if condition then else)

If the _condition_ exits with 0 (success) then proceed with executing the _then_ subcommand ; otherwise proceed with the _else_ subcommand.

### (pause duration)

Pause program execution for the many seconds.

### (wait (min1 min2...) (hour1 hour2...) (month-day1 month-day2...) (month1 month2...) (week-day1 week-day2..))

Pause the execution of the program until the current date and hour match the specification (à la crontab).

### (spawn "program")

Start the program which name is specified (as a string).

There is a rate limit controlling the number of started programs per minute to protect against "shell bombs".

### (for var-name (v1 v2 v3...) body)

Execute the body once for each specified value attached to a variable with the given name.

### (break n)

Break out of the latest n loops.
Can also break out of the current program if not that many loops are currently being executed.


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

2. For cgroup v1: cgcreate/cgdelete tools (from Debian package cgroup-tools), and a cgroup lurch given to the user:
    cgcreate -a $user -t $user -g cpuacct,memory:lurch

   For cgroup v2 a read-write top level cgroup named lurch, with cpu and memory controlers enabled:
   sudo mkdir /sys/fs/cgroup/lurch
   sudo sh -c 'echo +cpu +memory >> /sys/fs/cgroup/lurch/cgroup.subtree_control'

3. dockerd running on the host with ideally no other VMs (users will have access to them) for the docker containment strategy.

4. an HTTP server able to run CGI scripts (recommended: lighttpd, see a possible configuration in examples/lighttpd.conf)

### Build commands

`./configure && make`


## Run

### Using the docker image

If your host operating system is using cgroup2:

```shell
$ docker pull rixed/lurch
$ docker run -v /var/run/docker.sock:/var/run/docker.sock \
             -v /sys/fs/cgroup/lurch:/lurch/cgroup/lurch \
             -e CGROUP_VERSION=2 -e CGROUP_MOUNT_POINT=/lurch/cgroup \
             -P rixed/lurch
```

If it's using legacy cgroup:

```shell
$ docker pull rixed/lurch
$ docker run -v /var/run/docker.sock:/var/run/docker.sock \
             -P rixed/lurch
```

Log-in with user `demo`, password `demo`.

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
