# You rang?

This tool is for solo devs or very small teams willing to automate builds/tests/deployments or anything really, that requires keeping track of past results and occasionally need a human in the loop to validate or give instructions.

Lurch is a way to execute commands in a controlled fashion.
For each command, CPU and RAM resource usage as well as all output are saved in a database.
Apart from executing external commands, the language has a few simple control flow structure, some of which allow to set variables or to wait for user approval before proceeding.

This comes handy to build, test and deploy software, but could be useful for other of your devops need.

User interactions, as well as exploring logs and statistics, can happen either on the command line or in a simple web GUI.

This is open source, easy to self-host, and is implemented using OCaml and Postgresql.
It runs on Linux and makes use of either chroot or docker to isolate commands and cgroup to measure their resource consumptions.

## Why another CI/CD tool?

Existing CI/CD tools that I know about are designed for fully automatic processes but do not make it easy to include user feedback in the loop.
Also, they are focussed on building/delivering software - for instance, they trigger on commits.
But in many occasions I needed a piece of plumbing that I could either trigger by hand or periodically or even when some complex combination of events occurred, independently of any git repository.
Also, most of the popular CI/CD tools are impossible to self host.

The inspiration came from a tool that was used at Google for performing software roll-outs in a controlled fashion.

As I remember it, every week the tool would build the software, run the test suites and if all was green would proceed to roll it out onto the canary servers.

It would then wait for human approval, which would normally come only after a few days after having checked the monitoring and investigated user complains.
Once the SRE in charge of the roll-out had given its approval, the tool would then extend the roll-out to a small random set of datacenters, where again the new version would bake a few days before another approval enabled the complete roll-out to all datacenters, and the cycle would start again.

At any step humans could cancel the roll-out and maybe start the revert process.

All logs were easily available from within that tool to check what each step was doing.

As I remember it the tool was implemented in python and, although it was not easy to customize because of its javaesque conception, it was nonetheless rather pleasing to use.
I somewhat believed at the time that that tool was actually not internal and open source but I never managed to find it when I looked for it later on for my own needs.
After many years of frustration I decided to implement my own version of it, simpler, focussing on what matters to me.

(If you know of the python tool described above, or of any other tool in this domain that I could have used instead, please let me know!)

## Trying it out from the command line

Let's test this new tool using the docker image (`rixed/lurch`) and using only the command line at first.

Start by running the docker image. It will need either to access your docker daemon and/or your cgroup (v1 or v2) file system.

Assuming you have the `dockerd` running locally, to make it possible for the lurch instance to access it, it is enough to just mount the dockerd unix socket in the container, with the option `-v /var/run/docker.sock:/var/run/docker.sock`.

For cgroup, let's assume in this test that you are using cgroup v2 (the default on Debian stable) and that the cgroup file system is mounted in `/sys/fs/cgroup` (also the default on Debian stable). Lurch will create cgroups named `lurch/...XYZ...` so the subtree `lurch` needs to exist beforehand and the cpu and memory controllers need to be activated for this subtree:

```shell
% sudo mkdir /sys/fs/cgroup/lurch
% sudo sh -c 'echo +cpu +memory > /sys/fs/cgroup/lurch/cgroup.subtree_control'
```

Lurch itself will just use those controllers to gather stats, but you are free to configure further restrictions if you wish.

You will then have to bind-mount this cgroup subtree in the lurch container and indicate its location (as well as the cgroup version to use), with the additional options:
`-v /sys/fs/cgroup/lurch:/lurch/cgroup/lurch -e CGROUP_VERSION=2 -e CGROUP_MOUNT_POINT=/lurch/cgroup`.

Finally, we will also need `-P` to use the HTTP GUI later-on, so that the final docker command line is quite a mouthfull already:

```shell
% docker run --name lurch-demo \
             -v /var/run/docker.sock:/var/run/docker.sock \
             -v /sys/fs/cgroup/lurch:/lurch/cgroup/lurch \
             -e CGROUP_VERSION=2 -e CGROUP_MOUNT_POINT=/lurch/cgroup \
             -P --detach rixed/lurch
```

This will create and run the Postgresql database, the HTTP service for the GUI and the lurch command executor.
Now we are ready to give it some work to do.

There are really only three simple concepts to grasps: _programs_, which are composed of _commands_, and _runs_, which are program current or past execution.

The database come populated with a few programs that you can list with the `programs` subcommand:

```shell
% docker exec lurch-demo lurch programs | column -t -s $'\t'
# name          last run  last start  last stop  last exit code
test build      n.a.      n.a.        n.a.       n.a.
test chroot     n.a.      n.a.        n.a.       n.a.
test container  n.a.      n.a.        n.a.       n.a.
```

The command composing a program can be displayed as a compact s-expression with the `export` subcommand:

```shell
% docker exec lurch-demo lurch export 'test container'
(isolate
  (docker "rixed/lurch-docker-in-docker")
  (exec "/usr/bin/docker"
    ("ps")
    () null))
```

Which translates into: within the docker image "rixed/lurch-docker-in-docker", execute the binary "/usr/bin/docker" with the single argument "ps", an empty environment and no timeout.

That involves actually quite a lot of docker instances, so let's have a closer examination:

When we run this program, the lurch executor running within the `lurch-demo` container will first use the `docker` command (that's installed as part of the `rixed/lurch` image) to communicate with your local docker daemon to have it download the `rixed/lurch-docker-in-docker` image (should be instant, given it's the base image for `rixed/lurch`), and then execute `/usr/bin/docker ps` from _that_ image, which will again connect to your local docker daemon and retrieve from it the list of instances running on your local host and print it, thus proving that the docker daemon can be accessed from within the docker container.

Similarly here is the 'test chroot' program:

```shell
% docker exec lurch-demo lurch export 'test chroot'
(isolate
  (chroot "busybox")
  (exec "/bin/ls"
    ("/bin")
    () null))
```

This time, instead of using a docker image this program starts by creating a new chroot environment, populating it with busybox, and check the presence of the busybox tools by executing `/bin/ls /bin`.

Those programs are very simple and only make sure that the basic assumptions to run more complex programs are met.

The third and last demo program is marginally more useful, as it download, compile and test lurch itself, after having installed all its dependencies:

```shell
% docker exec lurch-demo lurch export 'test build'
(isolate
  (docker "debian:stable-slim")
  (sequence
    (exec "/usr/bin/apt-get"
      ("update")
      () null)
    (exec "/usr/bin/apt-get"
      ("install" "--quiet" "--yes" "git" "gcc" "make" "ocaml" "opam"
       "libpq-dev" "imagemagick")
      () null)
    (exec "/usr/bin/opam"
      ("init" "--no-setup" "--disable-sandboxing")
      () null)
    (exec "/usr/bin/opam"
      ("update" "--yes")
      () null)
    (exec "/usr/bin/opam"
      ("repo" "add" "--set-default" "ocalme"
       "git://github.com/rixed/ocalme-opam-repository.git")
      () null)
    (exec "/usr/bin/opam"
      ("repo" "priority" "ocalme" "1")
      () null)
    (exec "/usr/bin/opam"
      ("update" "--yes")
      () null)
    (exec "/usr/bin/opam"
      ("install" "--yes" "batteries" "cgi" "cmdliner" "js_of_ocaml"
       "js_of_ocaml-ppx" "js_of_ocaml-ppx_deriving_json" "ppx_deriving"
       "ocamlfind" "postgresql" "qtest" "syslog" "ocaml-vdom")
      () null)
    (exec "/usr/bin/git"
      ("clone" "https://github.com/rixed/lurch.git")
      () null)
    (exec "/bin/sh"
      ("-c" "cd lurch && ./configure && make && make check")
      ("OPAM_SWITCH_PREFIX=/root/.opam/default"
       "CAML_LD_LIBRARY_PATH=/root/.opam/default/lib/stublibs:/usr/local/lib/ocaml/4.11.1/stublibs:/usr/lib/ocaml/stublibs"
       "OCAML_TOPLEVEL_PATH=/root/.opam/default/lib/toplevel"
       "MANPATH=:/root/.opam/default/man"
       "PATH=/root/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin") null)))
```

This program checks that lurch itself can be build on Debian stable and that the test suite succeeds.

Let's write another very simple hello-world program and load it (notice the `-i` option to docker, without which lurch wouldn't be given an stdin!) :

```shell
% docker exec -i lurch-demo lurch import hello << EOF
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
% docker exec lurch-demo lurch start hello
Program hello started as run #1.
```

About 10 seconds later, that run should have ended:

```shell
% docker exec lurch-demo lurch runs hello | column -t -s $'\t'
# run  created              started              stopped              exit code  cpu usr   sys       ram usr    sys
1      2021-10-13T17:37:14  2021-10-13T17:37:15  2021-10-13T17:37:29  0          0.430348  2.430358  1MiB32KiB  16MiB527KiB592.
```

The logs for that run can be obtained as well:

```shell
% docker exec lurch-demo lurch logs 1
# run   time
2       2021-10-13T17:37:15     Populating chroot "/tmp/lurch/chroots/1634146635_85_YlGxbWQC" with busybox from "/usr/local/bin/busybox"
1       2021-10-13T17:37:17     Isolation builder (run #2) succeeded.
3       2021-10-13T17:37:18     Executing command #0 of sequence #15
3       2021-10-13T17:37:28     Executing command #1 of sequence #15
5       2021-10-13T17:37:28     hello world!
3       2021-10-13T17:37:29     sequence completed.
1       2021-10-13T17:37:29     Isolated subcommand (run #3) succeeded.
```

The first column indicates the run identifier of the command outputting that line.
Indeed, `1` was the run number of the outermost command composing the _hello_ program, but as most commands are composed of subcommands, and each subcommands are run (and monitored) individually, then each is also assigned a unique run identifier.
For instance, the run number 2 was the `(chroot "busybox")` and the run number 5 was the `echo`. We can drill down into individual runs:

```shell
% docker exec lurch-demo lurch logs 5
# run   time
5       2021-10-13T17:37:28     hello world!
```

You should now be familiar enough with the basic concepts, which are really quite trivial.
You might only interact with lurch in the web GUI from now on.

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

## The Web GUI

One does not have to manipulate the programs from the command line but can also use a web GUI.

Indeed, `lurch cgi` implements a simple CGI that serves a single-page-application performing all that
have been seen so far in an arguably user friendlier way.

The `rixed/lurch` docker image runs the `lighttpd` server on port 80.
It is configured to enforce basic authentication so that a username is known that can be logged in the database when a program is started.

Log in with user _demo_, password _demo_.

## Design principles

- Apart from some control flow, all commands are just handled by `execve`.

- All their output and resource usage are stored in the database.

- Part of the control flow must allow to run a program periodically à la crontab.

- Any program can be stopped at any step.

- Some bits of information can be collected from the logs of any command output (such as a measurement, a git tag name...) and stored as part of the run output and reused later. this is still TODO.

- For more flexibility when self hosting, the database (Postgres) and HTTP servers are external.

- A directory with secret files is bind mounted on the isolated container (default: /run/secrets)

## Running the docker image

Instead of installing your own you could use the docker demo image.

If your host operating system is using cgroup2:

```shell
% docker pull rixed/lurch
% docker run --name lurch-demo \
             -v /var/run/docker.sock:/var/run/docker.sock \
             -v /sys/fs/cgroup/lurch:/lurch/cgroup/lurch \
             -e CGROUP_VERSION=2 -e CGROUP_MOUNT_POINT=/lurch/cgroup \
             -P rixed/lurch
```

If it's using legacy cgroup:

```shell
% docker pull rixed/lurch
% docker run --name lurch-demo \
             -v /var/run/docker.sock:/var/run/docker.sock \
             -P rixed/lurch
```

Log-in with user `demo`, password `demo`.

Should you decide to use lurch, it is advised you either install lurch directly on the host or that you at least replace the `libhttpd` configuration of the demo docker image with a more secure one.

## How to Build

### Requirements

#### Busybox

Compile yourself a **statically linked** [busybox](https://www.busybox.net/) and store it somewhere.
(Note: look for the `Build static binary` option under `Build options` under the `Settings` option of `make menuconfig`).
It will be used for the chroot containment strategy.

#### CGroup

For cgroup v1: cgcreate/cgdelete tools (from Debian package cgroup-tools), and a cgroup lurch given to the user:

```shell
$ cgcreate -a $user -t $user -g cpuacct,memory:lurch`
```

For cgroup v2 a read-write top level cgroup named lurch, with cpu and memory controllers enabled:

```shell
$ mkdir /sys/fs/cgroup/lurch
$ sh -c 'echo +cpu +memory >> /sys/fs/cgroup/lurch/cgroup.subtree_control'
```

#### Docker

For the docker containment strategy, the `dockerd` daemon must be running on the host with ideally no other VMs (since lurch users will have access to them).

#### Http

An HTTP server able to run CGI scripts (such as `lighttpd`, for which you will find a possible configuration in `examples/lighttpd.conf`)

### Build commands

`./configure && make`

## How to Run

Now that you have build lurch, how do you run it?

### The database

Have a look at `db/create` to check/change the settings and then run it (ignore any NOTICE from psql because the database does not exist yet).

### The lurch daemon

Lurch executes programs step by step, using the `lurch step` command.
With the `--loop` option the command will keep running forever, otherwise it would execute one single step and stops.

```shell
% www/lurch step --loop
```

Or, manually for more control:

```shell
% while sudo www/lurch step --debug --busybox=$HOME/bin/busybox --secrets-dir=$PWD/programs/secrets --cgroup-version=2 ; do
    sleep 1
  done
```

### The WWW CGI interface

Here with lighttpd:

```shell
% cd www && lighttpd -f ../examples/lighttpd.conf
```

You should then be able to access it on port 80.

It is now a good idea to try to run the three sample programs `test container`, `test chroot` and `test build`.

### Want to run a program from a script?

As seen on the introduction, any program can be started with the `start` command:

```shell
% www/lurch start $whatever
```

## TODO

An important feature is still missing: we would like some bits of information collected from programs output and stored in the database and displayed as part of the run output, for instance to identify which git tag was build. Those results would be given a name, and the results from the last run would be accessible from the next run of that program.

For others, see: https://github.com/rixed/lurch/issues
