drop database if exists lurch;
create database lurch;
\connect lurch;

-- We have a very simple language to design workflow, that's simple enough to
-- represented and edited graphically.
-- It is composed of a few simple command. Each of those can succeed, fail, timeout or
-- be interrupted. Unless intercepted, any for of failure terminate the program.

-- Commands:

-- A base table (not using postgresql inheritance which is in limbo)
create table command (
  id serial,
  primary key (id)
);

-- isolate creates chroot/docker container instances, is which further
-- commands will be executed.
-- The actual isolation mechanism is build by the `build` command whereas
-- the command that must run within the isolated container is given by the
-- second command. This allows to track resources and duration of the
-- container creation command inddependantly, as a normal command.
-- To retrieve the isolation context of a command it is thus required to
-- lookup the chain of parents until the first "isolate" command, and then
-- look at its `build` subcommand.
create table command_isolate (
  command int,
  builder int not null,
  subcommand int not null,
  foreign key (command) references command (id) on delete cascade,
  foreign key (builder) references command (id),
  foreign key (subcommand) references command (id)
);

-- Those are the two isolation builder that are available:
-- chroot and docker.

create table command_chroot (
  command int,
  template text not null default 'busybox',
  foreign key (command) references command (id) on delete cascade
);

create table command_docker (
  command int,
  image text not null default 'debian:buster-slim',
  foreign key (command) references command (id) on delete cascade
);

create table command_exec (
  command int,
  pathname text not null,
  -- !!!WARNING WARNING WARNING!!! postgres arrays start at 1 !!!
  args text[] not null default '{}', -- Editor should provide sane default
  env text[] not null default '{}', -- Editor should provide sane default
  timeout float,
  foreign key (command) references command (id) on delete cascade
);

create table command_nop (
  command int,
  exit_code int not null default 0,
  foreign key (command) references command (id) on delete cascade
);

create table command_approve (
  command int,
  subcommand int not null,
  timeout float,
  comment text not null default '',
  autosuccess boolean not null default false,
  foreign key (command) references command (id) on delete cascade,
  foreign key (subcommand) references command (id)
);

create table command_sequence (
  command int,
  subcommands int array not null, -- TODO: each of those must reference command
  foreign key (command) references command (id) on delete cascade
);

create table command_retry (
  command int,
  subcommand int not null,
  up_to int not null default 3,
  foreign key (command) references command (id) on delete cascade,
  foreign key (subcommand) references command (id)
);

create table command_try (
  command int,
  subcommand int not null,
  on_failure int not null,
  foreign key (command) references command (id) on delete cascade,
  foreign key (subcommand) references command (id),
  foreign key (on_failure) references command (id)
);

create table command_pause (
  command int,
  subcommand int not null,
  duration float,
  foreign key (command) references command (id) on delete cascade,
  foreign key (subcommand) references command (id)
);

-- Now we have named program:

-- Append only tables so we can go back in history, see previous runs etc:
create table program (
  name text not null,
  created timestamp not null default now(),
  deleted timestamp not null default to_timestamp(0), -- meaning not deleted
  command int not null,

  primary key (name, deleted),
  foreign key (command) references command (id)
);

-- Save the run of any command (since program are their top level command,
-- of program too:
create table run (
  id serial,
  command int not null,
  -- The top-level run that triggered that one (NULL -> self).
  top_run int,
  -- The parent run (NULL -> no parent)
  parent_run int,
  created timestamp not null default now(),
  started timestamp,
  stopped timestamp check (stopped is null or started is not null),
  -- All terminal commands are run, but compound commands are not obviously:
  cgroup text check (cgroup is null or started is not null),
  pid int check (pid is null or started is not null),
  -- Of self only (includes subprocesses but not subcommands!), in seconds:
  cpu_usr float,
  cpu_sys float,
  -- max memory, ram+swap and then in the kernel, in bytes:
  mem_usr int,
  mem_sys int,

  -- 0..: process exit code
  -- -128..-1: killed by signal -value
  -- -129: expiration
  -- -130: cancellation
  -- see LurchApiTypes.ExitCode
  exit_code int,

  primary key (id),
  foreign key (command) references command (id) on delete cascade,
  foreign key (top_run) references run (id) on delete cascade,
  foreign key (parent_run) references run (id) on delete cascade,
  -- we must be able to associate a given subcommand to its run:
  unique (command, parent_run)
);

create table logline (
  run int not null,
  -- 1 and 2 are going to be the most common:
  fd int not null,
  time timestamp not null default now(),
  line text not null,

  foreign key (run) references run (id) on delete cascade
);

create index if not exists logline_run on logline using hash (run);
create index if not exists logline_time on logline (time);

-- Where to store that a command_approve has been approved:
create table approved (
  run int not null, -- must be a command_approve
  time timestamp not null default now(),
  message text not null default '',

  foreign key (run) references run (id) on delete cascade,
  unique (run)
);

-- Where to store chroot paths:
create table chroot_path (
  run int not null, -- must be a command_chroot
  path text not null,

  foreign key (run) references run (id) on delete cascade,
  unique (run)
);

-- Where to store docker instances:
create table docker_instance (
  run int not null, -- must be a command_docker
  instance text not null,
  docker_id text not null,

  foreign key (run) references run (id) on delete cascade,
  unique (run)
);

-- Views:

-- Log lines with the associated program run as program_run:
create view list_loglines as
  select
    r.top_run,
    r.id as run,
    r.command,
    l.fd, l.time, l.line
  from logline l
  join run r on l.run = r.id;

-- synthetic view of top level runs
create view top_level_runs as
  select
    r1.id,
    r1.command,
    min(r2.created) as created,
    min(r2.started) as started,
    max(r2.stopped) as stopped,
    r1.exit_code,
    sum(r2.cpu_usr) as cpu_usr,
    sum(r2.cpu_sys) as cpu_sys,
    sum(r2.mem_usr) as mem_usr,
    sum(r2.mem_sys) as mem_sys
  from
    run r1
    left outer join run r2 on r1.id = coalesce(r2.top_run, r2.id)
  group by r1.id, r1.command, r1.exit_code;

-- The list of last program runs:
-- Corresponds to API type ListPastRuns
create view list_past_runs as
  select
    p.name as name,
    r.id as top_run,
    r.created as created,
    r.started as started,
    r.stopped as stopped,
    r.cpu_usr as cpu_usr,
    r.cpu_sys as cpu_sys,
    r.mem_usr as mem_usr,
    r.mem_sys as mem_sys,
    r.exit_code as exit_code
  from
    program p
    join top_level_runs r on p.command = r.command
  where
    p.deleted <= p.created;

-- The list of program, with info about their last run:
-- Corresponds to API type ListPrograms
create view list_programs as
  select
    p.name as name,
    r.id as last_run,
    r.started as last_start,
    r.stopped as last_stop,
    r.exit_code as last_exit_code
  from
    program p
    left outer join (
      select command, max(created) as last_created from run
      group by command
    ) lr on p.command = lr.command
    left outer join run r on r.command = lr.command and
                             r.created = lr.last_created
  where
    p.deleted <= p.created
  group by p.name, r.id, r.started, r.stopped, r.exit_code;

-- All terminal commands that have not been started yet:
create view list_waiting_terminals as
  select
    r.id,
    r.created,
    c.command
  from run r
  join (
    select command from command_exec union
    select command from command_nop
  ) c on r.command = c.command
  where
    r.started is null;

-- All sequences that are still working:
create view list_running_sequences as
  select
    r.id,
    coalesce(r2.step_count, 0) as step_count,
    coalesce(r2.all_success, true) as all_success, -- so far
    r2.exit_codes
  from run r
  join command_sequence s on r.command = s.command
  -- to get the step number and check all previous steps have finished
  left outer join (
    select
      parent_run,
      array_agg(exit_code) filter (where exit_code is not null) as exit_codes,
      -- count the stopped steps:
      count(*) as step_count,
      bool_and(exit_code is not null and exit_code = 0) as all_success,
      -- Have all subcommands stopped already?
      bool_and(stopped is not null) as all_stopped
    from run
    group by parent_run
  ) r2 on r2.parent_run = r.id
  where
    -- the sequence itself is unfinished:
    r.stopped is null and
    -- but all started subcommands are:
    coalesce(r2.all_stopped, true);

create view list_running_pauses as
  select
    r.id as run,
    c.duration,
    c.subcommand
  from run r
  join command_pause c on c.command = r.command
  where r.stopped is null;

-- List all pending command_approve commands, possibly with the corresponding
-- confirmation message
create view list_pending_approval as
  select
    r.id as run,
    c.time,
    c.message,
    w.autosuccess
  from run r
  join command_approve w on w.command = r.command
  left outer join approved c on c.run = r.id
  where r.stopped is null;

-- List all containers/chroot that have to be build
create view list_pending_isolations as
  select r.id as run, r.created from run r
  join command_isolate c on c.command = r.command
  where r.stopped is null;

-- List all ongoing runs which parent has stopped (to propagate cancellations)
create view list_obsolete_runs as
  select r.id
  from run r
  join run rp on r.parent_run = rp.id
  where r.stopped is null and rp.stopped is not null;
