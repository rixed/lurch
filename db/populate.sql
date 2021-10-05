\connect lurch;

-- a simple build

insert into command default values;
insert into command_exec (command, pathname, args) values
  ((select max(id) from command),
   '/usr/bin/git',
   '{"clone","https://github.com/rixed/lurch.git"}');

insert into command default values;
insert into command_exec (command, pathname) values
  ((select max(id) from command), '/usr/bin/make');

insert into command default values;
insert into command_approve (command, subcommand, timeout) values
  ((select max(id) from command),
   (select max(command) from command_exec),
   864000); -- 10 days

insert into command default values;
insert into command_sequence (command, subcommands) values
  ((select max(id) from command),
   ARRAY[(select min(command) from command_exec),
         (select max(command) from command_approve),
         (select max(command) from command_exec)]);

insert into command default values;
insert into command_chroot (command, template) values
  ((select max(id) from command), 'busybox');

insert into command default values;
insert into command_isolate (command, builder, subcommand) values
  ((select max(id) from command),
   (select max(command) from command_chroot),
   (select max(command) from command_sequence));

insert into program (name, command) values
  ('test build', (select max(command) from command_isolate));

-- test busybox chroot:

insert into command default values;
insert into command_exec (command, pathname, args) values
  ((select max(id) from command), '/bin/ls', '{"/bin"}');

insert into command default values;
insert into command_chroot (command, template) values
  ((select max(id) from command), 'busybox');

insert into command default values;
insert into command_isolate (command, builder, subcommand) values
  ((select max(id) from command),
   (select max(command) from command_chroot),
   (select max(command) from command_exec));

insert into program (name, command) values
  ('test chroot', (select max(command) from command_isolate));

-- test docker (that we can run docker within docker!):

insert into command default values;
insert into command_exec (command, pathname, args) values
  ((select max(id) from command), '/usr/bin/docker', '{"ps"}');

insert into command default values;
insert into command_docker (command, image) values
  ((select max(id) from command), 'rixed/lurch-docker-in-docker');

insert into command default values;
insert into command_isolate (command, builder, subcommand) values
  ((select max(id) from command),
   (select max(command) from command_docker),
   (select max(command) from command_exec));

insert into program (name, command) values
  ('test container', (select max(command) from command_isolate));

-- test we can build Ramen from the ramen-dev image

insert into command default values;
insert into command_exec (command, pathname, args) values
  ((select max(id) from command), '/usr/bin/git', '{"fetch","origin","master"}');

insert into command default values;
insert into command_exec (command, pathname, args) values
  ((select max(id) from command), '/usr/bin/git', '{"checkout","origin/master"}');

insert into command default values;
insert into command_exec (command, pathname) values
  ((select max(id) from command), './configure');

insert into command default values;
insert into command_exec (command, pathname, args) values
  ((select max(id) from command), '/usr/bin/make', '{"-j3"}');

insert into command default values;
insert into command_exec (command, pathname, args) values
  ((select max(id) from command), '/usr/bin/make', '{"check"}');

insert into command default values;
insert into command_sequence (command, subcommands) values
  ((select max(id) from command),
   -- !!!WARNING WARNING WARNING!!! postgres arrays start at 1 !!!
   ARRAY[(select command from command_exec where args[1] = 'fetch'),
         (select command from command_exec where args[1] = 'checkout'),
         (select command from command_exec where pathname = './configure'),
         (select command from command_exec where args[1] = '-j3'),
         (select command from command_exec where args[1] = 'check')]);

insert into command default values;
insert into command_docker (command, image) values
  ((select max(id) from command), 'rixed/ramen-dev:latest');

insert into command default values;
insert into command_isolate (command, builder, subcommand) values
  ((select max(id) from command),
   (select max(command) from command_docker),
   (select max(command) from command_sequence));

insert into program (name, command) values
  ('build+test ramen', (select max(command) from command_isolate));
