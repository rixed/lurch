\connect lurch;

-- a simple build, no isolation

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'git clone https://github.com/rixed/lurch.git');

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'make');

insert into command default values;
insert into command_approve (command, subcommand, timeout) values
  ((select max(id) from command),
   (select max(command) from command_shell),
   864000); -- 10 days

insert into command default values;
insert into command_sequence (command, subcommands) values
  ((select max(id) from command),
   ARRAY[(select min(command) from command_shell),
         (select max(command) from command_approve),
         (select max(command) from command_shell)]);

insert into program (name, command) values
  ('test build', (select max(command) from command_sequence));

-- test busybox chroot:

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'ls bin');

insert into command default values;
insert into command_chroot (command, template) values
  ((select max(id) from command), 'busybox');

insert into command default values;
insert into command_isolate (command, builder, subcommand) values
  ((select max(id) from command),
   (select max(command) from command_chroot),
   (select max(command) from command_shell));

insert into program (name, command) values
  ('test chroot', (select max(command) from command_isolate));

-- test docker (that we can run docker within docker!):

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'docker ps');

insert into command default values;
insert into command_docker (command, image) values
  ((select max(id) from command), 'rixed/lurch-docker-in-docker');

insert into command default values;
insert into command_isolate (command, builder, subcommand) values
  ((select max(id) from command),
   (select max(command) from command_docker),
   (select max(command) from command_shell));

insert into program (name, command) values
  ('test container', (select max(command) from command_isolate));

-- test we can build Ramen from the ramen-dev image

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'git fetch origin master');

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'git checkout origin/master');

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), './configure');

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'make -j3');

insert into command default values;
insert into command_shell (command, line) values
  ((select max(id) from command), 'make check');

insert into command default values;
insert into command_sequence (command, subcommands) values
  ((select max(id) from command),
   ARRAY[(select command from command_shell where line = 'git fetch origin master'),
         (select command from command_shell where line = 'git checkout origin/master'),
         (select command from command_shell where line = './configure'),
         (select command from command_shell where line = 'make -j3'),
         (select command from command_shell where line = 'make check')]);

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
