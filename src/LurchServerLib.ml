open Batteries

let user = ref "rixed"

(* System *)

let int_of_fd : Legacy.Unix.file_descr -> int = fun fd -> Obj.magic fd

(* Http *)

let output = IO.output_string ()

let log_text = IO.output_string ()

(* Make a unit printer out of the string printer log_text: *)
let log_text_io =
  IO.create_out
    ~write:(Char.print log_text)
    ~output:(fun bytes o l -> IO.output log_text bytes o l)
    ~flush:(fun () -> IO.flush log_text)
    ~close:(fun () -> ())

let content_type = ref "text/html"
let todo what = failwith ("TODO: "^ what)

type 'a printer = ('a, unit IO.output, unit) Printf.t -> 'a

type logger =
  { mutable debug : 'a. 'a printer ;
    mutable info : 'a. 'a printer ;
    mutable warning : 'a. 'a printer ;
    mutable error : 'a. 'a printer }

let log =
  let ign fmt = Printf.ifprintf stderr fmt in
  { debug = ign ; info = ign ; warning = ign ; error = ign }

let is_debug = ref false

let init_log ?(with_time=true) dbg for_cli =
  is_debug := dbg ;
  let cgi fmt =
    Printf.fprintf log_text_io (fmt ^^ "\n") in
  let cli fmt =
    let open Unix in
    if with_time then (
      let now = time () in
      let tm = localtime now in
      let time = Printf.sprintf "%02dh%02dm%02d"
                   tm.tm_hour tm.tm_min tm.tm_sec in
      Printf.eprintf ("%s: " ^^ fmt ^^ "\n%!") time
    ) else
      Printf.eprintf (fmt ^^ "\n%!")
  in
  if for_cli then (
    log.error <- cli ;
    log.warning <- cli ;
    log.info <- cli ;
    if dbg then log.debug <- cli
  ) else (
    log.error <- cgi ;
    log.warning <- cgi ;
    log.info <- cgi ;
    if dbg then log.debug <- cgi
  )

let log_exceptions f =
  try f ()
  with e ->
    log.error "Ignoring that exception: %s\n%s"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())

let print_data b =
  content_type := "application/json" ;
  String.print output (Buffer.contents b)

let trailer () =
  match !content_type with
  | "text/html" ->
      Printf.printf "<!--debug-->\n<pre>%s</pre>\n" (IO.close_out log_text)
  | "text/plain" ->
      Printf.printf "\n%s\n" (IO.close_out log_text)
  | _ ->
      ()

let todo what =
  failwith ("TODO: "^ what)

let unknown () =
  let err_msg = "No such resource" in
  Cgi.header ~status:404 ~err_msg () ;
  print_string "You rang?"

let missing_parameter p =
  let err_msg = "Missing parameter" in
  Cgi.header ~status:400 ~err_msg () ;
  print_string ("Missing parameter "^ p)

let array_find_mapi f a =
  let res = ref None in
  try
    for i = 0 to Array.length a - 1 do
      match f i with
      | None -> ()
      | Some x ->
          res := Some x ;
          raise Exit
    done ;
    raise Not_found
  with Exit ->
    Option.get !res

let string_exists f s =
  let rec loop i =
    if i >= String.length s then false
    else if f s.[i] then true
    else loop (i + 1) in
  loop 0

let print_exit_status oc = function
  | Unix.WEXITED s -> Printf.fprintf oc "exited with status %d" s
  | Unix.WSIGNALED s -> Printf.fprintf oc "killed by signal %d" s
  | Unix.WSTOPPED s -> Printf.fprintf oc "stopped by signal %d" s

(* Trick from LWT: how to exit without executing the at_exit hooks: *)
external sys_exit : int -> 'a = "caml_sys_exit"

let fd_of_int : int -> Unix.file_descr = Obj.magic

let set_signals sigs behavior =
  List.iter (fun s ->
    Sys.set_signal s behavior
  ) sigs

let clean_fork () =
  flush_all () ;
  Gc.compact () ; (* Avoid doing that twice *)
  Unix.fork ()

let shell_quote s =
  "'"^ String.nreplace s "'" "'\\''" ^"'"

let write_or_fail ~fname fd s =
  try
    Unix.single_write_substring fd s 0 (String.length s) |>
    ignore
  with e ->
    Printf.sprintf "Cannot write into %s: %s"
      fname
      (Printexc.to_string e) |>
    failwith

let close_or_ignore ~fname fd =
  try
    Unix.close fd
  with e ->
    Printf.eprintf "Cannot close %s: %s, c'est la vie!\n"
      fname
      (Printexc.to_string e)

let getenv_or_empty n =
  try Sys.getenv n
  with Not_found -> ""

let system_or_fail cmd =
  log.debug "Execute %S" cmd ;
  match Unix.system cmd with
  | exception e ->
      log.error "Cannot execute command %s (PATH is %S)"
        cmd
        (getenv_or_empty "PATH") ;
      Printf.sprintf "Cannot exec %s: %s!\n"
        cmd
        (Printexc.to_string e) |>
      failwith
  | Unix.WEXITED 0 -> ()
  | status ->
      Printf.sprintf2 "Command %S %a!\n"
        cmd
        print_exit_status status |>
      failwith

let command_output cmd =
  log.debug "Execute %S" cmd ;
  match Unix.run_and_read cmd with
  | exception e ->
      log.error "Cannot execute command %s (PATH is %S)"
        cmd
        (getenv_or_empty "PATH") ;
      Printf.sprintf "Cannot exec %s: %s!\n"
        cmd
        (Printexc.to_string e) |>
      failwith
  | Unix.WEXITED 0, output ->
      String.trim output
  | status, _ ->
      Printf.sprintf2 "Command %S %a!\n"
        cmd
        print_exit_status status |>
      failwith

let random_string () =
  let chars = "abcdefghijklmnopqrstuvwxyz\
               ABCDEFGHIJKLMNOPQRSTUVWXYZ\
               0123456789" in
  String.init 8 (fun _ -> chars.[Random.int (String.length chars)])

(* Unix signals from `kill -L` (on Linux):
 *
 *   1 HUP      2 INT      3 QUIT     4 ILL      5 TRAP     6 ABRT     7 BUS
 *   8 FPE      9 KILL    10 USR1    11 SEGV    12 USR2    13 PIPE    14 ALRM
 *  15 TERM    16 STKFLT  17 CHLD    18 CONT    19 STOP    20 TSTP    21 TTIN
 *  22 TTOU    23 URG     24 XCPU    25 XFSZ    26 VTALRM  27 PROF    28 WINCH
 *  29 POLL    30 PWR     31 SYS
 *
 * FIXME: get those numbers for the current OS from some C function
 *)
let to_unix_signal s =
  let open Sys in
  if s = sigabrt then 6 else
  if s = sigalrm then 14 else
  if s = sigfpe then 8 else
  if s = sighup then 1 else
  if s = sigill then 4 else
  if s = sigint then 2 else
  if s = sigkill then 9 else
  if s = sigpipe then 13 else
  if s = sigquit then 3 else
  if s = sigsegv then 11 else
  if s = sigterm then 15 else
  if s = sigusr1 then 10 else
  if s = sigusr2 then 12 else
  if s = sigchld then 17 else
  if s = sigcont then 18 else
  if s = sigstop then 19 else
  if s = sigtstp then 20 else
  if s = sigttin then 21 else
  if s = sigttou then 22 else
  if s = sigvtalrm then 26 else
  if s = sigprof then 27 else
  if s = sigbus then 7 else
  if s = sigpoll then 29 else
  if s = sigsys then 31 else
  if s = sigtrap then 5 else
  if s = sigurg then 23 else
  if s = sigxcpu then 24 else
  if s = sigxfsz then 25 else
  (* [caml_convert_signal_number] does pass the actual number when it does
   * not know a signal: *)
  s
