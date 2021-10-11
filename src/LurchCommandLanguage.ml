(* Both the client and the server needs to represent commands as text, and
 * the other way around. *)
open LurchApiTypes.Command

module Api = LurchApiTypes

(* String representation of commands are mere s-expressions.
 * strings are represented as OCaml quoted strings. *)
type context = Blank | Enter | Leave | Symbol | String

let char_is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' | '\b' -> true
  | _ -> false

let option_map f = function
  | None -> None
  | Some v -> Some (f v)

let rec tok str res i =
  let ctx = match res with (ctx, _)::_ -> ctx | [] -> Blank in
  if i >= String.length str then List.rev res
  else if char_is_whitespace str.[i] then
    let res =
      if ctx = Blank || ctx = String then res
      else (Blank, i) :: res in
    tok str res (i + 1)
  else if str.[i] = '(' then
    let res =
      if ctx = String then res
      else (Enter, i) :: res in
    tok str res (i + 1)
  else if str.[i] = ')' then
    let res =
      if ctx = String then res
      else (Leave, i) :: res in
    tok str res (i + 1)
  else if str.[i] = '"' then
    let res =
      (* String start and stop with the double quotes, included: *)
      if ctx = String then (Blank, i + 1) :: res
      else (String, i) :: res in
    tok str res (i + 1)
  else if str.[i] = '\\' && ctx = String && i < String.length str - 1 then
    tok str res (i + 2)
  else
    let res =
      if ctx = Symbol || ctx = String then res
      else (Symbol, i) :: res in
    tok str res (i + 1)

type sexpr =
  | Sym of string
  | Str of string
  | Lst of sexpr list

let rec string_of_sexpr =
  let rec loop indent sep prev = function
    | Sym s ->
        prev ^ sep ^ s
    | Str s ->
        prev ^ sep ^ "\"" ^ String.escaped s ^ "\""
    | Lst lst ->
        let prev, indent =
          if prev = "" then
            prev, indent
          else
            prev ^ "\n" ^ indent ^ "  ",
            indent ^"  " in
        let prev = prev ^ "(" in
        let _, prev =
          List.fold_left (fun (i, prev) x ->
            i + 1,
            loop indent (if i > 0 then " " else "") prev x
          ) (0, prev) lst in
        prev ^ ")" in
  loop "" "" ""

exception Invalid_expression of sexpr
exception Extraneous_expressions of int

let sexpr_of_string str =
  let toks = tok str [] 0 in
  let add_sym sta sto lst =
    Sym (String.sub str sta (sto-sta)) :: lst in
  let add_str sta sto lst =
    Str (Scanf.sscanf (String.sub str sta (sto-sta)) "%S" (fun x -> x)) :: lst in
  let add_blk _sta _sto lst = lst in
  let rec loop lst adder = function
    | [] ->
        let sto = String.length str in
        List.rev (adder sto lst),
        []
    | (Symbol, i) :: rest -> loop (adder i lst) (add_sym i) rest
    | (String, i) :: rest -> loop (adder i lst) (add_str i) rest
    | (Blank, i) :: rest -> loop (adder i lst) (add_blk i) rest
    | (Enter, i) :: rest ->
        let lst = adder i lst in
        let sublst, rest = loop [] (add_blk i) rest in
        loop (Lst sublst :: lst) (add_blk i) rest
    | (Leave, i) :: rest ->
        List.rev (adder i lst),
        rest
  in
  let sublst, rest = loop [] (add_blk 0) toks in
  (match rest with
  | (_, i) :: _ -> raise (Extraneous_expressions i)
  | [] -> ()) ;
  sublst

let command_of_string str =
  let or_null conv = function
    | Sym "null" -> None
    | s -> Some (conv s) in
  let float_of_sym = function
    | Sym s -> float_of_string s
    | _ -> invalid_arg "float_of_sym" in
  let int_of_sym = function
    | Sym s -> int_of_string s
    | _ -> invalid_arg "int_of_sym" in
  let string_of_str = function
    | Str s -> s
    | _ -> invalid_arg "string_of_str" in
  let rec operation_of_sexpr = function
    | Lst [ Sym "no-op" ; Sym exit_code ] ->
        Nop { exit_code = int_of_string exit_code }
    | Lst [ Sym "isolate" ; s1 ; s2 ] ->
        Isolate { builder = command_of_sexpr s1 ;
                  subcommand = command_of_sexpr s2 }
    | Lst [ Sym "chroot" ; Str template ] ->
        Chroot { template }
    | Lst [ Sym "docker" ; Str image ] ->
        Docker { image }
    | Lst [ Sym "exec" ; Str pathname ; Lst args ; Lst env ; timeout ] ->
        let args = List.map string_of_str args |> Array.of_list
        and env = List.map string_of_str env |> Array.of_list
        and timeout = or_null float_of_sym timeout in
        Exec { pathname ; args ; env ; timeout }
    | Lst [ Sym "approve" ; Sym timeout ; Str comment ; Sym autosuccess ] ->
        let timeout =
          if timeout = "null" then None else Some (float_of_string timeout) in
        Approve { timeout ; comment ;
                  autosuccess = Api.sql_bool_of_string autosuccess }
    | Lst [ Sym "let" ; Sym var_name ; Str default ; Str comment ; s ] ->
        Let { var_name ; default ; comment ; subcommand = command_of_sexpr s }
    | Lst (Sym "sequence" :: cmds) ->
        Sequence { subcommands = List.map command_of_sexpr cmds }
    | Lst [ Sym "retry" ; s ; Sym up_to ] ->
        Retry { subcommand = command_of_sexpr s ;
                up_to = int_of_string up_to }
    | Lst [ Sym "if" ; s1 ; s2 ; s3 ] ->
        If { condition = command_of_sexpr s1 ;
             consequent = command_of_sexpr s2 ;
             alternative = command_of_sexpr s3 }
    | Lst [ Sym "pause" ; Sym duration ] ->
        Pause { duration = float_of_string duration }
    | Lst [ Sym "wait" ; Lst minute ; Lst hour ; Lst mday ; Lst month ;
            Lst wday ] ->
        Wait { minute = List.map int_of_sym minute ;
               hour = List.map int_of_sym hour ;
               mday = List.map int_of_sym mday ;
               month = List.map int_of_sym month ;
               wday = List.map int_of_sym wday }
    | Lst [ Sym "spawn" ; Str program ] ->
        Spawn { program }
    | Lst [ Sym "for" ; Sym var_name ; Lst values ; s ] ->
        let values = List.map string_of_str values |> Array.of_list in
        ForLoop { var_name ; values ; subcommand = command_of_sexpr s }
    | Lst [ Sym "break" ; Sym depth ] ->
        Break { depth = int_of_string depth }
    | x -> raise (Invalid_expression x)
  and command_of_sexpr s =
    { id = 0 ; operation = operation_of_sexpr s }
  in
  match List.map command_of_sexpr (sexpr_of_string str) with
  | [ c ] -> c
  | _ -> failwith "Cannot parse string into a single command"

let string_of_command ?max_depth cmd =
  let or_null conv = function
    | None -> Sym "null"
    | Some v -> conv v in
  let sym_of_float f = Sym (string_of_float f) in
  let sym_of_int i = Sym (string_of_int i) in
  let to_str s = Str s in
  let rec sexpr_of_operation ?max_depth = function
    | Nop { exit_code } ->
        Lst [ Sym "no-op" ; Sym (string_of_int exit_code) ]
    | Isolate { builder ; subcommand } ->
        Lst [ Sym "isolate" ;
              sexpr_of_command ?max_depth builder ;
              sexpr_of_command ?max_depth subcommand ]
    | Chroot { template } ->
        Lst [ Sym "chroot" ; Str template ]
    | Docker { image } ->
        Lst [ Sym "docker" ; Str image ]
    | Exec { pathname ; args ; env ; timeout } ->
        Lst [ Sym "exec" ; Str pathname ;
              Lst (List.map to_str (Array.to_list args)) ;
              Lst (List.map to_str (Array.to_list env)) ;
              or_null sym_of_float timeout ]
    | Approve { timeout ; comment ; autosuccess } ->
        Lst [ Sym "approve" ;
              Sym (match timeout with None -> "null"
                                    | Some t -> string_of_float t) ;
              Str comment ;
              Sym (Api.sql_string_of_bool autosuccess) ]
    | Let { var_name ; default ; subcommand ; comment } ->
        Lst [ Sym "let" ;
              Sym var_name ;
              Str default ;
              Str comment ;
              sexpr_of_command ?max_depth subcommand ]
    | Sequence { subcommands } ->
        Lst (Sym "sequence" ::
             (List.map (sexpr_of_command ?max_depth) subcommands))
    | Retry { subcommand ; up_to } ->
        Lst [ Sym "retry" ;
              sexpr_of_command ?max_depth subcommand ;
              Sym (string_of_int up_to) ]
    | If { condition ; consequent ; alternative } ->
        Lst [ Sym "if" ;
              sexpr_of_command ?max_depth condition ;
              sexpr_of_command ?max_depth consequent ;
              sexpr_of_command ?max_depth alternative ]
    | Pause { duration } ->
        Lst [ Sym "pause" ;
              Sym (string_of_float duration) ]
    | Wait { minute ; hour ; mday ; month ; wday } ->
        Lst [ Sym "wait" ;
              Lst (List.map sym_of_int minute) ;
              Lst (List.map sym_of_int hour) ;
              Lst (List.map sym_of_int mday) ;
              Lst (List.map sym_of_int month) ;
              Lst (List.map sym_of_int wday) ]
    | Spawn { program } ->
        Lst [ Sym "spawn" ; Str program ]
    | ForLoop { var_name ; values ; subcommand } ->
        let values = List.map to_str (Array.to_list values) in
        Lst [ Sym "for" ; Sym var_name ; Lst values ;
              sexpr_of_command ?max_depth subcommand ]
    | Break { depth } ->
        Lst [ Sym "break" ; Sym (string_of_int depth) ]
  and sexpr_of_command ?max_depth c =
    match max_depth with
    | Some d when d <= 0 -> Str "…"
    | _ ->
        let max_depth = option_map pred max_depth in
        sexpr_of_operation ?max_depth c.operation
  in
  sexpr_of_command ?max_depth cmd |>
  string_of_sexpr

(*$inject
  open Batteries

  let linearize =
    let re = Str.regexp "[ \n]+" in
    fun s -> Str.global_replace re " " s
*)

(*$= string_of_sexpr & ~printer:identity
  "(sequence (pause 10) (exec \"make\" () () null))" \
    (linearize \
      (string_of_sexpr (Lst [ Sym "sequence" ; \
        Lst [ Sym "pause" ; Sym "10" ] ; \
        Lst [ Sym "exec" ; Str "make" ; Lst [] ; Lst [] ; Sym "null" ] ])))
*)

(*$= string_of_command & ~printer:identity
  "(sequence (pause 4.) (exec \"make\" (\"make\") () 0.))" \
    (linearize \
      (string_of_command (command_of_string \
        "(sequence (pause 4) (exec \"make\" (\"make\") () 0))")))
*)
