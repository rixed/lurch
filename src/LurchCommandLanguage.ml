(* Both the client and the server needs to represent commands as text, and
 * the other way around. *)
open LurchApiTypes.Command

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
  let rec operation_of_sexpr = function
    | Lst [ Sym "nop" ] ->
        Nop
    | Lst [ Sym "isolate" ; s1 ; s2 ] ->
        Isolate { builder = command_of_sexpr s1 ;
                  subcommand = command_of_sexpr s2 }
    | Lst [ Sym "chroot" ; Str template ] ->
        Chroot { template }
    | Lst [ Sym "docker" ; Str image ] ->
        Docker { image }
    | Lst [ Sym "shell" ; Str line ] ->
        Shell { line ; timeout = None }
    | Lst [ Sym "shell" ; Str line ; Sym timeout ] ->
        let timeout = Some (float_of_string timeout) in
        Shell { line ; timeout }
    | Lst [ Sym "git-clone" ; Str url ] ->
        GitClone { url ; revision = None ; directory = None }
    | Lst [ Sym "git-clone" ; Str url ; Str revision ] ->
        let revision = Some revision in
        GitClone { url ; revision ; directory = None }
    | Lst [ Sym "git-clone" ; Str url ; Str revision ; Str directory ] ->
        let revision = if revision = "" then None else Some revision
        and directory = Some directory in
        GitClone { url ; revision ; directory }
    | Lst [ Sym "wait" ; (Lst _ as s) ] ->
        Wait { subcommand = command_of_sexpr s ; timeout = None }
    | Lst [ Sym "wait" ; Sym timeout ; s ] ->
        let timeout = Some (float_of_string timeout) in
        Wait { subcommand = command_of_sexpr s ; timeout }
    | Lst (Sym "sequence" :: cmds) ->
        Sequence { subcommands = List.map command_of_sexpr cmds }
    | Lst [ Sym "retry" ; s ; Sym up_to ] ->
        Retry { subcommand = command_of_sexpr s ;
                up_to = int_of_string up_to }
    | Lst [ Sym "try" ; s1 ; s2 ] ->
        Try { subcommand = command_of_sexpr s1 ;
              on_failure = command_of_sexpr s2 }
    | x -> raise (Invalid_expression x)
  and command_of_sexpr s =
    { id = 0 ; operation = operation_of_sexpr s }
  in
  match List.map command_of_sexpr (sexpr_of_string str) with
  | [ c ] -> c
  | _ -> failwith "Cannot parse string into a single command"

let string_of_command ?max_depth cmd =
  let rec sexpr_of_operation ?max_depth = function
    | Nop ->
        Lst [ Sym "nop" ]
    | Isolate { builder ; subcommand } ->
        Lst [ Sym "isolate" ;
              sexpr_of_command ?max_depth builder ;
              sexpr_of_command ?max_depth subcommand ]
    | Chroot { template } ->
        Lst [ Sym "chroot" ; Str template ]
    | Docker { image } ->
        Lst [ Sym "docker" ; Str image ]
    | Shell { line ; timeout = None } ->
        Lst [ Sym "shell" ; Str line ]
    | Shell { line ; timeout = Some t } ->
        Lst [ Sym "shell" ; Str line ; Sym (string_of_float t) ]
    | GitClone { url ; revision = None ; directory = None } ->
        Lst [ Sym "git-clone" ; Str url ]
    | GitClone { url ; revision = Some r ; directory = None } ->
        Lst [ Sym "git-clone" ; Str url ; Str r ]
    | GitClone { url ; revision = Some r ; directory = Some d } ->
        Lst [ Sym "git-clone" ; Str url ; Str r ; Str d ]
    | GitClone { url ; revision = None ; directory = Some d } ->
        Lst [ Sym "git-clone" ; Str url ; Str "" ; Str d ]
    | Wait { timeout = None } ->
        Lst [ Sym "wait" ]
    | Wait { timeout = Some t } ->
        Lst [ Sym "wait" ; Sym (string_of_float t) ]
    | Sequence { subcommands } ->
        Lst (Sym "sequence" ::
             (List.map (sexpr_of_command ?max_depth) subcommands))
    | Retry { subcommand ; up_to } ->
        Lst [ Sym "retry" ;
              sexpr_of_command ?max_depth subcommand ;
              Sym (string_of_int up_to) ]
    | Try { subcommand ; on_failure } ->
        Lst [ Sym "try" ;
              sexpr_of_command ?max_depth subcommand ;
              sexpr_of_command ?max_depth on_failure ]
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
  "(sequence (git-clone \"url\") (shell \"make\"))" \
    (linearize \
      (string_of_sexpr (Lst [ Sym "sequence" ; \
        Lst [ Sym "git-clone" ; Str "url" ] ; \
        Lst [ Sym "shell" ; Str "make" ] ])))
*)

(*$= string_of_command & ~printer:identity
  "(sequence (git-clone \"url\") (shell \"make\"))" \
    (linearize \
      (string_of_command (command_of_string \
        "(sequence (git-clone \"url\") (shell \"make\"))")))
*)
