(* Manage the representation of a command, as a possibly interactive widget to
 * display it when it's running or to edit it. *)
open Js_of_ocaml
open Vdom

module Api = LurchApiTypes
module Lang = LurchCommandLanguage
open LurchClientLib

type op_mode =
  | NotIsolated (* Any command is available but Exec *)
  | Isolation (* Only the isolation commands are permitted *)
  | Isolated (* Isolate command is no longer an option, but Exec is *)

let string_of_op_mode = function
  | NotIsolated -> "NotIsolated"
  | Isolation -> "Isolation"
  | Isolated -> "Isolated"

(* TODO: also pass a set of commands which help has already been printed to avoid
 * re-printing it unnecessarily *)
(* [depth] is the level of looping construct one can break from. This is always
 * possible to break from the running program if [depth] is 0. *)
let rec edit_as_form ~depth ~op_mode ?(editable=true) ?dom_id command =
  (* Editable items have an id prefix of [dom_id] and suffix of [suff]: *)
  let id suff = option_map (fun pref -> pref ^"/"^ suff) dom_id in
  let refresh_msg ?add_input ?rem_input () =
    `RefreshProgram (dom_id, add_input, rem_input) in
  (* Start with a combo to select the command: *)
  let command_labels =
    if editable then
      match op_mode with
      | NotIsolated ->
          [ "isolate" ;
            "no-op" ; "approve" ; "sequence" ; "retry" ; "if" ; "pause" ;
            "wait" ; "let" ; "spawn" ; "for" ; "break" ]
      | Isolation ->
          [ "chroot" ; "docker" ; "let" ]
      | Isolated ->
          [ "no-op" ; "exec" ; "approve" ; "sequence" ; "retry" ; "if" ;
            "pause" ; "wait" ; "let" ; "for" ; "break" ]
    else [
      (* This [edit] function can also be used to view any command out of
       * context so spare the console from spurious error messages in that
       * case: *)
      "isolate" ; "chroot" ; "docker" ; "no-op" ; "exec" ; "approve" ;
      "sequence" ; "retry" ; "if" ; "pause" ; "wait" ; "let" ; "spawn" ;
      "for" ; "break"
    ] in
  let label_of_operation op =
    let l = Api.Command.name_of_operation op in
    if not (List.mem l command_labels) then
      log ("not in command_labels: "^ l ^" while op_mode="^
           string_of_op_mode op_mode) ;
    assert (List.mem l command_labels) ;
    l in
  let op_selection =
    let id = id "op" in
    if List.length command_labels = 1 then (
      (* Still check that it's the right one: *)
      let l = Api.Command.name_of_operation command.Api.Command.operation in
      assert (List.mem l command_labels) ;
      (* Do not offer to choose the command if there is only one possible: *)
      input_hidden ?id (List.hd command_labels)
    ) else (
      let selected = label_of_operation command.Api.Command.operation in
      let a =
        match dom_id with
        | None -> []
        (* When an operation is changed, the form for this branch of the
         * command must be redrawn. *)
        | Some dom_id ->
            [ onchange_index (fun _ -> refresh_msg ()) ] in
      select ~a ~editable ?id ~selected command_labels
    ) in
  div [
    op_selection ;
    (* Then the arguments of that specific operand: *)
    (match command.operation with
    | Api.Command.Nop { exit_code } ->
        div [
          p [ text "No-op does nothing but return the specified exit code." ] ;
          input_text ?id:(id "exit_code") ~label:"Exit code:" ~editable
            (string_of_int exit_code) ]
    | Isolate { builder ; subcommand } ->
        div [
          p [ text "This is the required first command, and will build an isolated \
                    environment in which to run some more commands." ] ;
          h3 [ text "Isolation type" ] ;
          edit_as_form ~depth ~op_mode:Isolation ~editable ?dom_id:(id "builder")
            builder ;
          h3 [ text "Isolated command" ] ;
          edit_as_form ~depth ~op_mode:Isolated ~editable ?dom_id:(id "subcommand")
            subcommand ]
    | Chroot { template } ->
        div [
          p [ text "Chroot-based isolation will initially populate the chroot with \
                    busybox." ] ;
          (* TODO: other templates *)
          input_text ?id:(id "template") ~label:"Template:" ~editable
            ~placeholder:"template…" template ]
    | Docker { image } ->
        div [
          p [ text "Docker-based isolation will run in any specified image." ] ;
          input_text ?id:(id "image") ~label:"Image:" ~editable
            ~placeholder:"image…" image ]
    | Exec { pathname ; args ; env ; timeout } ->
        div [
          p [ text "Executes any program with the given arguments and environment. \
                    A minimal argument vector and environment are provided by \
                    default. The program will be forcibly terminated, and that \
                    command considered a failure, if it's not finished after \
                    the given timeout expires." ] ;
          p [ input_text ?id:(id "pathname") ~label:"Executable:" ~editable
                pathname ] ;
          input_text_multi ?id:(id "args") ~editable ~label:"Arguments:"
            ~on_add:(refresh_msg ~add_input:(id "args", "") ())
            ~on_rem:(fun i -> refresh_msg ~rem_input:(id "args", i) ())
            (Array.to_list args) ;
          input_text_multi ?id:(id "env") ~editable ~label:"Environment:"
            ~on_add:(refresh_msg ~add_input:(id "env", "") ())
            ~on_rem:(fun i -> refresh_msg ~rem_input:(id "env", i) ())
            ~placeholder:"NAME=VALUE" (Array.to_list env) ;
          p [ input_text ?id:(id "timeout") ~label:"Timeout:" ~units:"seconds"
                ~editable ~placeholder:"seconds…"
                (option_map_default "" string_of_float timeout) ] ]
    | Approve { timeout ; comment ; autosuccess } ->
        div [
          p [ text "Wait for a manual approval before proceeding. \
                    If no confirmation is received before the optional \
                    timeout expires then either the execution proceed or the
                    program fails, depending on the auto-success flag." ] ;
          div [
            p [ text "Text for user:" ] ;
            textarea ?id:(id "comment") ~editable [ text comment ] ] ;
          p [ input_text ?id:(id "timeout") ~label:"Timeout:" ~units:"seconds"
                ~editable ~placeholder:"seconds…"
                (option_map_default "" string_of_float timeout) ] ;
          p [ radios ?id:(id "autosuccess") ~label:"On timeout:" ~editable
                [ "proceed", "t" ; "fail", "f" ]
                (Lang.sql_string_of_bool autosuccess) ] ]
    | Let { var_name ; default ; subcommand ; comment } ->
        div [
          p [ text "Variable entered by the user that will then be substituted
                    within every subcommand parameters." ] ;
          div [
            p [ text "Text for user:" ] ;
            textarea ?id:(id "comment") ~editable [ text comment ] ] ;
          p [ input_text ?id:(id "var_name") ~label:"Variable Name:"
                ~editable var_name ] ;
          p [ input_text ?id:(id "default") ~label:"Default Value:"
                ~editable default ] ;
          edit_as_form ~depth ~op_mode ~editable ?dom_id:(id "subcommand")
            subcommand ]
    | Sequence { subcommands } ->
        let lis =
          List.mapi (fun i subcommand ->
            li [
              edit_as_form ~depth ~op_mode ~editable ?dom_id:(id (string_of_int i))
                subcommand ]
          ) subcommands in
        let lis =
          if editable then lis @ [
            li [
              edit_as_form ~depth ~op_mode ~editable
                ?dom_id:(id (string_of_int (List.length subcommands)))
                { operation = Nop { exit_code = 0 } ; id = 0 } ] ]
          else lis in
        div [
          p [ text "Executes a sequence of command, one after the other." ] ;
          ol lis ]
    | Retry { subcommand ; up_to } ->
        div [
          p [ text "Execute the given subcommand up to a given number of times \
                    until it succeeds." ] ;
          h3 [ text "Command" ] ;
          edit_as_form ~depth ~op_mode ~editable ?dom_id:(id "subcommand")
            subcommand ;
          input_text ?id:(id "up_to") ~label:"Up to:" ~units:"times" ~editable
            ~placeholder:"number…" (string_of_int up_to) ]
    | If { condition ; consequent ; alternative } ->
        div [
          p [ text "Execute the condition, and then either the consequent \
                    or the alternative, depending on the success of the \
                    condition." ] ;
          h3 [ text "Condition" ] ;
          edit_as_form ~depth ~op_mode ~editable ?dom_id:(id "condition") condition ;
          h3 [ text "On Success" ] ;
          edit_as_form ~depth ~op_mode ~editable ?dom_id:(id "consequent") consequent ;
          h3 [ text "On Failure" ] ;
          edit_as_form ~depth ~op_mode ~editable ?dom_id:(id "alternative") alternative ]
    | Pause { duration } ->
        div [
          p [ text "Pause for the given amount of time before proceeding." ] ;
          input_text ?id:(id "duration") ~label:"Duration:" ~units:"seconds" ~editable
            ~placeholder:"seconds…" (string_of_float duration) ]
    | Wait { minute ; hour ; mday ; month ; wday } ->
        let input_time k label vals =
          input_text_multi ?id:(id k) ~editable ~label
            ~on_add:(refresh_msg ~add_input:(id k, "0") ())
            ~on_rem:(fun i -> refresh_msg ~rem_input:(id k, i) ())
            (List.map string_of_int vals) in
        div [
          p [ text "Wait for the next occurrence of a specific point in time \
                    (à la crontab) before proceeding." ] ;
          input_time "minute" "Minutes:" minute ;
          input_time "hour" "Hours:" hour ;
          input_time "mday" "Days of month:" mday ;
          input_time "month" "Months:" month ;
          input_time "wday" "Days of week (0=Sun):" wday ]
    | Spawn { program } ->
        div [
          p [ text "Start the specified program. Do not wait for its \
                    termination." ] ;
          input_text ?id:(id "program") ~label:"Program name:" ~editable
            ~placeholder:"enter the program name" program ]
    | ForLoop { var_name ; values ; subcommand } ->
        div [
          p [ text "Repeat the body for each specified value. A variable of the \
                    specified name will be made available within the body with \
                    the current value." ] ;
          input_text ?id:(id "var_name") ~label:"Variable Name:" ~editable
            var_name ;
          input_text_multi ?id:(id "values") ~editable ~label:"Values:"
            ~on_add:(refresh_msg ~add_input:(id "values", "") ())
            ~on_rem:(fun i -> refresh_msg ~rem_input:(id "values", i) ())
            (Array.to_list values) ;
          h3 [ text "Body" ] ;
          edit_as_form ~depth:(depth+1) ~op_mode ~editable
            ?dom_id:(id "subcommand") subcommand ]
    | Break { depth = d } ->
        div [
          p [ text "Exit the current loop (or program). If depth > 0, exit \
                    that many loops." ] ;
          let depths = List.init (depth + 1) string_of_int in
          select ~editable ?id:(id "depth")
                 ~selected:(string_of_int d) depths ]) ]

let edit ~editable ?dom_id command =
  edit_as_form ~depth:0 ~op_mode:NotIsolated ~editable ?dom_id command

(* The reverse of edit_as_form (fail with an exception)
 * When a value is missing a default is assumed. This is required when updating
 * the form when another operation is selected. Ideally, when a value is missing
 * it would be fetched from a cache of past values for that id: *)
let rec command_of_form_exc ?add_input ?rem_input document dom_id =
  let id suff = dom_id ^"/"^ suff in
  let opt_subcommand ?(def_op=Api.Command.Nop { exit_code = 0 }) suff =
    try command_of_form_exc ?add_input ?rem_input document (id suff)
    with e ->
      (* Will happen each time an operation is changed: *)
      Api.Command.{ operation = def_op ; id = 0 } in
  let value ?def suff =
    let id = id suff in
    match Js_browser.Document.get_element_by_id document id, def with
    | Some e, _ ->
        (* merely return this element's value: *)
        Js_browser.Element.value e
    | None, None -> failwith ("No such element: "^ id)
    | None, Some def -> def in
  let value_radio ?def suff =
    let id = id suff in
    match Js_browser.Document.get_element_by_id document id, def with
    | Some e, _ ->
        (* Radio groups come within a div: *)
        if Js_browser.Element.node_name e = "DIV" then (
          (* return the value of the checked radio: *)
          try
            Js_browser.Element.get_elements_by_tag_name e "input" |>
            array_find Js_browser.Element.checked |>
            Js_browser.Element.value
          with _ ->
            log "Cannot find any checked input" ;
            (match def with
            | Some v -> v
            | None -> failwith ("No checked value for: "^ id))
        ) else
          invalid_arg "value_radio"
    | None, None -> failwith ("No such element: "^ id)
    | None, Some def -> def in
  (* Specialized to get an optional list of strings build with
   * input_text_multi, optionally adding/removing an antry according to
   * add_input/rem_input. Note than add_input is a pair of id+value and
   * rem_input a pair of id+index: *)
  let value_strings suff =
    let id = id suff in
    match Js_browser.Document.get_element_by_id document id with
    | Some e ->
        (* Multi-strings come within a div: *)
        if Js_browser.Element.node_name e = "DIV" then (
          (* return all values present below that, as an array of strings: *)
          let inputs = Js_browser.Element.get_elements_by_tag_name e "input" in
          let inputs =
            array_filter (fun input ->
              Js_browser.Element.has_attribute input "type" &&
              Js_browser.Element.get_attribute input "type" = "text"
            ) inputs in
          let inputs = Array.map Js_browser.Element.value inputs in
          let inputs =
            match rem_input with
            | Some (Some id_, idx) when id_ = id ->
                array_filteri (fun i _ -> i <> idx) inputs
            | _ ->
                inputs in
          let inputs =
            match add_input with
            | Some (Some id_, def) when id_ = id ->
                Array.append inputs [| def |]
            | _ ->
                inputs in
          inputs
        ) else
          invalid_arg "value_strings"
    | None ->
        (* Default to an empty array *)
        [||] in
  let value_ints suff =
    value_strings suff |>
    Array.to_list |>
    List.map int_of_string in
  let value_opt suff =
    match value ~def:"" suff with
    | "" -> None
    | s -> Some s in
  let operation =
    (* There must be default value for all parameters for operator switch to
     * work, but not for the operator itself so that sequence know where to
     * stop *)
    match value "op" with
    | "no-op" ->
        let exit_code = value ~def:"0" "exit_code" in
        Api.Command.Nop { exit_code = int_of_string exit_code }
    | "isolate" ->
        let builder =
          opt_subcommand ~def_op:(Chroot { template="busybox" }) "builder"
        and subcommand = opt_subcommand "subcommand" in
        Api.Command.Isolate { builder ; subcommand }
    | "chroot" ->
        let template = value ~def:"busybox" "template" in
        Api.Command.Chroot { template }
    | "docker" ->
        let image = value ~def:"" "image" in
        Api.Command.Docker { image }
    | "exec" ->
        let pathname = value ~def:"" "pathname"
        and args = value_strings "args"
        and env = value_strings "env"
        and timeout = option_map float_of_string (value_opt "timeout") in
        Api.Command.Exec { pathname ; args ; env ; timeout }
    | "approve" ->
        let timeout = option_map float_of_string (value_opt "timeout")
        and comment = value ~def:"" "comment"
        and autosuccess =
          Lang.sql_bool_of_string (value_radio ~def:"t" "autosuccess") in
        Api.Command.Approve { timeout ; comment ; autosuccess }
    | "let" ->
        let subcommand = opt_subcommand "subcommand"
        and var_name = value ~def:"name" "var_name"
        and default = value ~def:"" "default"
        and comment = value ~def:"" "comment" in
        Api.Command.Let { subcommand ; var_name ; default ; comment }
    | "sequence" ->
        let subcommand i = command_of_form_exc ?add_input ?rem_input
                             document (id (string_of_int i)) in
        let rec loop lst i =
          match subcommand i with
          | exception _ ->
              List.rev lst
          | { operation = Api.Command.Nop { exit_code = 0 } ; _ } ->
              loop lst (i + 1)
          | s ->
              loop (s :: lst) (i + 1) in
        let subcommands = loop [] 0 in
        Api.Command.Sequence { subcommands }
    | "retry" ->
        let subcommand = opt_subcommand "subcommand"
        and up_to = int_of_string (value ~def:"1" "up_to") in
        Api.Command.Retry { subcommand ; up_to }
    | "if" ->
        let condition = opt_subcommand "condition"
        and consequent = opt_subcommand "consequent"
        and alternative = opt_subcommand "alternative" in
        Api.Command.If { condition ; consequent ; alternative }
    | "pause" ->
        let duration = float_of_string (value ~def:"0" "duration") in
        Api.Command.Pause { duration }
    | "wait" ->
        let minute = value_ints "minute"
        and hour = value_ints "hour"
        and mday = value_ints "mday"
        and month = value_ints "month"
        and wday = value_ints "wday" in
        Api.Command.Wait { minute ; hour ; mday ; month ; wday }
    | "spawn" ->
        let program = value ~def:"" "program" in
        Api.Command.Spawn { program }
    | "for" ->
        let var_name = value ~def:"" "var_name"
        and values = value_strings "values"
        and subcommand = opt_subcommand "subcommand" in
        Api.Command.ForLoop { var_name ; values ; subcommand }
    | "break" ->
        let depth = int_of_string (value ~def:"0" "depth") in
        Api.Command.Break { depth }
    | _ ->
        invalid_arg "command_of_form_exc" in
  Api.Command.{ id = 0 ; operation }

let command_of_form ?add_input ?rem_input document dom_id =
  try Some (command_of_form_exc ?add_input ?rem_input document dom_id)
  with e ->
    log (Printexc.to_string e) ;
    None

let rec view run =
  let config_txt = txt_span ~a:[class_ "command"] in
  let find_subruns subcmd_id =
    array_find_all (fun r ->
      r.Api.Run.command.id = subcmd_id
    ) run.Api.Run.children in
  (* This [view] function draws the commands when they are running.
   * If they are not running then just draw the read-only version of
   * the command editor instead: *)
  let view_subcommand subcmd =
    match find_subruns subcmd.Api.Command.id with
    | [] ->
        edit ~editable:false subcmd
    | [ subrun ] ->
        view subrun
    | subruns ->
        ol (List.map view subruns) in
  div [
    p [ text "(" ;
        txt_span ~a:[ class_ "click" ;
                      onclick (fun _ -> `GetRun (run.id, run.logs)) ]
          ("#" ^ string_of_int run.id) ;
        text ")" ] ;
    (* div (text "env is" :: List.map (fun (n,v) -> text (n ^"="^ v)) run.env) ; *)
    (match run.command.operation with
    | Nop { exit_code } ->
        text ("Returns exit code "^ string_of_int exit_code)
    | Isolate { builder ; subcommand } ->
        ol [
          li [
            div [
              text "Build an isolated environment with:" ;
              view_subcommand builder ] ] ;
          li [
            div [
              text "Then run the following command:" ;
              view_subcommand subcommand ] ] ]
    | Chroot { template } ->
        div [
          p (
            text "Build a chroot with " ::
            config_txt template ::
            text ". " ::
            match run.chroot_path with
            | None ->
                [ text "Chroot not created yet." ]
            | Some path ->
                [ text "Chroot path is " ; config_txt path ; text "." ]) ]
    | Docker { image } ->
        div [
          p (
            text "Build a docker instance from image " ::
            config_txt image ::
            text ". " ::
            match run.docker_instance with
            | None ->
                [ text "Instance not started yet." ]
            | Some instance ->
                [ text "Instance is " ; config_txt instance ; text "." ]) ]
    | Exec { pathname ; args ; env ; timeout } ->
        let txt_of_strings a =
          List.map (fun s ->
            config_txt (s ^" ")
          ) (Array.to_list a) in
        let args = txt_of_strings args
        and env = txt_of_strings env in
        div [
          p (
            [ text "Execute program: " ; config_txt pathname ;
              text (if args = [] then " with no arguments"
                                 else " with arguments: ") ] @ args @
              (if env = [] then [] else (text " in environment: " :: env))
          ) ;
          match timeout with
          | None ->
              no_elt
          | Some t ->
              p [ text "Time out after " ; config_txt (string_of_float t) ; text "." ] ]
    | Approve { comment } ->
        let comment = Api.Run.var_expand run.env comment in
        div [
          (match run.confirmation_msg with
          | None ->
              (* Still not confirmed: *)
              let msg_dom_id = "run_confirm_msg_" ^ string_of_int run.id in
              div (
                (if comment <> "" then [ p [ text comment ] ] else []) @
                (if run.stopped = None then [
                  p [ text "Waiting since " ;
                      text (date_of_ts run.created) ] ;
                  input_text ~label:"Leave a message:" ~id:msg_dom_id
                    ~placeholder:"message…" "" ;
                  button "Approve"
                    (`ConfirmCommand (run.id, msg_dom_id, run.top_run)) ]
                else []))
          | Some msg ->
              p (
                text "Confirmed " ::
                if msg <> "" then
                  [ text " with message: " ; text msg ]
                else
                  [ text " with no message" ])) ]
    | Let { subcommand ; comment ; var_name ; default } ->
        div [
          (match run.var_value with
          | None ->
              (* Still unset: *)
              let msg_dom_id = "run_var_value_" ^ string_of_int run.id in
              div (
                (if comment <> "" then [ p [ text comment ] ] else []) @
                (if run.stopped = None then [
                  input_text ~label:"Value:" ~id:msg_dom_id default ;
                  button "Set"
                    (`SetVariable (run.id, msg_dom_id, run.top_run)) ]
                else []))
          | Some value ->
              p [ text (var_name ^" is set to "^ value) ]) ;
          view_subcommand subcommand ]
    | Sequence { subcommands } ->
        let num_steps = List.length subcommands in
        div [
          p [ text ("Sequence of "^ string_of_int num_steps ^" steps:") ] ;
          ol (
            List.map (fun subcommand ->
              li [ view_subcommand subcommand ]
            ) subcommands) ]
    | Retry { up_to ; subcommand } ->
        div [
          p [ text "Retry up to " ;
              config_txt (string_of_int up_to) ;
              text " times." ] ;
          view_subcommand subcommand ]
    | If { condition ; consequent ; alternative } ->
        div [
          p [ text "If:" ] ;
          view_subcommand condition ;
          p [ text "Then:" ] ;
          view_subcommand consequent ;
          p [ text "Else:" ] ;
          view_subcommand alternative ]
    | Pause { duration } ->
        div [
          p [ text ("Pause for "^ string_of_float duration ^" seconds") ] ]
    | Wait { minute ; hour ; mday ; month ; wday } ->
        let disp_times label vals =
          let line =
            label ^": "^ String.concat ", " (List.map string_of_int vals) in
          li [ text line ] in
        div [
          p [ text "Wait until next occurrence of:" ] ;
          ul [
            disp_times "Minutes" minute ;
            disp_times "Hours" hour ;
            disp_times "Day of month" mday ;
            disp_times "Month" month ;
            disp_times "Day of week (0=Sun)" wday ] ]
    | Spawn { program } ->
        let program = Api.Run.var_expand run.env program in
        div [
          p [ text ("Spawn program "^ program ^".") ] ]
    | ForLoop { var_name ; values ; subcommand } ->
        div [
          p [ text ("For "^ var_name ^" in "^ array_join ", " values ^" do:") ] ;
          view_subcommand subcommand ]
    | Break { depth } ->
        div [
          p [ text ("Break out of "^ string_of_int depth ^" loops") ] ]) ;
    (match run.started, run.pid, run.stopped with
    | None, _, _ ->
        p ~a:[ class_ "status" ]
          [ text "Not started." ]
    | Some started, None, _ ->
        p ~a:[ class_ "status" ]
          [ text "Started at " ;
            text (date_of_ts started) ;
            text "." ]
    | Some started, Some pid, None ->
        p ~a:[ class_ "status" ]
          [ text "Started at " ;
            text (date_of_ts started) ;
            text " and running as pid " ;
            text (string_of_int pid) ;
            text "." ]
    | Some started, _, Some _ ->
        p ~a:[ class_ "status" ]
          [ text "Started at " ;
            text (date_of_ts started) ;
            text "." ]) ;
    (match run.stopped, option_map Api.ExitStatus.of_code run.exit_code with
    | None, _ ->
        p ~a:[ class_ "status" ]
          [ text "Not finished." ;
            button "Cancel" (`CancelRun run.id) ]
    | Some stopped, None ->
        p ~a:[ class_ "status" ]
          [ text "Stopped at " ;
            text (date_of_ts stopped) ;
            text "." ]
    | Some stopped, Some status ->
        div [
          p ~a:[ class_ "status" ]
            [ text "Stopped at " ;
              text (date_of_ts stopped) ;
              text "." ] ;
          stats run.stats_self run.stats_desc ;
          txt_span ~a:[ class_ (class_of_status status) ]
            (Api.ExitStatus.to_string status) ]) ]
