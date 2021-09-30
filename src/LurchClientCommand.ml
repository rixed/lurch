(* Manage the representation of a command, as a possibly interactive widget to
 * display it when it's running or to edit it. *)
open Js_of_ocaml
open Vdom

module Api = LurchApiTypes
module Lang = LurchCommandLanguage
open LurchClientLib

let edit_as_text ~editor ?dom_id command =
  edit_text ?id:dom_id editor (Lang.string_of_command command)

type form_level = TopLevel | Isolation | Normal

(* TODO: also pass a set of commands which help has already been printed to avoid
 * re-printing it unnecessarily *)
let rec edit_as_form ?(build_isolation=Normal) ~editor ?dom_id command =
  (* Editable items have an id prefix of [dom_id] and suffix of [suff]: *)
  let id suff = option_map (fun pref -> pref ^"/"^ suff) dom_id in
  (* Start with a combo to select the command: *)
  let command_labels =
    match build_isolation with
    | TopLevel ->
        [ "isolate" ]
    | Isolation ->
        [ "chroot" ; "docker" ]
    | Normal ->
        [ "nop" ; "shell" ; "git-clone" ; "approve" ; "sequence" ; "retry" ;
          "try" ; "pause" ] in
  let label_of_operation op =
    let l = Api.Command.name_of_operation op in
    if not (List.mem l command_labels) then
      log ("not in command_labels: "^ l) ;
    assert (List.mem l command_labels) ;
    l in
  let a =
    if editor then []
    else [ disabled true ] in
  let op_selection =
    let id = id "op" in
    if List.length command_labels = 1 then
      input_hidden ~a ?id (List.hd command_labels)
    else
      let selected = label_of_operation command.Api.Command.operation in
      let a =
        match dom_id with
        | None -> a
        (* When an operation is changed, the form for this branch of the
         * command must be redrawn. *)
        | Some dom_id ->
            onchange_index (fun _ -> `RefreshProgram dom_id) :: a in
      select ~a ?id ~selected command_labels in
  div [
    op_selection ;
    (* Then the arguments of that specific operand: *)
    (match command.operation with
    | Api.Command.Nop ->
        p [ text "Nop has no argument" ] ; (* no_elt *)
    | Isolate { builder ; subcommand } ->
        div [
          p [ text "This is the required first command, and will build an isolated \
                    environment in which to run some more commands." ] ;
          h3 [ text "Isolation type" ] ;
          edit_as_form ~build_isolation:Isolation ~editor ?dom_id:(id "builder") builder ;
          h3 [ text "Isolated command" ] ;
          edit_as_form ~editor ?dom_id:(id "subcommand") subcommand ]
    | Chroot { template } ->
        div [
          p [ text "Chroot-based isolation will initially populate the chroot with \
                    busybox." ] ;
          (* TODO: other templates *)
          input_text ?id:(id "template") ~label:"Template:" ~a
            ~placeholder:"template…" template ]
    | Docker { image } ->
        div [
          p [ text "Docker-based isolation will run in any specified image." ] ;
          input_text ?id:(id "image") ~label:"Image:" ~a ~placeholder:"image…" image ]
    | Shell { line ; timeout } ->
        div [
          p [ text "Executes any shell command using /bin/sh with a minimal \
                    environment (PATH, USER). The command will be forcibly \
                    terminated, and that command considered a failure, if it's \
                    not finished after the given timeout." ] ;
          p [ input_text ?id:(id "line") ~label:"Command:" ~a ~placeholder:"shell…" line ] ;
          p [ input_text ?id:(id "timeout") ~label:"Timeout:" ~units:"seconds" ~a
                ~placeholder:"seconds…" (option_map_default "" string_of_float timeout) ] ]
    | GitClone { url ; revision ; directory } ->
        div [
          p [ text "Clones a git directory and checkout a given revision." ] ;
          p [ input_text ?id:(id "url") ~label:"Repository:" ~a
                ~placeholder:"url…" url ] ;
          p [ input_text ?id:(id "revision") ~label:"Revision:" ~a
                ~placeholder:"master…" (revision |? "") ] ;
          p [ input_text ?id:(id "directory") ~label:"Directory:" ~a
                (directory |? "") ] ]
    | Approve { subcommand ; timeout ; comment ; autosuccess } ->
        div [
          p [ text "Wait for a manual approval before running the given \
                    subcommand. If no confirmation is received before the optional \
                    timeout expires then either the execution proceed or the
                    program fails, depending on the auto-success flag." ];
          div [
            p [ text "Text for user:" ] ;
            textarea ?id:(id "comment") ~a [ text comment ] ] ;
          p [ input_text ?id:(id "timeout") ~label:"Timeout:" ~units:"seconds"
                ~a ~placeholder:"seconds…"
                (option_map_default "" string_of_float timeout) ] ;
          p [ radios ?id:(id "autosuccess") ~label:"On timeout:" ~a
                [ "proceed", "t" ; "fail", "f" ]
                (Lang.sql_string_of_bool autosuccess) ] ;
          edit_as_form ~editor ?dom_id:(id "subcommand") subcommand ]
    | Sequence { subcommands } ->
        let lis =
          List.mapi (fun i subcommand ->
            li [
              edit_as_form ~editor ?dom_id:(id (string_of_int i)) subcommand ]
          ) subcommands in
        let lis =
          if editor then lis @ [
            li [
              edit_as_form ~editor
                ?dom_id:(id (string_of_int (List.length subcommands)))
                { operation = Nop ; id = 0 } ] ]
          else lis in
        div [
          p [ text "Executes a sequence of command, one after the other." ] ;
          ol lis ]
    | Retry { subcommand ; up_to } ->
        div [
          p [ text "Execute the given subcommand up to a given number of times \
                    until it succeeds." ] ;
          h3 [ text "Command" ] ;
          edit_as_form ~editor ?dom_id:(id "subcommand") subcommand ;
          input_text ?id:(id "up_to") ~label:"Up to:" ~units:"times" ~a
            ~placeholder:"number…" (string_of_int up_to) ]
    | Try { subcommand ; on_failure } ->
        div [
          p [ text "Execute the given subcommand. If it fails, also executes \
                    the fallback subcommand." ] ;
          h3 [ text "Command" ] ;
          edit_as_form ~editor ?dom_id:(id "subcommand") subcommand ;
          h3 [ text "Fallback" ] ;
          edit_as_form ~editor ?dom_id:(id "on_failure") on_failure ]
    | Pause { duration ; subcommand } ->
        div [
          p [ text "Pause for the given amount of time before starting the \
                    specified subcommand." ] ;
          input_text ?id:(id "duration") ~label:"Duration:" ~units:"seconds" ~a
            ~placeholder:"seconds…" (string_of_float duration) ;
          h3 [ text "Command" ] ;
          edit_as_form ~editor ?dom_id:(id "subcommand") subcommand ]) ]

let edit ~editor ?dom_id command =
  edit_as_form ~build_isolation:TopLevel ~editor ?dom_id command

(* The reverse of edit_as_form (fail with an exception)
 * When a value is missing a default is assumed. This is required when updating
 * the form when another operation is selected. Ideally, when a value is missing
 * it would be fetched from a cache of past values for that id: *)
let rec command_of_form_exc document dom_id =
  let id suff = dom_id ^"/"^ suff in
  let opt_subcommand suff =
    try command_of_form_exc document (id suff)
    with _ -> Api.Command.{ operation = Nop ; id = 0 } in
  let value ?def suff =
    let id = id suff in
    match Js_browser.Document.get_element_by_id document id, def with
    | Some e, _ ->
        (* If this is a div then we have a radio group: *)
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
        ) else (
          (* merely return this element's value: *)
          Js_browser.Element.value e
        )
    | None, None -> failwith ("No such element: "^ id)
    | None, Some def -> def in
  let value_opt suff =
    match value ~def:"" suff with
    | "" -> None
    | s -> Some s in
  let operation =
    (* There must be default value for all parameters for operator switch to
     * work, but not for the operator itself so that sequence know where to
     * stop *)
    match value "op" with
    | "nop" ->
        Api.Command.Nop
    | "isolate" ->
        let builder = opt_subcommand "builder"
        and subcommand = opt_subcommand "subcommand" in
        Api.Command.Isolate { builder ; subcommand }
    | "chroot" ->
        let template = value ~def:"busybox" "template" in
        Api.Command.Chroot { template }
    | "docker" ->
        let image = value ~def:"" "image" in
        Api.Command.Docker { image }
    | "shell" ->
        let line = value ~def:"" "line"
        and timeout = option_map float_of_string (value_opt "timeout") in
        Api.Command.Shell { line ; timeout }
    | "git-clone" ->
        let url = value ~def:"" "url"
        and revision = value_opt "revision"
        and directory = value_opt "directory" in
        Api.Command.GitClone { url ; revision ; directory }
    | "approve" ->
        let subcommand = opt_subcommand "subcommand"
        and timeout = option_map float_of_string (value_opt "timeout")
        and comment = value ~def:"" "comment"
        and autosuccess = Lang.sql_bool_of_string (value ~def:"t" "autosuccess") in
        Api.Command.Approve { subcommand ; timeout ; comment ; autosuccess }
    | "sequence" ->
        let subcommand i =
          command_of_form_exc document (id (string_of_int i)) in
        let rec loop lst i =
          match subcommand i with
          | exception _ -> List.rev lst
          | { operation = Api.Command.Nop ; _ } -> loop lst (i + 1)
          | s -> loop (s :: lst) (i + 1) in
        let subcommands = loop [] 0 in
        Api.Command.Sequence { subcommands }
    | "retry" ->
        let subcommand = opt_subcommand "subcommand"
        and up_to = int_of_string (value ~def:"1" "up_to") in
        Api.Command.Retry { subcommand ; up_to }
    | "try" ->
        let subcommand = opt_subcommand "subcommand"
        and on_failure = opt_subcommand "on_failure" in
        Api.Command.Try { subcommand ; on_failure }
    | "pause" ->
        let subcommand = opt_subcommand "subcommand"
        and duration = float_of_string (value ~def:"0" "duration") in
        Api.Command.Pause { duration ; subcommand }
    | _ ->
        invalid_arg "command_of_form_exc" in
  Api.Command.{ id = 0 ; operation }

let command_of_form document dom_id =
  try Some (command_of_form_exc document dom_id)
  with e ->
    log (Printexc.to_string e) ;
    None

let rec view run =
  let config_txt = txt_span ~a:[class_ "command"] in
  let find_subrun subcmd_id =
    array_find (fun r ->
      r.Api.Run.command.id = subcmd_id
    ) run.Api.Run.children in
  let view_subcommand subcmd =
    match find_subrun subcmd.Api.Command.id with
    | exception Not_found ->
        edit ~editor:false subcmd
    | subrun ->
        view subrun in
  div [
    p [ text "(" ;
        txt_span ~a:[ class_ "click" ;
                      onclick (fun _ -> `GetRun (run.id, run.logs)) ]
          ("#" ^ string_of_int run.id) ;
        text ")" ] ;
    (match run.command.operation with
    | Nop ->
        text "No Operation"
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
    | Shell { line ; timeout } ->
        div [
          p (
            text "Execute shell command " ::
            config_txt line ::
            match timeout with
            | None ->
                [ text "." ]
            | Some t ->
                [ text " and time out after " ;
                  config_txt (string_of_float t) ;
                  text "." ]) ]
    | GitClone { url ; revision ; directory } ->
        div [
          p (
            text "Git-clone repository " ::
            config_txt url ::
            (match revision with
            | None ->
                []
            | Some rev ->
                [ text " at revision " ; config_txt rev ; text "." ]) @
            (match directory with
            | None ->
                []
            | Some dir ->
                [ text " into directory " ; config_txt dir ; text "." ])) ]
    | Approve { subcommand ; comment } ->
        div [
          (match run.confirmation_msg with
          | None ->
              (* Still not confirmed: *)
              let msg_dom_id = "run_confirm_msg_" ^ string_of_int run.id in
              div [
                (if comment <> "" then p [ text comment ] else no_elt) ;
                p [ text "Waiting since " ;
                    text (date_of_ts run.created) ] ;
                input_text ~label:"Leave a message:" ~id:msg_dom_id
                  ~placeholder:"message…" "" ;
                button "Approve"
                  (`ConfirmCommand (run.id, msg_dom_id, run.top_run)) ]
          | Some msg ->
              p (
                text "Confirmed " ::
                if msg <> "" then
                  [ text " with message: " ; text msg ]
                else
                  [ text " with no message" ])) ;
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
    | Try { subcommand ; on_failure } ->
        div [
          p [ text "try:" ] ;
          view_subcommand subcommand ;
          p [ text "on failure:" ] ;
          view_subcommand on_failure ]
    | Pause { duration ; subcommand } ->
        div [
          p [ text ("Pause for "^ string_of_float duration ^" seconds, then:") ] ;
          view_subcommand subcommand ]) ;
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
    (match run.stopped, run.exit_status with
    | None, _ ->
        p ~a:[ class_ "status" ]
          [ text "Not finished." ]
    | Some stopped, None ->
        p ~a:[ class_ "status" ]
          [ text "Stopped at " ;
            text (date_of_ts stopped) ;
            text "." ]
    | Some stopped, Some 0 ->
        p ~a:[ class_ "status" ]
          [ text "Stopped at " ;
            text (date_of_ts stopped) ;
            text "." ; br () ;
            txt_span ~a:[ class_ "success" ] "Success" ]
    | Some stopped, Some s when s > 0 ->
        p ~a:[ class_ "status" ]
          [ text "Stopped at " ;
            text (date_of_ts stopped) ;
            text "." ; br () ;
            txt_span ~a:[ class_ "failure" ]
              ("Failed with code "^ string_of_int s) ]
    | Some stopped, Some s ->
        p ~a:[ class_ "status" ]
          [ text "Stopped at " ;
            text (date_of_ts stopped) ;
            text "." ; br () ;
            txt_span ~a:[ class_ "filled" ]
              ("Killed with signal "^ string_of_int s) ]) ]
