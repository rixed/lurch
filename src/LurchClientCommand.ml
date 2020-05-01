(* Manage the representation of a command, as a possibly interactive widget to
 * display it when it's running or to edit it. *)
open Js_of_ocaml
open Vdom

module Api = LurchApiTypes
module Lang = LurchCommandLanguage
open LurchClientLib

let edit ~editor ?dom_id command =
  edit_text ?id:dom_id editor (Lang.string_of_command command)

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
    | Wait { subcommand } ->
        div [
          (match run.confirmation_msg with
          | None ->
              (* Still not confirmed: *)
              let msg_dom_id = "run_confirm_msg_" ^ string_of_int run.id in
              div [
                p [ text "Waiting since " ;
                    text (date_of_ts run.created) ] ;
                input_text ~label:"Leave a message" ~a:[id_ msg_dom_id]
                  ~placeholder:"message…" "" ;
                button "Click to unblock"
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
        let num_steps = Array.length subcommands in
        div [
          p [ text ("Sequence of "^ string_of_int num_steps ^" steps:") ] ;
          ol (
            List.init num_steps (fun step ->
              li [ view_subcommand subcommands.(step) ])) ]
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
          view_subcommand on_failure ]) ;
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
