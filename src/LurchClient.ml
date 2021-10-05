open Js_of_ocaml
open Vdom

open LurchClientLib
module Api = LurchApiTypes
module Lang = LurchCommandLanguage
module Program = LurchClientProgram
module State = LurchClientState
module Views = LurchClientViews

(*
 * View
 *)

let view_of_location st =
  let open State in
  let waiting = st.State.waiting in
  match st.location with
  | ShowError e ->
      div [ p ~a:[class_ "error"] [ text e ] ]
  | ListPastRuns lst ->
      Views.list_past_runs ~waiting lst
  | ListProgramsAndRun lst ->
      Views.list_programs_and_run ~waiting lst
  | ShowProgram { program ; editable ; last_runs } ->
      Views.program_editor ~waiting ~editable program last_runs
  | ConfirmDeleteProgram { program } ->
      Views.program_confirm_deletion program
  | ShowRun { run ; more_logs_expected } ->
      Views.show_run ~waiting run st.selected_logs more_logs_expected
  | Test ->
      Views.test

let view st =
  div [
    Views.header st ;
    Views.menu st ;
    Views.spinner st ;
    view_of_location st ]

(*
 * Update
 *)

open Js_browser

(* Custom commands *)

(* This work around required by what appears to be a compiler bug, complaining that:
 * File "src/LurchClient.ml", line 208, characters 19-26:
 * Error: The record field url_del belongs to the type 'a Http_del
 *        but is mixed here with fields of type 'weak11 Http_del
 *               The type constructor Http_del would escape its scope *)
type 'msg http_del_p = { url : string ; callback : api_response -> 'msg }

type 'msg Vdom.Cmd.t +=
  | Initialize of (unit -> 'msg) (* hide actual type for 'msg *)
  | Http_get of { url : string ; callback : api_response -> 'msg }
  | Http_put of { url : string ; payload : string ; callback : api_response -> 'msg }
  | Http_del of 'msg http_del_p
  | After of int * 'msg

let lurch_url page params =
  List.fold_left (fun s (n, v) ->
    s ^ "&" ^ Url.urlencode n ^"="^ Url.urlencode v
  ) ("lurch?p="^ page) params

let oldest_top_run = function
  | State.ListPastRuns runs when Array.length runs > 0 ->
      Some runs.(Array.length runs - 1).top_run
  | State.ShowProgram { last_runs } when Array.length last_runs > 0 ->
      let oldest =
        Array.fold_left (fun oldest r ->
          if r.Api.ListPastRuns.created < oldest.Api.ListPastRuns.created then
            r else oldest
        ) last_runs.(0) last_runs in
      Some oldest.Api.ListPastRuns.top_run
  | _ ->
      None

let update =
  (* How many log lines to ask in one batch: *)
  let logs_limit = 500 in
  let debug = false in
  fun st -> function
  | `Test ->
      return State.{ st with location = Test }
  | `SetLocation location ->
      return State.{ st with location ; refresh_msg = None }
  | `MaybeRefresh refresh_msg ->
      let c =
        if st.refresh_msg = Some refresh_msg then
          [ Vdom.Cmd.echo refresh_msg ]
        else (
          log "Not refreshing because we moved away." ;
          []
        ) in
      return ~c st
  | `GetPastProgramRuns -> (* Fetch list of program past runs and display them: *)
      let params =
        match oldest_top_run st.State.location with
        | None -> []
        | Some oldest -> [ "oldest_top_run", string_of_int oldest ] in
      let ajax =
        Http_get { url = lurch_url "list_past_runs" params ;
                   callback = fun r -> `GotPastProgramRuns r } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some `GetPastProgramRuns }
  | `GotPastProgramRuns (Error e) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `GotPastProgramRuns (Ok res) ->
      if debug then log_js (Js_browser.JSON.parse res) ;
      let res = Api.ListPastRuns.array_of_json_string res in
      let res =
        match st.location with
        | ListPastRuns runs ->
            Array.append runs res
        | _ ->
            res in
      return State.{ st with location = ListPastRuns res ; waiting = false }
  | `GetProgramsAndRun ->
      let ajax =
        Http_get { url = lurch_url "list_programs" [] ;
                   callback = fun r -> `GotProgramsAndRun r } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some `GetProgramsAndRun }
  | `GotProgramsAndRun (Error e) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `GotProgramsAndRun (Ok res) ->
      if debug then log_js (Js_browser.JSON.parse res) ;
      let res = Api.ListPrograms.array_of_json_string res in
      return State.{ st with location = ListProgramsAndRun res ;
                             waiting = false }
  | `CreateProgram ->
      return State.{ st with
        location = ShowProgram { program = Program.init ;
                                 editable = true ; last_runs = [||] } ;
        refresh_msg = None }
  | `GetProgram name ->
      let ajax =
        Http_get { url = lurch_url "get_program" [ "program", name ] ;
                   callback = fun r -> `GotProgram r } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some (`GetProgram name) }
  | `GotProgram (Error e) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `GotProgram (Ok res) ->
      if debug then log_js (Js_browser.JSON.parse res) ;
      let res = Api.Program.of_json_string res in
      let program = Program.of_api res in
      return ~c:[Vdom.Cmd.echo (`GetLastRuns res.name) ]
        State.{ st with
          location = ShowProgram { program ; editable = false ;
                                   last_runs = [||] } ;
          waiting = false ;
          refresh_msg = Some (`GetProgram res.name) }
  | `GetLastRuns name ->
      let params =
        match oldest_top_run st.State.location with
        | None -> []
        | Some oldest -> [ "oldest_top_run", string_of_int oldest ] in
      let ajax =
        let params = ("program", name) :: params in
        Http_get { url = lurch_url "list_past_runs" params ;
                   callback = fun r -> `GotLastRuns (r, name) } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some (`GetLastRuns name) }
  | `GotLastRuns (Error e, _) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `GotLastRuns (Ok res, name) ->
      if debug then log_js (Js_browser.JSON.parse res) ;
      let res = Api.ListPastRuns.array_of_json_string res in
      (match st.State.location with
      | ShowProgram { program ; editable ; last_runs }
        when program.saved <> None &&
             (option_get program.saved).name = name ->
          return State.{ st with
            location = ShowProgram { program ; editable ;
                                     last_runs = Array.append last_runs res } ;
            waiting = false }
      | _ ->
          return State.{ st with waiting = false } (* used clicked away already *))
  | `RefreshProgram (dom_id, add_input, rem_input) ->
      (match Js_browser.Document.get_element_by_id document "program_name",
             LurchClientCommand.command_of_form ?add_input ?rem_input
              document "program_command" with
      | Some name, Some command ->
          let name = Element.value name in
          (match st.State.location with
          | ShowProgram { program ; editable ; last_runs } ->
              let program =
                Program.{ edited = Api.Program.{ name ; created = 0. ; command } ;
                          saved = program.saved } in
              let location = State.ShowProgram { program ; editable ; last_runs } in
              return State.{ st with location ; waiting = false }
          | _ ->
              return State.{ st with
                location = ShowError "Received a RefresProgram message while \
                                      not in ShowProgram view!?" ;
                waiting = false ; refresh_msg = None })
      | _ ->
          return State.{ st with
            location = ShowError "Cannot find program values while refreshing!?" ;
            waiting = false ;
            refresh_msg = None })
  | `SaveProgram prev_name -> (* prev_name is "" for new programs *)
      (match Js_browser.Document.get_element_by_id document "program_name",
             LurchClientCommand.command_of_form document "program_command" with
      | Some name, Some command ->
          let name = Element.value name in
          let payload =
            Api.Program.(to_json_string
              { name ; command ; created = 0. }) in
          let ajax =
            Http_put {
              url = lurch_url "save_program" [ "prev_name", prev_name ] ;
              payload ;
              callback = fun r -> `GotProgram r } in
          return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
      | _ ->
          return State.{ st with
            location = ShowError "Cannot find program values" ;
            waiting = false ;
            refresh_msg = None })
  | `DeleteProgram name ->
      let ajax =
        Http_del { url = lurch_url "del_program" [ "program", name ] ;
                   callback = fun r -> `DeletedProgram r } in
      return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
  | `DeletedProgram (Error e) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `DeletedProgram (Ok _) ->
      return ~c:[Vdom.Cmd.echo `GetPastProgramRuns]
        State.{ st with waiting = true }
  | `StartProgram name ->
      let ajax =
        Http_get { url = lurch_url "start_program" [ "program", name ] ;
                   callback = fun r -> `StartedProgram r } in
      return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
  | `StartedProgram (Error e) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `StartedProgram (Ok res) ->
      if debug then log_js (Js_browser.JSON.parse res) ;
      let run = Api.Run.of_json_string res in
          return ~c:[Vdom.Cmd.echo (`GetMoreLogs run)]
            State.{ st with location = ShowRun { run ; more_logs_expected = true } ;
                            waiting = false }
  | `GetRun (run_id, logs) ->
      let ajax =
        Http_get { url = lurch_url "get_run" [ "id", string_of_int run_id ] ;
                   callback = fun r -> `GotRun (r, logs) } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = None }
  | `GotRun (Error e, _) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `GotRun (Ok res, logs) ->
      if debug then log_js (Js_browser.JSON.parse res) ;
      let run = { (Api.Run.of_json_string res) with logs } in
      return ~c:[Vdom.Cmd.echo (`GetMoreLogs run)]
        State.{ st with location = ShowRun { run ; more_logs_expected = true } }
  | `CancelRun run_id ->
      let ajax =
        Http_get { url = lurch_url "cancel_run" [ "id", string_of_int run_id ] ;
                   callback = fun r -> `CancelledRun r } in
      return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
  | `CancelledRun (Ok _) ->
      (* If we are displaying runs then refresh otherwise keep calm: *)
      let c =
        match st.State.location with
        | ShowRun { run } ->
            (* Even if we are not displaying the very run that has been
             * cancelled it could still be part of the displayed run, so
             * refresh in any cases: *)
            [ Vdom.Cmd.echo (`GetRun (run.id, run.logs)) ]
        | ListPastRuns _ ->
            [ Vdom.Cmd.echo `GetPastProgramRuns ]
        | _ ->
            [] in
      return ~c st
  | `CancelledRun (Error e) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `GetMoreLogs run ->
      let offset = Array.length run.Api.Run.logs
      and limit = logs_limit in
      if debug then log ("GetMoreLogs: currently have "^ string_of_int offset ^" lines") ;
      let ajax =
        let params = [ "run", string_of_int run.id ;
                       "offset", string_of_int offset ;
                       "limit", string_of_int limit ] in
        Http_get { url = lurch_url "get_logs" params ;
                   callback = fun r -> `GotMoreLogs (r, run) } in
      (* Since those are happening regularly, allow user to navigate away: *)
      return ~c:[ajax] State.{ st with waiting = false }
  | `GotMoreLogs (Error e, _) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `GotMoreLogs (Ok res, top_run) ->
      let res = Api.LogLine.array_of_json_string res in
      let len = Array.length res in
      if debug then log ("Got "^ string_of_int len ^" log lines!") ;
      (match st.State.location with
      | ShowRun { run } when run.Api.Run.id = top_run.Api.Run.id ->
          let run = { run with logs = Array.append run.logs res } in
          let refresh_msg = `GetRun (run.id, run.logs) in
          let c, more_logs_expected =
            (* Keep asking for logs as long as the run is not complete or we
             * haven't got all log lines: *)
            if len >= logs_limit then
              [ Vdom.Cmd.echo (`GetMoreLogs run) ], true
            else if run.stopped = None then
              [ After (1000, `MaybeRefresh refresh_msg) ], true
            else
              [], false
          in
          return ~c
            State.{ st with
              location = ShowRun { run ; more_logs_expected } ;
              waiting = false ;
              refresh_msg = Some refresh_msg }
      | ShowRun { run ; _ } ->
          (* FIXME: why is it an error? Why not simply get this run's logs? *)
          let err_msg = string_of_int run.id ^" <> "^ string_of_int top_run.id in
          return State.{ st with location = ShowError err_msg ; waiting = false }
      | _ ->
          return st)
  | `SetLogsFds (selected, run) ->
      (match st.State.location with
      | ShowRun { run } when run.Api.Run.id = run.Api.Run.id ->
          return State.{ st with selected_logs = selected }
      | _ ->
          return st)
  | `ConfirmCommand (run_id, msg_dom_id, top_run) ->
      let open Js_browser in
      (match Document.get_element_by_id document msg_dom_id with
      | Some elmt ->
          let msg = Element.value elmt in
          let params = [ "run", string_of_int run_id ; "message", msg ] in
          let ajax =
            Http_get {
              url = lurch_url "approve" params ;
              callback = fun r -> `ConfirmedCommand (r, top_run) } in
          return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
      | _ ->
          log "Cannot find confirmation message value" ;
          return st)
  | `ConfirmedCommand (Error e, _) ->
      return State.{ st with location = ShowError e ; waiting = false }
  | `ConfirmedCommand (Ok res, top_run) ->
      (* Reload the run to see the confirmation: *)
      return ~c:[Vdom.Cmd.echo (`GetRun (top_run, [||]))]
        State.{ st with waiting = false }

let cmd_handler ctx = function
  | Initialize callback ->
      Vdom_blit.Cmd.send_msg ctx (callback ()) ;
      true
  | Http_put { url ; payload ; callback } ->
      run_http_put ~url ~payload ~callback:(fun s ->
        Vdom_blit.Cmd.send_msg ctx (callback s)) () ;
      true
  | Http_del { url ; callback } ->
      run_http_del ~url ~callback:(fun s ->
        Vdom_blit.Cmd.send_msg ctx (callback s)) () ;
      true
  | Http_get { url ; callback } ->
      run_http_get ~url ~callback:(fun s ->
        Vdom_blit.Cmd.send_msg ctx (callback s)) () ;
      true
  | After (n, msg) ->
      Js_browser.Window.set_timeout window (fun () ->
        Vdom_blit.Cmd.send_msg ctx msg) n |>
      ignore ;
      true
  | _ ->
      false

let run () =
  let vdom_app = ref None in
  let inject msg =
    Vdom_blit.process (option_get !vdom_app) msg
  in
  let where =
    match Document.get_element_by_id document "spa" with
    | Some e -> e
    | None -> assert false in
  let init =
    return (*~c:[Cmd.echo (Initialize (fun () -> return `GetPastProgramRuns))]*)
      State.{
        location = ListPastRuns [||] ;
        waiting = false ;
        refresh_msg = Some `GetPastProgramRuns ;
        selected_logs = [0; 1; 2] } in
  let a = app ~init ~view ~update () in
  let a = Vdom_blit.run a in
  vdom_app := Some a ;
  Vdom_blit.dom a |>
  Element.append_child where ;
  inject `GetProgramsAndRun

let () =
  Vdom_blit.(register (cmd {Cmd.f = cmd_handler})) ;
  Window.set_onload window run
