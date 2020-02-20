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

let view_of_location =
  let open State in
  function
  | Absent ->
      assert false (* Not for main view *)
  | ShowError e ->
      div [
        p ~a:[class_ "error"] [ text e ] ;
        button "Back" (`SetDialog Absent) ]
  | ListPastRuns lst ->
      Views.list_past_runs lst
  | ListProgramsAndRun lst ->
      Views.list_programs_and_run lst
  | EditProgram { program ; editor ; last_runs } ->
      Views.program_editor program editor last_runs
  | ConfirmDeleteProgram { program } ->
      Views.program_confirm_deletion program
  | ShowRun { run } ->
      Views.show_run run

let view st =
  div [
    Views.header st ;
    Views.menu st ;
    Views.spinner st ;
    match st.State.dialog with
    | State.Absent ->
        view_of_location st.location
    | dialog ->
        view_of_location dialog ]

(*
 * Update
 *)

open Js_browser

(* Custom commands *)
type 'msg Vdom.Cmd.t +=
  | Initialize of (unit -> 'msg) (* hide actual type for 'msg *)
  | Http_get of { url : string ; callback : api_response -> 'msg }
  | Http_put of { url : string ; payload : string ; callback : api_response -> 'msg }
  | Http_del of { url : string ; callback : api_response -> 'msg }
  | After of int * 'msg

let lurch_url page params =
  List.fold_left (fun s (n, v) ->
    s ^ "&" ^ Url.urlencode n ^"="^ Url.urlencode v
  ) ("lurch?p="^ page) params

let oldest_run = function
  | State.ListPastRuns runs when Array.length runs > 0 ->
      if Array.length runs = 0 then None
      else
        Some runs.(Array.length runs - 1).created
  | State.EditProgram { last_runs } ->
      Array.fold_left (fun oldest r ->
        match oldest, r.Api.ListPastRuns.created with
        | None, created -> Some created
        | Some oldest, created -> Some (min oldest created)
      ) None last_runs
  | _ ->
      None

let update st = function
  | `SetDialog dialog ->
      return State.{ st with dialog ; refresh_msg = None }
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
        match oldest_run st.State.location with
        | None -> []
        | Some f -> [ "from", string_of_float f ] in
      let ajax =
        Http_get { url = lurch_url "list_past_runs" params ;
                   callback = fun r -> `GotPastProgramRuns r } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some `GetPastProgramRuns }
  | `GotPastProgramRuns (Error e) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `GotPastProgramRuns (Ok res) ->
      log "Got PastProgramRuns!" ;
      log_js (Js_browser.JSON.parse res) ;
      let res = Api.ListPastRuns.array_of_json_string res in
      let res =
        match st.location with
        | ListPastRuns runs ->
            Array.append runs res
        | _ ->
            res in
      return State.{ st with location = ListPastRuns res ;
                             dialog = Absent ; waiting = false }
  | `GetProgramsAndRun ->
      let ajax =
        Http_get { url = lurch_url "list_programs" [] ;
                   callback = fun r -> `GotProgramsAndRun r } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some `GetProgramsAndRun }
  | `GotProgramsAndRun (Error e) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `GotProgramsAndRun (Ok res) ->
      log "Got ProgramsAndRun!" ;
      log_js (Js_browser.JSON.parse res) ;
      let res = Api.ListPrograms.array_of_json_string res in
      return State.{ st with location = ListProgramsAndRun res ;
                             dialog = Absent ; waiting = false }
  | `CreateProgram ->
      return State.{ st with
        dialog = EditProgram { program = Program.init ;
                               editor = true ; last_runs = [||] } ;
        refresh_msg = None }
  | `GetProgram name ->
      let ajax =
        Http_get { url = lurch_url "get_program" [ "program", name ] ;
                   callback = fun r -> `GotProgram r } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some (`GetProgram name) }
  | `GotProgram (Error e) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `GotProgram (Ok res) ->
      log "Got Program!" ;
      log_js (Js_browser.JSON.parse res) ;
      let res = Api.Program.of_json_string res in
      let program = Program.of_api res in
      return ~c:[Vdom.Cmd.echo (`GetLastRuns res.name) ]
        State.{ st with
          dialog = EditProgram { program ; editor = false ;
                                 last_runs = [||] } ;
          waiting = false ;
          refresh_msg = Some (`GetProgram res.name) }
  | `GetLastRuns name ->
      let from = oldest_run st.State.dialog in
      let params = match from with None -> []
                   | Some f -> [ "from", string_of_float f ] in
      let ajax =
        let params = ("program", name) :: params in
        Http_get { url = lurch_url "list_past_runs" params ;
                   callback = fun r -> `GotLastRuns (r, name) } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = Some (`GetLastRuns name) }
  | `GotLastRuns (Error e, _) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `GotLastRuns (Ok res, name) ->
      log "Got last runs!" ;
      log_js (Js_browser.JSON.parse res) ;
      let res = Api.ListPastRuns.array_of_json_string res in
      (match st.State.dialog with
      | EditProgram { program ; editor ; last_runs }
        when program.saved <> None &&
             (option_get program.saved).name = name ->
          return State.{ st with
            dialog = EditProgram { program ; editor ;
                                   last_runs = Array.append last_runs res } ;
            waiting = false }
      | _ ->
          return State.{ st with waiting = false } (* used clicked away already *))
  | `SaveProgram prev_name -> (* prev_name is "" for new programs *)
      let open Js_browser in
      (match Document.get_element_by_id document "program_name",
             Document.get_element_by_id document "program_command" with
      | Some name, Some command ->
          let name = Element.value name in
          let command = Element.value command |>
                        Lang.command_of_string in
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
            dialog = ShowError "Cannot find program values" ;
            waiting = false ; refresh_msg = None })
  | `DeleteProgram name ->
      let ajax =
        Http_del { url = lurch_url "del_program" [ "program", name ] ;
                   callback = fun r -> `DeletedProgram r } in
      return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
  | `DeletedProgram (Error e) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `DeletedProgram (Ok _) ->
      return ~c:[Vdom.Cmd.echo `GetPastProgramRuns]
        State.{ st with dialog = Absent ; waiting = false }
  | `StartProgram name ->
      let ajax =
        Http_get { url = lurch_url "start_program" [ "program", name ] ;
                   callback = fun r -> `StartedProgram r } in
      return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
  | `StartedProgram (Error e) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `StartedProgram (Ok res) ->
      log "Got run id!" ;
      log_js (Js_browser.JSON.parse res) ;
      let run = Api.Run.of_json_string res in
      return ~c:[Vdom.Cmd.echo (`GetMoreLogs run)]
        State.{ st with dialog = ShowRun { run } ; waiting = false }
  | `GetRun (run_id, logs) ->
      let ajax =
        Http_get { url = lurch_url "get_run" [ "id", string_of_int run_id ] ;
                   callback = fun r -> `GotRun (r, logs) } in
      return ~c:[ajax]
        State.{ st with waiting = true ; refresh_msg = None }
  | `GotRun (Error e, _) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `GotRun (Ok res, logs) ->
      log "Got run!" ;
      log_js (Js_browser.JSON.parse res) ;
      let run = { (Api.Run.of_json_string res) with logs } in
      return ~c:[Vdom.Cmd.echo (`GetMoreLogs run)]
        State.{ st with dialog = ShowRun { run } }
  | `GetMoreLogs top_run ->
      (* FIXME: since is always 0 when refreshing because this run has always
       * 0 lines then, whereas the `More` button work as expected.
       * Instead of auto-refreshing, make that more button use the timeout
       * on its own to do the right thing? *)
      let since =
        let len = Array.length top_run.Api.Run.logs in
        log ("GetMoreLogs: currently have "^ string_of_int len ^" lines") ;
        if len = 0 then 0. else top_run.logs.(len-1).Api.LogLine.time in
      let ajax =
        let params = [ "top_run", string_of_int top_run.id ;
                       "since", string_of_float since ] in
        Http_get { url = lurch_url "get_more_logs" params ;
                   callback = fun r -> `GotMoreLogs (r, top_run) } in
      return ~c:[ajax] State.{ st with waiting = true }
  | `GotMoreLogs (Error e, _) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `GotMoreLogs (Ok res, top_run) ->
      log "Got logs!" ;
      log_js (Js_browser.JSON.parse res) ;
      let res = Api.LogLine.array_of_json_string res in
      (match st.State.dialog with
      | ShowRun { run } when run.Api.Run.id = top_run.Api.Run.id ->
          (* Periodically recall GetRun: *)
          let refresh_msg = `GetRun (top_run.id, top_run.logs) in
          let run = { run with logs = Array.append run.logs res } in
          (* TODO: ajax call a long pull *)
          let c =
            (* FIXME: reloading more logs reset viewport to top of the page *)
            if false && run.stopped = None then
              [ After (1000, `MaybeRefresh refresh_msg) ]
            else
              []
          in
          return ~c
            State.{ st with dialog = ShowRun { run } ;
                      waiting = false ;
                      refresh_msg = Some refresh_msg }
      | ShowRun { run } ->
          let err_msg = string_of_int run.id ^" <> "^ string_of_int top_run.id in
          return State.{ st with dialog = ShowError err_msg ; waiting = false }
      | _ ->
          return st)
  | `ConfirmCommand (run_id, msg_dom_id, top_run) ->
      let open Js_browser in
      (match Document.get_element_by_id document msg_dom_id with
      | Some msg ->
          let msg = Element.value msg in
          let params = [ "run", string_of_int run_id ; "message", msg ] in
          let ajax =
            Http_get {
              url = lurch_url "wait_confirm" params ;
              callback = fun r -> `ConfirmedCommand (r, top_run) } in
          return ~c:[ajax] State.{ st with waiting = true ; refresh_msg = None }
      | _ ->
          return State.{ st with
            dialog = ShowError "Cannot find confirmation message value" ;
            waiting = false })
  | `ConfirmedCommand (Error e, _) ->
      return State.{ st with dialog = ShowError e ; waiting = false }
  | `ConfirmedCommand (Ok res, top_run) ->
      log "Confirmed!" ;
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
      State.{ location = ListPastRuns [||] ; dialog = Absent ;
              waiting = false ; refresh_msg = Some `GetPastProgramRuns } in
  let a = app ~init ~view ~update () in
  let a = Vdom_blit.run a in
  vdom_app := Some a ;
  Vdom_blit.dom a |>
  Element.append_child where ;
  inject `GetProgramsAndRun

let () =
  Vdom_blit.(register (cmd {Cmd.f = cmd_handler})) ;
  Window.set_onload window run
