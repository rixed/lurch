open Js_of_ocaml
open Vdom

module Api = LurchApiTypes
module Command = LurchClientCommand
module Lang = LurchCommandLanguage
module Program = LurchClientProgram
module State = LurchClientState
module Version = LurchVersion
open LurchClientLib

let header _st =
  p ~a:[ class_ "header" ]
    [ txt_span ~a:[ class_ "title" ] "Lurch" ;
      text " " ;
      txt_span ~a:[ class_ "version" ] Version.release_tag ;
      text " - " ;
      a_ "https://github.com/rixed/lurch" "source" ]

let menu _st =
  ul ~a:[ class_ "menu" ] [
    li ~a:[onclick (fun _ -> `GetProgramsAndRun)]
      [ text "Programs" ] ;
    li ~a:[onclick (fun _ -> `GetPastProgramRuns)]
      [ text "Runs"] ]

let spinner st =
  p ~a:[ class_ "spinner" ] [
    if st.State.waiting then
      text "wait…"
    else match st.refresh_msg with
      | None -> no_elt
      | Some msg ->
          txt_span ~a:[ onclick (fun _ -> msg) ] "refresh" ]

let outcome exit_code =
  let color_of_status = function
    | Api.ExitStatus.Ok -> "green"
    | Warn -> "orange"
    | Err -> "red" in
  match exit_code with
  | Some code ->
      let status = Api.ExitStatus.of_code code in
      Api.ExitStatus.to_string status,
      color_of_status (Api.ExitStatus.err_level status)
  | None ->
      "waiting", ""

let bgcolor_of = function
  | "" -> no_attr
  | c -> attr "bgcolor" c

let list_past_runs runs =
  div [
    let columns =
      [ "run#" ; "program" ; "started" ; "stopped" ; "cpu" ; "mem" ; "outcome" ] in
    let header = simple_table_header columns in
    let footer =
      [ tr [
        td ~a:[colspan (List.length columns)] [
          button "more" `GetPastProgramRuns ;
          button "create a new program" `CreateProgram ] ] ] in
    let len = Array.length runs in
    table ~header ~footer (List.init len (fun i ->
      let r = runs.(len - i - 1) in
      let goto_prog = onclick (fun _ -> `GetProgram r.Api.ListPastRuns.name)
      and goto_run = onclick (fun _ -> `GetRun (r.top_run, [||])) in
      tr (
        td ~a:[ class_ "click" ; goto_run ]
          [ text ("#"^ string_of_int r.top_run) ] ::
        td ~a:[ class_ "click" ; goto_prog ]
          [ text r.name ] ::
        (if r.started = None then
          [ td ~a:[ class_ "click" ; goto_run ; colspan 2 ]
              [ text "waiting" ] ]
        else
          [ td ~a:[ class_ "click time" ; goto_run ]
              [ text (date_of_ts (option_get r.started)) ] ;
            td ~a:[ class_ "click time" ; goto_run ]
              [ text (date_of_tsn (r.stopped) |? "running") ] ]) @
        [
          td ~a:[ class_ "click" ; goto_run ] [
            text ((option_map string_of_float r.cpu_usr |? "n.a") ^" usr + "^
                  (option_map string_of_float r.cpu_sys |? "n.a") ^" sys") ] ;
          td ~a:[ class_ "click" ; goto_run ] [
            text ((option_map string_of_int r.mem_usr |? "n.a") ^" usr + "^
                  (option_map string_of_int r.mem_sys |? "n.a") ^" sys") ] ;
          let outcome, color = outcome r.exit_code in
          let bgcolor = bgcolor_of color in
          td ~a:[ class_ "click" ; goto_run ; bgcolor]
            [ text outcome ] ]))) ]

let list_programs_and_run programs =
  let header =
    simple_table_header [ "program" ; "last start" ; "last stop" ; "outcome" ] in
  let footer =
    [ tr [
        td ~a:[ colspan 4] [
          button "create a new program" `CreateProgram ] ] ] in
  let len = Array.length programs in
  table ~a:[ class_ "programs" ] ~header ~footer (List.init len (fun i ->
    let p = programs.(len - i -1) in
    tr (
      td ~a:[ class_ "click" ;
              onclick (fun _ -> `GetProgram p.Api.ListPrograms.name) ]
        [ text p.name ] ::
      match p.last_run with
      | None ->
          [ td ~a:[ colspan 3 ] [ text "not yet run" ] ]
      | Some last_run ->
          let goto_run = onclick (fun _ -> `GetRun (last_run, [||])) in
          (match p.last_start with
          | None ->
              [ td ~a:[ class_ "click" ; goto_run ; colspan 3 ]
                  [ text "waiting" ] ]
          | Some last_start ->
              td ~a:[ class_ "click time" ; goto_run ]
                [ text (date_of_ts last_start) ] ::
              match p.last_stop with
              | None ->
                  [ td ~a:[ class_ "click" ; goto_run ; colspan 2 ]
                      [ text "running" ] ]
              | Some last_stop ->
                  [ td ~a:[ class_ "click time" ; goto_run ]
                      [ text (date_of_ts last_stop) ] ;
                    let txt, color = outcome p.last_exit_code in
                    let bgcolor = bgcolor_of color in
                    td ~a:[ class_ "click" ; goto_run ; bgcolor ]
                      [ text txt ] ]))))

let program_editor program ~editable last_runs =
  let saved_name =
    match program.Program.saved with
    | None -> ""
    | Some s -> s.Api.Program.name in
  div [
    h2 [ text (
      match program.Program.saved with
      | None -> "New Program"
      | Some s -> "Program " ^ s.Api.Program.name) ] ;
    table [
      tr [
        td [ text "Name" ] ;
        td [
          input_text ~id:"program_name" ~editable ~a:[autofocus]
            ~placeholder:"enter a unique name..." program.edited.name ] ] ;
      tr [
        td [ text "Command" ] ;
        td [
          Command.edit ~editable ~dom_id:"program_command" program.edited.command ] ] ;
      tr [
        td ~a:[attr "collspan" "2"] [
          div (
            (if editable || saved_name = "" then
              []
            else
              [ button "Run" (`StartProgram saved_name) ]) @
            [ horiz_spacer ] @
            (if editable then
              let prev_name =
                match program.saved with Some saved -> saved.Api.Program.name
                                 | None -> "" in [
              button "Cancel"
                (`SetLocation (State.ShowProgram { program ; editable = false ;
                                                   last_runs })) ;
              button "Save" (`SaveProgram prev_name)
            ] else [
              button "Edit" (`SetLocation (State.ShowProgram
                { program ; editable = true ; last_runs })) ]) @
            (if saved_name <> "" then [
              button "Delete" (
                `SetLocation (State.ConfirmDeleteProgram { program }))
            ] else [])) ] ] ] ;
    if saved_name = "" then
      no_elt
    else div [
      (* List of past runs *)
      h2 [ text "Past Runs" ] ;
      let header =
        simple_table_header [ "started" ; "stopped" ; "outcome" ] in
      let footer =
        [ tr [
            td ~a:[colspan 3]
              [ button "more" (`GetLastRuns saved_name) ] ] ] in
      let len = Array.length last_runs in
      table ~header ~footer (List.init len (fun i ->
        let r = last_runs.(len - i - 1) in
        tr ~a:[ class_ "click" ;
                onclick (fun _ -> `GetRun (r.top_run, [||])) ] (
          if r.started = None then
            [ td ~a:[ colspan 2 ] [ text "waiting" ] ]
          else
            [ td ~a:[ class_ "time" ]
                [ text (date_of_ts (option_get r.started)) ] ;
              td ~a:[ class_ "time" ]
                [ text (date_of_tsn r.stopped |? "running") ] ] @
          [ let txt, color = outcome r.exit_code in
            let bgcolor = bgcolor_of color in
            td ~a:[ bgcolor ] [ text txt ] ]))) ] ]

let program_confirm_deletion program =
  (* Called only for saved programs: *)
  let program_name = (option_get program.Program.saved).Api.Program.name in
  div [
    p [ text ("Are you sure you want to delete program "^ program_name ^"?") ] ;
    p [ button "Confirm" (`DeleteProgram program_name) ] ]

let info label def =
  div ~a:[ class_ "info-pair" ] [
    txt_span ~a:[ class_ "info-label" ] label ;
    text ": " ; def ]

let show_run run more_logs_expected =
  div [
    h2 [ text ("Details of Run #"^ string_of_int run.Api.Run.id) ] ;
    div [
      (match run.program with
      | Some name ->
          info "For Program"
            (txt_span ~a:[ class_ "click" ;
                           onclick (fun _ -> `GetProgram name) ] name)
      | _ -> no_elt) ;
      (if run.top_run <> run.id then
        let label = "run #" ^ string_of_int run.top_run in
        info "Part of "
          (txt_span ~a:[ class_ "click" ;
                         onclick (fun _ -> `GetRun (run.top_run, [||]))] label)
      else no_elt) ;
      Command.view run ;
      match run.stopped, run.program with
      | Some _, Some name ->
          button "Run Again" (`StartProgram name)
      | _ ->
          button "Cancel" (`CancelRun run.id) ] ;
    h2 [ text "Logs" ] ;
    let header =
      simple_table_header [ "run" ; "file" ; "time" ; "text" ] in
    let footer =
      if run.stopped = None then
        [ tr [
          td ~a:[colspan 4]
            [ button "more" (`GetMoreLogs run) ] ] ]
      else [] in
    let len = Array.length run.logs in
    let last_tr =
      tr [
        td ~a:[ id_ "bottom" ; colspan 4 ; class_ "bottom-of-logs" ]
           [ text (if more_logs_expected then "more logs arriving..."
                                         else "-- end of logs --") ] ] in
    let rec trs acc i =
      (* Build the list in reverse order to ensure tail-rec: *)
      if i < 0 then acc else
      let l = run.logs.(i) in
      let acc =
        tr [
          td ~a:[ class_ "click" ;
                  onclick (fun _ -> `GetRun (l.run, [||])) ]
            (* Apply the "click" class on the text rather than the td to
             * decorate that text as a link: *)
            [ txt_span ~a:[ class_ "click" ] ("#"^ string_of_int l.run) ] ;
          td [ text (filename_of_fd l.fd) ] ;
          td ~a:[ class_ "time" ] [ text (date_of_ts l.time) ] ;
          td ~a:[ class_ "logline" ] (dom_of_ansi l.line) ] :: acc in
      trs acc (i - 1) in
    table ~a:[ class_ "logs" ] ~header ~footer (trs [ last_tr ] (len - 1)) ]

let test =
  div [
    text "Simplest:" ;
    div (dom_of_ansi "bla") ;
    text "Empty:" ;
    div (dom_of_ansi "") ;
    text "Some colors:" ;
    div (dom_of_ansi "this is \027[32mcolo\027[31mred\027[0m.") ;
    text "Some background change:" ;
    div (dom_of_ansi "this is \027[32;44mcolo\027[47;31mred\027[0m.") ;
    text "Example #1 from real logs:" ;
    div (dom_of_ansi "\027[1;32m19h15m18:\027[0m Max FPR test") ;
    text "Example #2 from real logs:" ;
    div (dom_of_ansi "\027[32;1mSUCCESS\027[0m") ;
    text "Example #3 from real logs:" ;
    div (dom_of_ansi "\027[1;32m17h28m23:\027[0m\027[1;34m groups/f: \027[0m: Starting...") ;
    text "Some unknown sequence:" ;
    div (dom_of_ansi "this is \027[1;2;3;4zINVALID") ;
    text "Short reset:" ;
    div (dom_of_ansi "this is \027[32mcolored\027[m and then not.") ;
  ]
