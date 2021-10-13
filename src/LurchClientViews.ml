open Js_of_ocaml
open Vdom

module Api = LurchApiTypes
module Command = LurchClientCommand
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

let menu st =
  let oldest_top_run =
    match st.State.location with
    | ListPastRuns runs when Array.length runs > 0 ->
        Some runs.(Array.length runs - 1).Api.ListPastRuns.top_run
    | _ ->
        None in
  ul ~a:[ class_ "menu" ] [
    li ~a:(onclick_if_allowed st.waiting `GetProgramsAndRun)
      [ text "Programs" ] ;
    li ~a:(onclick_if_allowed st.waiting
             (`GetPastProgramRuns oldest_top_run))
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

(* Display past runs. [runs] must be given most recent first. *)
let list_past_runs ~single_program ~waiting ~on_more runs =
  let len = Array.length runs in
  (* For each column, for each program, compute the average and the stddev,
   * first by computing the sums, then the averages, then also the stddev: *)
  let avgs = Hashtbl.create 50 in
  Array.iter (fun r ->
    let k = r.Api.ListPastRuns.name in
    match Hashtbl.find avgs k with
    | exception Not_found ->
        Hashtbl.add avgs k (r.stats_self, r.stats_desc)
    | self, desc ->
        Hashtbl.replace avgs k (Api.RunStats.aggr self r.stats_self,
                                Api.RunStats.aggr desc r.stats_desc)
  ) runs ;
  Hashtbl.filter_map_inplace (fun _k (self, desc) ->
    Some Api.RunStats.(avg self, avg desc)
  ) avgs ;
  let stddevs = Hashtbl.create (Hashtbl.length avgs) in
  Array.iter (fun r ->
    let k = r.Api.ListPastRuns.name in
    let avg_self, avg_desc = Hashtbl.find avgs k in
    let dev_self = Api.RunStats.(square (sub r.stats_self avg_self))
    and dev_desc = Api.RunStats.(square (sub r.stats_desc avg_desc)) in
    match Hashtbl.find stddevs k with
    | exception Not_found ->
        Hashtbl.add stddevs k (dev_self, dev_desc)
    | self, desc ->
        Hashtbl.replace stddevs k (Api.RunStats.aggr self dev_self,
                                   Api.RunStats.aggr desc dev_desc)
  ) runs ;
  Hashtbl.filter_map_inplace (fun _k (self, desc) ->
    Some Api.RunStats.(sqrt (avg self), sqrt (avg desc))
  ) stddevs ;
  (* Build the table: *)
  let num_cols =
    if single_program then 12 else 13 in
  div [
    let header =
      [ tr (
          td ~a:[rowspan 4] [ text "run#" ] ::
          (let tds =
            [ td ~a:[rowspan 4] [ text "started" ] ;
              td ~a:[rowspan 4] [ text "stopped" ] ;
              td ~a:[colspan 8] [ text "resources" ] ;
              td ~a:[rowspan 4] [ text "outcome" ] ] in
          if single_program then tds
          else td ~a:[rowspan 4] [ text "program" ] :: tds)) ;
        tr [
          td ~a:[colspan 4] [ text "CPU" ] ;
          td ~a:[colspan 4] [ text "RAM" ] ] ;
        tr [
          td ~a:[colspan 2] [ text "self" ] ;
          td ~a:[colspan 2] [ text "children" ] ;
          td ~a:[colspan 2] [ text "self" ] ;
          td ~a:[colspan 2] [ text "children" ] ] ;
        tr [
          td [ text "usr" ] ;
          td [ text "sys" ] ;
          td [ text "usr" ] ;
          td [ text "sys" ] ;
          td [ text "usr" ] ;
          td [ text "sys" ] ;
          td [ text "usr" ] ;
          td [ text "sys" ] ] ] in
    let footer =
      [ tr [
          td ~a:[colspan num_cols] (
            [ button "more" on_more ] @ (
            if single_program then [] else
              [ horiz_spacer ;
                button "create a new program" `CreateProgram ])) ] ] in
    table ~header ~footer (List.init len (fun i ->
      let r = runs.(i) in
      let goto_prog = onclick_if_allowed waiting (`GetProgram r.name)
      and goto_run = onclick_if_allowed waiting (`GetRun (r.top_run, [||]))
      and avg_self, avg_desc = Hashtbl.find avgs r.name
      and stddev_self, stddev_desc = Hashtbl.find stddevs r.name in
      let td_stat to_string avg stddev v =
        let cls, txt, diff_txt =
          match avg, stddev, v with
          | Some avg, Some stddev, Some v ->
              let anomaly = 2. *. stddev
              and warning = stddev
              and diff = v -. avg in
              let diff_txt =
                if diff > warning || diff < ~-.warning then
                  let sigmas = diff /. stddev in
                  Printf.sprintf "(%+.1fσ)" sigmas
                else
                  "" in
              let cls =
                if diff > anomaly || diff < ~-.anomaly then "stat-anomaly" else
                if diff > warning || diff < ~-.warning then "stat-warning" else
                "stat-normal" in
              cls, to_string v, Some diff_txt
          | _, _, None ->
              "stat-na", "n.a.", None
          | _ ->
              (* If we had at least ont value then we must have been able to compute
               * the avg and stddev: *)
              assert false in
        td ~a:(class_ "click" :: goto_run)
          (span ~a:[class_ cls] [ text txt ] ::
          match diff_txt with
          | None ->
              []
          | Some diff_txt ->
              [ span ~a:[class_ (cls ^" stat-diff") ] [ text diff_txt ] ]) in
      tr (
        td ~a:(class_ "click" :: goto_run)
          [ text ("#"^ string_of_int r.top_run) ] ::
        (let tds =
          (if r.started = None then
            [ td ~a:(class_ "click" :: colspan 2 :: goto_run)
                [ text "waiting" ] ]
          else
            [ td ~a:(class_ "click time" :: goto_run)
                [ text (date_of_ts (option_get r.started)) ] ;
              td ~a:(class_ "click time" :: goto_run)
                [ text (date_of_tsn (r.stopped) |? "running") ] ]) @
          [ td_stat string_of_secs avg_self.cpu_usr stddev_self.cpu_usr r.stats_self.cpu_usr ;
            td_stat string_of_secs avg_self.cpu_sys stddev_self.cpu_sys r.stats_self.cpu_sys ;
            td_stat string_of_secs avg_desc.cpu_usr stddev_desc.cpu_usr r.stats_desc.cpu_usr ;
            td_stat string_of_secs avg_desc.cpu_sys stddev_desc.cpu_sys r.stats_desc.cpu_sys ;
            td_stat string_of_mem avg_self.mem_usr stddev_self.mem_usr r.stats_self.mem_usr ;
            td_stat string_of_mem avg_self.mem_sys stddev_self.mem_sys r.stats_self.mem_sys ;
            td_stat string_of_mem avg_desc.mem_usr stddev_desc.mem_usr r.stats_desc.mem_usr ;
            td_stat string_of_mem avg_desc.mem_sys stddev_desc.mem_sys r.stats_desc.mem_sys ;
            let outcome, color = outcome r.exit_code in
            let bgcolor = bgcolor_of color in
            td ~a:(class_ "click" :: bgcolor :: goto_run)
              [ text outcome ] ] in
        if single_program then tds else
          td ~a:(class_ "click" :: goto_prog) [ text r.name ] :: tds)))) ]

(* Display all programs in a single table, also displaying their last run *)
let list_programs_and_run ~waiting programs =
  let header =
    simple_table_header [ "program" ; "last start" ; "last stop" ; "outcome" ; "action" ] in
  let footer =
    [ tr [
        td ~a:[ colspan 5] [
          button "create a new program" `CreateProgram ] ] ] in
  let len = Array.length programs in
  table ~a:[ class_ "programs" ] ~header ~footer (List.init len (fun i ->
    let p = programs.(len - i -1) in
    tr (
      td ~a:(class_ "click" ::
            onclick_if_allowed waiting (`GetProgram p.Api.ListPrograms.name))
        [ text p.name ] ::
      (match p.last_run with
      | None ->
          [ td ~a:[ colspan 3 ] [ text "not yet run" ] ]
      | Some last_run ->
          let goto_run = onclick_if_allowed waiting (`GetRun (last_run, [||])) in
          (match p.last_start with
          | None ->
              [ td ~a:(class_ "click" :: colspan 3 :: goto_run)
                  [ text "waiting" ] ]
          | Some last_start ->
              td ~a:(class_ "click time" :: goto_run)
                [ text (date_of_ts last_start) ] ::
              match p.last_stop with
              | None ->
                  [ td ~a:(class_ "click" :: colspan 2 :: goto_run)
                      [ text "running" ] ]
              | Some last_stop ->
                  [ td ~a:(class_ "click time" :: goto_run)
                      [ text (date_of_ts last_stop) ] ;
                    let txt, color = outcome p.last_exit_code in
                    let bgcolor = bgcolor_of color in
                    td ~a:(class_ "click" :: bgcolor :: goto_run)
                      [ text txt ] ])) @
      [ td ~a:(class_ "click" :: onclick_if_allowed waiting (`StartProgram p.name))
          [ text "run" ] ])))

(* Display the program editor followed by the last runs.
 * [last_runs] must be ordered most recent first. *)
let program_editor ~waiting ~editable program last_runs current_location =
  let len = Array.length last_runs in
  let oldest_top_run =
    if len = 0 then None
    else Some last_runs.(len - 1).Api.ListPastRuns.top_run in
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
        td ~a:[colspan 2] [
          p (
            (if editable || saved_name = "" then
              []
            else
              [ button "Run" (`StartProgram saved_name) ;
                horiz_spacer ]) @
            (match editable, program.saved with
            | true, Some saved ->
                [ button "Cancel"
                    (* FIXME: Neither redirecting to ShowProgram or fetching again
                     * with (`GetProgram saved.name) does refresh the input values
                     * properly.
                     * For some reason, even though the html contains the proper
                     * value the displayed text is still the old one. Bug in the
                     * diff? *)
                    (`SetLocation (State.ShowProgram {
                      program = { program with edited = saved } ;
                      editable = false ; last_runs })) ;
                  horiz_spacer ]
            | _ ->
                (* If not editable or if this is a new program, skip the cancel
                 * button: *)
                []) @
            [
              (if editable then
                let prev_name =
                  match program.saved with Some saved -> saved.Api.Program.name
                                         | None -> "" in
                button "Save" (`SaveProgram prev_name)
              else
                button "Edit" (`SetLocation (State.ShowProgram
                  { program ; editable = true ; last_runs }))) ;
              horiz_spacer
            ] @
            (if saved_name <> "" then [
              button "Delete" (
                `SetLocation (State.ConfirmDeleteProgram {
                  program ; back = current_location }))
            ] else [])) ] ] ] ;
    if saved_name = "" || Array.length last_runs = 0 then
      no_elt
    else div [
      (* List of past runs *)
      h2 [ text "Past Runs" ] ;
      list_past_runs ~single_program:true ~waiting
        ~on_more:(`GetLastRuns (saved_name, oldest_top_run)) last_runs ] ]

let program_confirm_deletion program back =
  (* Called only for saved programs: *)
  let program_name = (option_get program.Program.saved).Api.Program.name in
  div [
    p [ text ("Are you sure you want to delete program "^ program_name ^"?") ] ;
    p [ button "Cancel" (`SetLocation back) ;
        horiz_spacer ;
        button "Confirm" (`DeleteProgram program_name) ] ]

let info label def =
  div ~a:[ class_ "info-pair" ] [
    txt_span ~a:[ class_ "info-label" ] label ;
    text ": " ; def ]

let show_run ~waiting run selected_logs more_logs_expected =
  div [
    h2 [ text ("Details of Run #"^ string_of_int run.Api.Run.id) ] ;
    div [
      (match run.program with
      | Some name ->
          info "For Program"
            (txt_span ~a:(class_ "click" ::
                          onclick_if_allowed waiting (`GetProgram name)) name)
      | _ -> no_elt) ;
      (if run.top_run <> run.id then
        let label = "run #" ^ string_of_int run.top_run in
        info "Part of "
          (txt_span ~a:(class_ "click" ::
                        onclick_if_allowed waiting (`GetRun (run.top_run, [||]))) label)
      else no_elt) ;
      Command.view run ;
      match run.stopped, run.program with
      | Some _, Some name ->
          button "Run Again" (`StartProgram name)
      | _ ->
          button "Cancel" (`CancelRun run.id) ] ;
    h2 [ text "Logs" ] ;
    (* TODO: also have a date selector, and a choice to display absolute or
     * relative times: *)
    div [
      let fds =
        Array.fold_left (fun l line ->
          let fd = line.Api.LogLine.fd in
          if List.mem fd l then l else fd :: l
        ) [] run.logs |>
        List.map filename_of_fd in
      let selection = List.map filename_of_fd selected_logs in
      p [ text "show:" ;
          checkboxes ~action:(fun fn checked ->
            let fd = fd_of_filename fn in
            let selected_logs =
              if checked && not (List.mem fd selected_logs) then
                fd :: selected_logs else
              if not checked && List.mem fd selected_logs then
                List.filter ((<>) fd) selected_logs else
              selected_logs (* Should not happen *) in
            `SetLogsFds (selected_logs, run)
          ) fds selection ] ] ;
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
        if List.mem l.fd selected_logs then
          tr [
            td ~a:(class_ "click" ::
                   onclick_if_allowed waiting (`GetRun (l.run, [||])))
              (* Apply the "click" class on the text rather than the td to
               * decorate that text as a link: *)
              [ txt_span ~a:[ class_ "click" ] ("#"^ string_of_int l.run) ] ;
            td [ text (filename_of_fd l.fd) ] ;
            td ~a:[ class_ "time" ] [ text (date_of_ts l.time) ] ;
            td ~a:[ class_ "logline" ] (dom_of_ansi l.line) ] :: acc
        else acc in
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
