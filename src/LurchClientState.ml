(*
 * Application State
 *)
module Api = LurchApiTypes
module Program = LurchClientProgram

type location =
  | Absent
  | ShowError of string
  | ListPastRuns of Api.ListPastRuns.ts
  | ListProgramsAndRun of Api.ListPrograms.ts
  | EditProgram of
      { program : Program.t ; editor : bool ;
        last_runs : Api.ListPastRuns.ts }
  | ConfirmDeleteProgram of
      { program : Program.t }
  | ShowRun of { run : Api.Run.t ; more_logs_expected : bool }
  | Test

type 'msg state =
  { (* Define the main location. This is basically a list of things: *)
    location : location ;
    (* Optionally, we can have a modal dialog on top of it for viewing/
     * editing one of the things. Also useful to display errors.
     * With that design we can have short lived interactions and easily go
     * back to where we came from, with no need to remember the full history
     * of locations: *)
    dialog : location (* Absent if absent *) ;
    (* Indicates whether we are waiting for an ajax query: *)
    waiting : bool ;
    (* Recent message that should be resent to refresh the page. That's usually
     * the last `GetFoo. *)
    refresh_msg : 'msg option }
