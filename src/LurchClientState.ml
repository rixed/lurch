(*
 * Application State
 *)
module Api = LurchApiTypes
module Program = LurchClientProgram

type location =
  | ShowError of string
  | ListPastRuns of Api.ListPastRuns.ts
  | ListProgramsAndRun of Api.ListPrograms.ts
  | ShowProgram of
      { program : Program.t ;
        (* If true, display the program in an editor: *)
        editable : bool ;
        last_runs : Api.ListPastRuns.ts }
  | ConfirmDeleteProgram of
      { program : Program.t }
  | ShowRun of { run : Api.Run.t ; more_logs_expected : bool }
  | Test

let string_of_location = function
  | ShowError _ -> "ShowError"
  | ListPastRuns _ -> "ListPastRuns"
  | ListProgramsAndRun _ -> "ListProgramsAndRun"
  | ShowProgram _ -> "ShowProgram"
  | ConfirmDeleteProgram _ -> "ConfirmDeleteProgram"
  | ShowRun _ -> "ShowRun"
  | Test -> "Test"

type 'msg state =
  { (* Define the main location. *)
    location : location ;
    (* Indicates whether we are waiting for an ajax query: *)
    waiting : bool ;
    (* Recent message that should be resent to refresh the page. That's usually
     * the last `GetFoo. *)
    refresh_msg : 'msg option }
