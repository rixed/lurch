open Batteries

open LurchServerLib

let is_directory f =
  try Sys.is_directory f with _ -> false

let mkdir_all ?(is_file=false) dir =
  let dir = if is_file then Filename.dirname dir else dir in
  let rec ensure_exist d =
    if d <> "" && not (is_directory d) then (
      ensure_exist (Filename.dirname d) ;
      log.debug "mkdir %S" d ;
      try Unix.mkdir d 0o755
      with Unix.(Unix_error (EEXIST, _, _)) -> ()
    ) in
  ensure_exist dir

let cp a b =
  mkdir_all ~is_file:true b ;
  system_or_fail ("cp "^ shell_quote a ^" "^ shell_quote b)
