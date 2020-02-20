open Js_of_ocaml
open Vdom

(*
 * Misc
 *)

let log str =
  Js_browser.(Console.log console (Ojs.string_to_js str))

let log_js obj =
  Js_browser.(Console.log console obj)

let assert_fail str =
  log str ; assert false

let option_get = function
  | Some x -> x
  | None -> assert_fail "invalid_arg: option_get with None"

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let (|?) a b =
  match a with Some a -> a | None -> b

let array_find f a =
  let rec loop i =
    if i >= Array.length a then raise Not_found
    else if f a.(i) then a.(i) else
    loop (i + 1) in
  loop 0

let date_of_ts ts =
  let date = Js.(new%js date_fromTimeValue (1000. *. ts)) in
  Js.to_string date##toISOString

let date_of_tsn = option_map date_of_ts

let filename_of_fd = function
  | 0 -> "in"
  | 1 -> "out"
  | 2 -> "err"
  | n -> string_of_int n

(*
 * View
 *)

let no_elt = elt "span" []

let label ?key ?a l = elt "label" ?key ?a l

let br ?key ?a () = elt "br" ?key ?a []

let h1  ?key ?a l = elt "h1" ?key ?a l

let h2  ?key ?a l = elt "h2" ?key ?a l

let h3  ?key ?a l = elt "h3" ?key ?a l

let ul ?key ?a l = elt "ul" ?key ?a l

let ol ?key ?a l = elt "ol" ?key ?a l

let li ?key ?a l = elt "li" ?key ?a l

let tr ?key ?a l = elt "tr" ?key ?a l

let td ?key ?a l = elt "td" ?key ?a l

let colspan = int_attr "colspan"

let rowspan = int_attr "rowspan"

let no_attr = attr "x" ""

let id_ = attr "id"

let tech_text str = txt_span ~a:[class_ "tech"] str

let textarea ?key ?a l = elt "textarea" ?key ?a l

let select ?key ?a ?selected options =
  elt "select" ?key ?a (
    List.map (fun opt ->
      let a = if selected = Some opt then [ attr "selected" "yes" ] else [] in
      elt ~a "option" [ text opt ]
    ) options)

let p ?key ?a l = elt "p" ?key ?a l

let pre ?key ?a l = elt "pre" ?key ?a l

let table ?key ?a ?(header=[]) ?(footer=[]) l =
  elt "table" ?key ?a [
    elt "thead" header ;
    elt "tbody" l ;
    elt "tfoot" footer ]

let simple_table_header l =
  [ tr (List.map (fun s -> td [ text s ]) l) ]

let button ?key ?(a=[]) label msg =
  input ?key ~a:(type_button :: value label :: onclick (fun _ -> msg) :: a) []

let input_text ?key ?(a=[]) ?placeholder ?label value =
  let a = type_ "text" ::
          (* attribute not property for value or the input won't be editable! *)
          attr "value" value ::
          a in
  let a =
    match placeholder with None -> a
                         | Some p -> attr "placeholder" p :: a in
  match label with
  | None ->
      input ?key ~a []
  | Some label ->
      p [ elt "label" [ text label ] ;
          input ?key ~a [] ]

let horiz_spacer =
  p ~a:[class_ "horiz-spacer"] [ text " " ]

(*  vdom does not propagate multi-values properly
let select ?key ?(a=[]) name options selection =
  let a = (str_prop "name" name) :: a in
  elt "select" ?key ~a (
    Array.map (fun opt ->
        let selected = Array.mem opt selection in
        if name = "parents" && selected then log ("Option "^ opt ^" selected") ;
        elt "option" ~a:[value opt; str_prop "label" opt; bool_prop "selected" selected] []
      ) options |>
    Array.to_list)

let multiselect ?key ?(a=[]) name options selection =
  let a = bool_prop "multiple" true :: a in
  select ?key ~a name options selection
*)

let checkboxes ~action ?key ?a options selection =
  div ?key ?a
    (List.map (fun opt ->
        let selected = List.mem opt selection in
        label [
          (let a = [type_ "checkbox"; value opt; onchange action] in
           let a = if selected then attr "checked" ""::a else a in
           input ~a []) ;
          text opt ]
      ) options)

let unorderd_list ?key ?a lst =
  if lst = [] then text "None" else
    elt "ul" ?key ?a (List.map (fun n -> elt "li" [ text n ]) lst)

let togle_list s lst =
  let rec loop prev = function
  | [] -> s :: prev
  | x :: rest ->
    if x = s then List.rev_append prev rest
    else loop (x::prev) rest
  in
  loop [] lst

(* We need some widgets that are either editors or read-only version, toggling
 * with a [editor] boolean. *)

let edit_string ?id ?key ?(a=[]) editor str =
  let a =
    match id with None -> a
    | Some id -> attr "id" id :: a in
  if editor then
    input_text ?key ~a:(autofocus::a) ~placeholder:"enter a unique name..." str
  else
    p ?key ~a:(class_ "editable" :: a) [ text str ]

let edit_text ?id ?key ?(a=[]) editor str =
  let a =
    match id with None -> a
    | Some id -> attr "id" id :: a in
  if editor then
    textarea ?key ~a [ text str ]
  else
    pre ?key ~a:(class_ "editable" :: a) [ text str ]

(*
 * AJAX
 *)

type api_response = Ok of string | Error of string

let run_http ?(command="GET") ?(content_type="text/plain")
             ~url ?(payload="") ~callback () =
  let open Js_browser in
  let open XHR in
  let r = create () in
  open_ r command url;
  set_onreadystatechange r
    (fun () ->
       match ready_state r with
       | Done ->
          let res = response_text r and code = status r in
          let resp =
            if code = 200 then Ok res (*(Js_browser.JSON.parse res)*)
            else Error res in
          callback resp
       | _ -> ()) ;
  set_request_header r "Content-Type" content_type ;
  send r payload

let run_http_get ~url ~callback () =
  run_http ~url ~callback ()

let run_http_put ~url ~payload ~callback () =
  run_http ~command:"PUT" ~content_type:"application/json"
           ~url ~callback ~payload ()

let run_http_del ~url ~callback () =
  run_http ~command:"DELETE" ~url ~callback ()
