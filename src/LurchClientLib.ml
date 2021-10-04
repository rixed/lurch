module Api = LurchApiTypes
open Js_of_ocaml
open Vdom

(*
 * Misc
 *)

let log str =
  Js_browser.(Console.log console (Ojs.string_to_js str))

let log_js obj =
  Js_browser.(Console.log console obj)

let name_ x = Property ("name", String x)

let checked_ = attr "checked" ""

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

let option_map_default def f opt =
  option_map f opt |? def

let array_find f a =
  let rec loop i =
    if i >= Array.length a then raise Not_found
    else if f a.(i) then a.(i) else
    loop (i + 1) in
  loop 0

let array_filteri f a =
  let _, items =
    Array.fold_left (fun (i, items) v ->
      i + 1,
      if f i v then v :: items else items
    ) (0, []) a in
  List.rev items |> Array.of_list

let array_filter f a =
  array_filteri (fun _ x -> f x) a

let date_of_ts ts =
  let date = Js.(new%js date_fromTimeValue (1000. *. ts)) in
  Js.to_string date##toISOString

let date_of_tsn = option_map date_of_ts

let filename_of_fd = function
  | 0 -> "in"
  | 1 -> "out"
  | 2 -> "err"
  | n -> string_of_int n

let class_of_status = function
  | Api.ExitStatus.Completed -> "success"
  | Failed _ | CouldNotStart | TimedOut -> "failure"
  | Interrupted _ | Expired | Cancelled -> "killed"

(*
 * View
 *)

let span ?key ?a l = elt "span" ?key ?a l

let no_elt = span []

let label ?key ?a l = elt "label" ?key ?a l

let br ?key ?a () = elt "br" ?key ?a []

let h1 ?key ?a l = elt "h1" ?key ?a l

let h2 ?key ?a l = elt "h2" ?key ?a l

let h3 ?key ?a l = elt "h3" ?key ?a l

let ul ?key ?a l = elt "ul" ?key ?a l

let ol ?key ?a l = elt "ol" ?key ?a l

let li ?key ?a l = elt "li" ?key ?a l

let tr ?key ?a l = elt "tr" ?key ?a l

let td ?key ?a l = elt "td" ?key ?a l

let a_ ?key ?(a=[]) href label =
  let a = attr "href" href :: a in
  elt "a" ?key ~a [ text label ]

let colspan = int_attr "colspan"

let rowspan = int_attr "rowspan"

let no_attr = attr "x" ""

let id_ = attr "id"

let disabled b = attr "disabled" (if b then "true" else "false")

let tech_text str = txt_span ~a:[class_ "tech"] str

let textarea ?id ?key ?(a=[]) ?(editable=true) l =
  let a = option_map_default a (fun id -> attr "id" id :: a) id in
  let a = if editable then a else (disabled true :: a) in
  elt "textarea" ?key ~a l

let select ?id ?key ?(a=[]) ?(editable=true) ?selected options =
  let a = option_map_default a (fun id -> attr "id" id :: a) id in
  let a = if editable then a else (disabled true :: a) in
  elt "select" ?key ~a (
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

let input_text ?id ?key ?(a=[]) ?(editable=true) ?placeholder ?label ?units value =
  let a = option_map_default a (fun id -> attr "id" id :: a) id in
  let a = if editable then a else (disabled true :: a) in
  let a = type_ "text" ::
          (* attribute not property for value or the input won't be editable! *)
          attr "value" value ::
          a in
  let a =
    match placeholder with None -> a
                         | Some p -> attr "placeholder" p :: a in
  match label, units with
  | None, None ->
      input ?key ~a []
  | Some label, None ->
      elt "label" [
        text label ;
        input ?key ~a [] ]
  | None, Some units ->
      elt "label" [
        input ?key ~a [] ;
        text units ]
  | Some label, Some units ->
      elt "label" [
        text label ;
        input ?key ~a [] ;
        text units ]

(* Offer to edit a list of strings.
 * [id], [key] and [a] are that of a grouping div, whereas input ids will have an
 * index appended. [placeholder] apply to individual inputs. *)
let input_text_multi ?id ?key ?(a=[]) ?(editable=true) ?label ?on_add ?on_rem
                     ?placeholder values =
  let a = option_map_default a (fun id -> attr "id" id :: a) id in
  let lines =
    List.mapi (fun i value ->
      let sub_id = option_map (fun pref -> pref ^"/"^ string_of_int i) id in
      let inp = input_text ?id:sub_id ~editable ?placeholder value in
      match editable, on_rem with
      | true, Some msg -> p [ inp ; button "rem" (msg i) ]
      | _ -> inp
    ) values in
  let lines =
    lines @
      match editable, on_add with
      | true, Some msg -> [ p [ button "add" msg ] ]
      | _ -> [] in
  match label with
  | None -> div ?key ~a lines
  | Some label -> div ?key ~a (p [ text label ] :: lines)

let input_hidden ?id ?key ?(a=[]) value =
  let a = option_map_default a (fun id -> attr "id" id :: a) id in
  let a = type_ "hidden" ::
          (* attribute not property for value or the input won't be editable! *)
          attr "value" value ::
          a in
  input ?key ~a []

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
           let a = if selected then checked_ :: a else a in
           input ~a []) ;
          text opt ]
      ) options)

let group_name_seq = ref 0
let get_group_name () =
  let n = "group_name_"^ string_of_int !group_name_seq in
  incr group_name_seq ;
  n

let rec radios ?id ?key ?(a=[]) ?label ?(editable=true) options def =
  match label with
  | Some label ->
      p ?key [
        text label ; (* Not to be confused with the option labels *)
        radios ?id ~a ~editable options def ]
  | None ->
      let a = option_map_default a (fun id -> attr "id" id :: a) id in
      let group_name = get_group_name () in
      div ~a ?key
        (List.map (fun (n, v) ->
          let a = [type_ "radio"; name_ group_name; value v] in
          let a = if v = def then checked_ :: a else a in
          let a = if editable then a else (disabled true :: a) in
          elt "label" [ input ~a [] ; text n ]
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

let is_digit c =
  c >= '0' && c <= '9'

let digit_of_char c =
  Char.code c - Char.code '0'

let identity x = x

let dom_of_ansi s =
  let escape = '\027' in
  let len = String.length s in
  let dom_of_sgr =
    let s w v = span ~a:[ class_ ("ansi-" ^ w) ] [ v ] in
    function
    | 1 -> s "bold"
    | 2 -> s "faint"
    | 3 -> s "italic"
    | 4 -> s "underline"
    | 5 | 6 -> s "blink"
    | 7 -> s "reverse"
    | 9 -> s "strike"
    | 30 -> s "fg-color-0" | 31 -> s "fg-color-1" | 32 -> s "fg-color-2"
    | 33 -> s "fg-color-3" | 34 -> s "fg-color-4" | 35 -> s "fg-color-5"
    | 36 -> s "fg-color-6" | 37 -> s "fg-color-7"
    | 40 -> s "bg-color-0" | 41 -> s "bg-color-1" | 42 -> s "bg-color-2"
    | 43 -> s "bg-color-3" | 44 -> s "bg-color-4" | 45 -> s "bg-color-5"
    | 46 -> s "bg-color-6" | 47 -> s "bg-color-7"
    | _ -> identity in
  let rec loop html to_html start i =
    let append stop =
      to_html (String.sub s start (stop - start)) :: html in
    if i >= len then append i |> List.rev else
    if i < len - 2 && s.[i] = escape && s.[i+1] = '[' then
      let stop = i in
      let i = i + 2 in
      let cmd, params, i =
        let rec read_sgr params i =
          if i < len - 1 && is_digit s.[i] then
            let p = digit_of_char s.[i] in
            let i = i + 1 in
            let p, i =
              if i < len - 1 && is_digit s.[i] then
                p * 10 + digit_of_char s.[i],
                i + 1
              else
                p, i in
            let params = p :: params in
            if i < len - 1 && s.[i] = ';' then read_sgr params (i + 1)
            else s.[i], params, i + 1
          else if i < len - 1 && s.[i] = ';' then
            read_sgr (0 :: params) (i + 1)
          else
            s.[i], params, i + 1 in
        read_sgr [] i in
      let html, to_html, start =
        if cmd = 'm' then
          append stop,
          List.fold_left (fun dom_of_str p ->
            fun str -> (dom_of_sgr p) (dom_of_str str)
          ) text params,
          i
        else
          (* ignore that sequence and display it as if it were normal text *)
          html, to_html, start in
      loop html to_html start i
    else
      loop html to_html start (i + 1) in
  loop [] text 0 0

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
  send r (Ojs.string_to_js payload)

let run_http_get ~url ~callback () =
  run_http ~url ~callback ()

let run_http_put ~url ~payload ~callback () =
  run_http ~command:"PUT" ~content_type:"application/json"
           ~url ~callback ~payload ()

let run_http_del ~url ~callback () =
  run_http ~command:"DELETE" ~url ~callback ()

(* Display the stats of a run: *)
let stats self desc =
  let of_opt conv = function
    | None -> "n.a"
    | Some v -> conv v in
  let of_float = of_opt string_of_float
  and of_int = of_opt string_of_int in
  div [
    p [ text ("CPU: "^
              of_float self.Api.RunStats.cpu_usr ^"+"^
              of_float desc.Api.RunStats.cpu_usr ^" usr + "^
              of_float self.cpu_sys ^"+"^
              of_float desc.cpu_sys ^" sys") ] ;
    p [ text ("RAM: "^
              of_int self.mem_usr ^"+"^
              of_int desc.mem_usr ^" usr + "^
              of_int self.mem_sys ^"+"^
              of_int desc.mem_sys ^" sys") ] ]
