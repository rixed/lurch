open Js_of_ocaml
open Vdom

module Api = LurchApiTypes

type 'a edited =
  { edited : 'a ; saved : 'a option }

type t = Api.Program.t edited

let init =
  let command operation = Api.Command.{ id = 0 ; operation } in
  (* The default operation must be the top-level one: *)
  let operation =
    Api.Command.Isolate {
      builder = command (Api.Command.Chroot { template = "busybox" }) ;
      subcommand = command Api.Command.Nop } in
  { edited = Api.Program.{
      name = "" ; created = 0. ;
      command = command operation } ;
    saved = None }

let of_api p =
  { edited = p ; saved = Some p }
