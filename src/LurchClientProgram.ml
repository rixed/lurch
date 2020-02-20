open Js_of_ocaml
open Vdom

module Api = LurchApiTypes

type 'a edited =
  { edited : 'a ; saved : 'a option }

type t = Api.Program.t edited

let init =
  { edited = Api.Program.{
      name = "" ; created = 0. ;
      command = { id = 0 ; operation = Api.Command.Nop } } ;
    saved = None }

let of_api p =
  { edited = p ; saved = Some p }
