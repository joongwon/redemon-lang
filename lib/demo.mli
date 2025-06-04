open Tree.Syntax

type path = int list

type edit =
  | Dup of int
  | Del of int
  | Replace of const
  | Insert of int * tree
  | SetAttr of string * const option

type event = Click | Input
type action = label * event * string option
type demo_step = action * (path * edit) list
type demo = { init : tree; steps : demo_step list }

exception Type_error of string

val apply_do : edit -> tree -> tree
val apply_traverse : path -> edit -> tree -> tree

open Ppx_deriving_runtime

val equal_action : action -> action -> bool
val equal_event : event -> event -> bool
val pp_action : Format.formatter -> action -> unit
val pp_event : Format.formatter -> event -> unit
val show_action : action -> string
val show_event : event -> string
