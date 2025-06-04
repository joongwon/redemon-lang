type const = String of string | Int of int
type label = int
type attr_value = AttrConst of const | AttrFunc of label

type tree = Const of const | Elem of elem

and elem = {
  name : string;
  attrs : (string * attr_value) list;
  children : tree list;
}

val string_of_const : const -> string
val tree_const : const -> tree
val tree_elem : string -> (string * attr_value) list -> tree list -> tree

open Ppx_deriving_runtime
open Ppx_yojson_conv_lib

val equal_attr_value : attr_value -> attr_value -> bool
val equal_const : const -> const -> bool
val equal_elem : elem -> elem -> bool
val equal_label : label -> label -> bool
val equal_tree : tree -> tree -> bool
val pp_attr_value : Format.formatter -> attr_value -> unit
val pp_const : Format.formatter -> const -> unit
val pp_elem : Format.formatter -> elem -> unit
val pp_label : Format.formatter -> label -> unit
val pp_tree : Format.formatter -> tree -> unit
val show_attr_value : attr_value -> string
val show_const : const -> string
val show_elem : elem -> string
val show_label : label -> string
val show_tree : tree -> string
val yojson_of_attr_value : attr_value -> Yojson.Safe.t
val yojson_of_const : const -> Yojson.Safe.t
val yojson_of_elem : elem -> Yojson.Safe.t
val yojson_of_label : label -> Yojson.Safe.t
val yojson_of_tree : tree -> Yojson.Safe.t
