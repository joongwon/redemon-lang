open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type const = String of string | Int of int [@@deriving eq, show, yojson]
type label = Label of int [@@unboxed] [@@deriving eq, show, yojson]

type attr_value = AttrConst of const | AttrFunc of label
[@@deriving eq, show, yojson]

type tree = Const of const | Elem of elem

and elem = {
  name : string;
  attrs : (string * attr_value) list;
  children : tree list;
}
[@@deriving eq, show, yojson]

let tree_const c = Const c
let tree_elem name attrs children = Elem { name; attrs; children }
let string_of_const = function String s -> s | Int i -> string_of_int i
