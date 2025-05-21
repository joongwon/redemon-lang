open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type const = String of string | Int of int | Bool of bool
[@@deriving eq, show, yojson_of]

type attr_value = AttrConst of const | AttrFunc
[@@deriving eq, show, yojson_of]

type tree =
  | Const of const
  | Elem of {
      name : string;
      attrs : (string * attr_value) list;
      children : tree list;
    }
[@@deriving eq, show, yojson_of]

let tree_const c = Const c
let tree_elem name attrs children = Elem { name; attrs; children }

let string_of_const = function
  | String s -> s
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
