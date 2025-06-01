open Tree.Syntax
open Texpr

type dexpr =
  | Const of const
  | Var of var
  | Null
  | List of dexpr list
  | Record of (var * dexpr) list
  | OMap of { var : var; body : dexpr }
  | LMap of { var : var; body : dexpr }

type prog = { view : texpr; data : dexpr; states : (var * value) list }
