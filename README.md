# redemon-lang

- Syntax definition for trees used for user demonstration of UI.
- Tree Expression AST that abstracts the given demonstration.

## tree

AST is defined as follows:

```
const       c ::=   s       : string
                |   i       : integer

attr_value  a ::=   c       : const
                |   $ℓ      : function with label

tree        T ::=   {c}                 : const
                |   <t (x=a)*>T*</t>  : element
```

## tree expression

The tree expression AST is defined as follows:

```
vexpr   e_v ::=     c
              |     x

aexpr   e_a ::=     e_v
              |     $ℓ

texpr   e_t ::=     {e_v}
              |     <t (x=e_a)*>e_l</t>
              |     x?.(e_t)            : Option.map

lexpr   e_l ::=     [ e_t* ]
              |     x[].(e_t)           : List.map
```

The semantics domain of input for tree expression is defined as follows:

```
value   v ::=   c
            |   [ r* ]
            |   null
            |   r
record  r ::=   { (x=v)* }
```

- For `x?.(e_t)`, the `x` must be evaluated to a record or `null`, and if it is `null`, the expression evaluates to `None`.
  Otherwise, it evaluates to the tree represented by `e_t` under the record `x` as the environment.
- For `x[].(e_t)`, the `x` must be evaluated to a list of records, and it evaluates to a list of trees represented by `e_t` under each record in the list as the environment.

The definitional interpreter is defined in `lib/texpr.ml` as following functions:
- `veval` : `vexpr -> record -> value`
- `aeval` : `aexpr -> record -> attr_value option`
- `teval` : `texpr -> record -> tree option`
- `leval` : `lexpr -> record -> tree list`

## demo

The user demonstration is defined as follows:

```
path    p ::=   i*              : indices for traversing the tree
edit    E ::=   Dup i           : duplicate i-th child
            |   Del i           : delete i-th child
            |   Insert i T      : insert T as i-th child
            |   SetAttr x c?    : set attribute x to c or remove it
            |   Replace c       : replace the current node with c

action  A ::=   Click ℓ
            |   Input ℓ s

demo    D ::=   T; (A, [(p, E)*])*
```

(**TODO**: The interpretation of path is different in concrete edit and abstraction. With `[ x; null; y ]`,
the path `1` refers to the second data `null` in abstraction, but in concrete it refers to the second node which is `y`.)

The semantics of edit operations is defined in `lib/demo.ml` as following functions:

- `apply_do` : `edit -> tree -> tree`.
  This function applies the edit operation to the current node.
- `apply_traverse` : `path -> edit -> tree -> tree`.
  This function traverses the tree according to the path and applies the edit operation using `apply_do` on the current node.
- `apply_do` raises `Type_error` if the edit operation is not applicable to the current node;
  `Dup`, `Del`, `Insert`, and `SetAttr` are applicable only to elements,
  and `Replace` is only applicable to constants.
- Both raises `Invalid_argument` if the path or the index of edit operation is invalid.

## abstraction

The abstraction of the demonstration is defined as follows:

```
abstraction     α ::= e_t; r; (A, r)*
```

The abstraction consists of a tree expression `e_t` and alternating sequences of actions and records.
Each record corresponds to the environment of the tree expression after the action is performed.

The details of the abstraction are defined in `lib/abstraction.ml` as following functions:
- `texpr_of_tree` : `tree -> texpr`.
  This function converts a concrete tree to a tree expression which expects an empty record as the environment.
- `abstract_step` : `edit -> texpr -> record -> texpr * (record -> record) * record`.
  This function abstracts a single step of the demonstration, returning the updated tree expression,
  a function to update the previous environments, and the new environment.
- `abstract_step_traverse` : `path -> edit -> texpr -> record -> texpr * (record -> record) * record`.
  This function traverses the tree expression and abstracts the edit operation using `abstract_step`.
- `init_abstraction` : `tree -> abstraction`.
  This function initializes the abstraction from a concrete initial tree using `texpr_of_tree`.
- `add_step` : `abstraction -> (action * (path * edit) list) -> abstraction`.
  This function adds a step to the abstraction, updating the tree expression and environments accordingly.
- `abstract_demo` : `demo -> abstraction`.
  This function abstracts the entire demonstration, returning the final abstraction.

One can use `init_abstraction` and `add_step` to build an abstraction step by step,
or use `abstract_demo` to abstract the entire demonstration at once.
