# Type Information Extraction for DWARF Code

To emit type information into DWARF, we need to propagate enough typing
information through the compiler such that we can create DWARF DIEs, which are
then used by LLDB to correctly display information about the current values of
variables.


## Overview

In DWARF, we can attach type information to variables. For this information, we
need more than just the name of the type. To correctly interpret the raw bytes
in memory in the debugger, we need to know additional information about the
layout of variables (e.g., bits64 vs. value), about the constructors of variants,
and so on. The more information we can extract, the better.

The general strategy for extracting this information will be to piggyback on
(and extend) the existing shape mechanism in the compiler (in `shape.ml`), which
Merlin already uses for lookups. The shape mechanism already provides "UIDs",
unique identifiers (across modules), at the typed tree level for bound variables
and type declarations. We will use these identifiers to track the debugging
information through the different intermediate representations of the compiler.

**Grabbing UIDs.** Based on the typed tree representation, we can conceptually
create two mappings (with caveats discussed below):

```OCaml
val type_expression_mapping : Uid.t -> Types.type_expr
  (** type expressions for bound variables,
      more efficiently represented as a table in the implementation. *)
val type_declaration_mapping : Uid.t -> Types.type_declaration
  (** type declarations for UIDs occurring in type expressions,
      more efficiently represented as a table in the implementation. *)
```

We can populate the first mapping when we traverse the typed tree and encounter
bound variables (after their types have already been determined). We can
populate the second mapping when we traverse module declarations and encounter
type declarations.

**Type shapes.** Instead of directly working with rich type expressions (of type
`Types.type_expr`) and type declarations (of type `Types.type_declaration`), we
use a leaner representation called *type shapes* and *type declaration shapes*.
(These will eventually be integrated with the existing shapes in the compiler.)
Compared to full type expressions and type declarations, these shapes carry less
information and are smaller. In essence, they only need to capture the
information that we eventually output in DWARF and interpret in LLDB on the
other side. (This also makes them easier to store in auxiliary compiler files
such as `.cms` files.)

The collection of UIDs (and a first version of the type shapes and type
declaration shapes) is currently implemented in the following PR:

1. [Type Shapes and Type Declaration Shapes for Debugger (#4253)](https://github.com/oxcaml/oxcaml/pull/4253)


**Propagating UIDs.** At the typed tree level, bound variables are already
associated with a UID. We populate the `type_expression_mapping` with them
and then propagate the UIDs through the different representations of the
compiler. This is currently implemented in the following PRs:

1. [Propagate Debugging Identifiers through Lambda (#3942)](https://github.com/oxcaml/oxcaml/pull/3942); already merged
2. [Propagate Debugging Identifiers through the Middle End (#3967)](https://github.com/oxcaml/oxcaml/pull/3967)
3. [Printing Support for Debugging UIDs (#4255)](https://github.com/oxcaml/oxcaml/pull/4255); adds only printing support


**Emitting DWARF Type DIEs.** Once we have propagated the UIDs through the
different representations of the compiler, we can eventually use the two
mappings to emit DWARF DIEs. We do this in the following PRs:

1. [Debugging DWARF Emission of Type Shapes (#4254)](https://github.com/oxcaml/oxcaml/pull/4254); simple types only
2. [Additional Types for DWARF Emission (#4259)](https://github.com/oxcaml/oxcaml/pull/4259); unboxed types, SIMD, ...


**Beyond.** Once the basic propagation and DWARF emission is complete, we will
add support for looking up declarations across files (see [.cms
files](#cms-files)) and integrate the type shapes with the existing shape
mechanism (see [Supporting Functors](#supporting-functors)).




## Type Shapes and Type Declaration Shapes


In the first instance, type shapes and type declaration shapes will be roughly
of the following form (feel free to skip this code block):

```OCaml
module Type_shape : sig
  type without_layout = Layout_to_be_determined

  type 'a t =
  | Ts_constr of (Uid.t * Path.t * 'a) * without_layout t list
  | Ts_tuple of 'a t list
  | Ts_unboxed_tuple of 'a t list
  | Ts_var of string option * 'a
  | Ts_predef of Predef.t * without_layout t list
  | Ts_arrow of without_layout t * without_layout t
  | Ts_variant of 'a t poly_variant_constructors
  | Ts_other of 'a

  (* ... *)
end


module Type_decl_shape : sig

  (** For type substitution to work as expected, we store the layouts in the
      declaration alongside the shapes instead of directly going for the
      substituted version. *)
  type tds_desc =
    | Tds_variant of
        { simple_constructors : string list;
                (** The string is the name of the constructor. The runtime
                    representation of the constructor at index [i] in this list
                    is [2 * i + 1]. See [dwarf_type.ml] for more details. *)
            complex_constructors :
            (Type_shape.without_layout Type_shape.t * Layout.t)
            complex_constructor
            list
                (** All constructors in this category are represented as blocks.
                    The index [i] in the list indicates the tag at runtime. The
                    length of the constructor argument list [args] determines
                    the size of the block. *)
        }
    | Tds_variant_unboxed of
        { name : string;
          arg_name : string option;
              (** if this is [None], we are looking at a singleton tuple;
              otherwise, it is a singleton record. *)
          arg_shape : Type_shape.without_layout Type_shape.t;
          arg_layout : Layout.t
        }
        (** An unboxed variant corresponds to the [@@unboxed] annotation.
            It must have a single, complex constructor. *)
    | Tds_record of
        { fields :
            (string * Type_shape.without_layout Type_shape.t * Layout.t) list;
          kind : record_kind
        }
    | Tds_alias of Type_shape.without_layout Type_shape.t
    | Tds_other

  and 'a complex_constructor =
    { name : string;
      kind : Types.constructor_representation;
      args : 'a complex_constructor_arguments list
    }

  and 'a complex_constructor_arguments =
    { field_name : string option; field_value : 'a }


  type t =
    { path : Path.t;
      definition : tds_desc;
      type_params : Type_shape.without_layout Type_shape.t list
    }

    (* ... *)

end
```

**Translating type expressions into type shapes.** For translating type
expressions/type declarations into type shapes, we provide two functions:

```OCaml
val of_type_expr :
  Types.type_expr ->
  (Path.t -> Uid.t option) ->
  without_layout Type_shape.t

val of_type_decl :
  Types.type_declaration ->
  (Path.t -> Uid.t option) ->
  Type_decl_shape.t
```

Initially, these two functions need a lookup for UIDs in the form
of an environment. This will later be replaced by lookups for shapes at the
[Supporting Functors](#supporting-functors) stage.


**Layouts/Sorts.** One aspect of type shapes that requires special
consideration is the treatment of layouts (i.e., `Jkind_types.Sort.Const.t`). To
emit DWARF information correctly, we must always know the layout of a
variable. Type expressions themselves, however, do not always have a layout
associated with them. Instead, the layout can come from (1) the variable whose
type we are considering or (2) information in the type declaration
(e.g., the fields of a mixed record have a layout associated with them). The way
we handle layouts in shapes is to provide one function to construct a shape
without a layout, `of_type_expr`, and another, `shape_with_layout`, to
substitute in a layout.

When substituting in the layout, there can be leaf cases such as `Ts_constr`,
where we store the layout in the data type until we actually look up the type
constructor during DWARF code emission (at which point we can propagate the
layout further).


**Incomplete Information.** It may be the case that we *cannot* find the
declaration of a type (e.g., it could be an abstract type with no OCaml-level
definition), the type could be a polymorphic argument of a function, or the type
could be declared in a file for which we do not have shape information. In these
cases, we will use the shapes `Ts_other`/`Tds_other`. For DWARF emission, we
will mark the value as an OCaml value of unknown type. The LLDB side will do its
best to interpret the runtime representation of the value.

Crucially, while type shapes can be missing or can be `Ts_other`, layouts must
always be known, since they fundamentally affect how data should be
interpreted. Layout information is known for all bound variables in the typed
tree.


## Supporting Functors

One tedious aspect of the shape definition given above is that
we have to manually implement looking up type constructors in the `Ts_constr`
case, which can include, for example, resolving a functor application and, in
the worst case, can require looking up definitions in another file. To avoid
duplicating work that is already done in the compiler's shape mechanism
(`shape.ml`), the next step (once basic support is implemented) is to integrate
the definition of type shapes with the compiler's shape mechanism.

The gist of this change is pretty straightforward:
  1. We extend the shapes in the compiler with a new case for type declarations.
  2. We modify the type shapes to store shapes from `shape.ml` in the
     constructor case.

By implementing these two changes, we can then piggyback on the compiler's shape
reduction mechanism (in `shape_reduce.ml`) to resolve module
expressions. In particular, we can use it for lookups in the case of type
constructors.

*Implementation:* A prototype of this is already implemented, but further
work is needed to handle type variables via the shape mechanism and to
extend the shape mechanism with recursive binders.


## .cms files

As part of integrating with the shape mechanism, we can use its support for
looking up shapes in `.cms` files to handle code distributed across multiple
files. More specifically, to support declarations in other `.ml` files (which
may not necessarily be visible in the `.mli` file, but are very useful for
providing helpful debugging information), we can augment the `.cms` format with
the file-local version of `type_declaration_mapping`. We can then instantiate
the shape reduction mechanism so that it will load the file-local mapping of
UIDs to type declaration shapes whenever a lookup of the corresponding library
becomes necessary.

*Implementation:* A prototype of this mechanism is already implemented. It can
handle the following lookup via `.cms` files:

```OCaml
(* library.ml *)
module Func(X: sig
    type arg_t
    val x : arg_t
end) : sig
    type func_t
    val x : func_t
end = struct
    type mylist' = Foo | Bar of X.arg_t * mylist
    and mylist = mylist'

    type func_t = mylist

    let x = Bar (X.x, (Bar (X.x, Foo)))
end

module ArgUpdDown : sig
    type arg_t
    val x : arg_t
end = struct
    type arg_t = Up | Down
    let x = Down
end


module ArgLeftRight : sig
    type arg_t
    val x : arg_t
end = struct
    type arg_t = Left | Right
    let x = Left
end


(* test.ml *)
module S = Library.Func(Library.ArgUpdDown)
module T = Library.Func(Library.ArgLeftRight)

let[@inline never][@locals never] f (z : T.func_t * S.func_t) = (z = z)

let _ = f (T.x, S.x)
```

resulting in

```
(lldb) b Test.f
Breakpoint 1: where = test`Test.f + 10 at test.ml:8:64, address = 0x000000000041f75a
(lldb) run
Process 3672107 stopped
* thread #1, name = 'test', stop reason = breakpoint 1.1
    frame #0: 0x000000000041f75a test`Test.f(z=((Bar (Left, (Bar (Left, Foo)))), (Bar (Down, (Bar (Down, Foo))))) : T.func_t * S.func_t @ value) at test.ml:8:64
   5
   6
   7
-> 8    let[@inline never][@locals never] f (z : T.func_t * S.func_t) = (z = z)
```


## Performance Considerations

**Compilation time.** Computing the shapes from type expressions should have
negligible overhead. Reducing the shape information per variable (see
[Supporting Functors](#supporting-functors)) could have nontrivial overhead
and should be measured.

**Execution time.** The DWARF information should have no notable effect on the
performance of the resulting code.

**Memory usage.** Keeping the shape information in memory will increase memory
usage during compilation of a file, but hopefully this should not significantly
increase the overall memory consumption.

**Binary size.** The DWARF information will considerably increase the size of
binaries. As such, it should be split off into a separate file to keep binary
sizes small.

**.cms files.** For each `.cms` file, we will additionally need to store the
type declaration shapes. Ideally, this should not be too large and can be
enabled by default. We should examine the size of declarations when stored in
`.cms` files and determine whether we can compress the representation if
necessary.


# Implementation Status

| Feature                                 | Status                             |
|-----------------------------------------|------------------------------------|
| Type Shapes and Type Declaration Shapes | Up for Review (PR #4253)           |
| Propagating UIDs through Lambda         | Merged (PR #3942)                  |
| Propagating UIDs through the Middle End | Up for Review (PR #3967)           |
| Printing Support for Debugging UIDs     | Up for Review (PR #4255)           |
| Debugging DWARF Emission of Type Shapes | Up for Review (PR #4254)           |
| Additional Types for DWARF Emission     | Up for Review (PR #4259)           |
| Functor Support                         | Work in Progress                   |
| Support for .cms files                  | Work in Progress                   |
