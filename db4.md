# Simple GADTs

We have a number of issue to address from the last DB implementation. Let's look first at the problem of having to pass the table name as a value into the field.

We know that our fields are tagged with a phantom type that represents the object class it references, but we can't use this information easily at runtime to figure out the table we care about. Enter GADTs, which give us a way to do precisely this.

A GADT is like a normal ADT, except that the type of it can vary from constructor to constructor. Imagine for example a normal ADT called `t`. Each of the tags is a constructor that gives us a value of type `t`, which can be explicitly declared in the type definition like this:

```ocaml
type t =
  | FirstConstructor : t
  | SecondConstructor : t
```

The type annotations on each constructor make it clear that each of them is of type `t`. Equally, we can parameterise over a type variable like this:

```ocaml
type 'a t =
  | FirstConstructor : 'a t
  | SecondConstructor : 'a t
```

Since this is a normal ADT, anywhere we use `FirstConstructor` in a context where `'a` has a concrete type, for example, an `int`, then `SecondConstructor` will also expect `'a` to be an int. The types are all consistent and the values can be used interchangably - they can be put in a list, for example.

A GADT by contrast takes this parameterised type and can associate a _different_ type for the parameter with each constructor:

```ocaml
type _ t =
  | FirstConstructor : int t
  | SecondConstructor : string t
```

Note that we can no longer write anything useful where we had `'a` in our original type declaration `type 'a t`, so we replace it with an underscore.

Let's see how we can use this to help us here. We can construct a GADT representing a specific object class:

```ocaml
type _ cls =
  | VM : [`vm] cls
  | VBD : [`vbd] cls
```

Now we can pass this to our field constructor and Ref.of_string function and we no longer have to worry about accidentally passing a string saying "VBD" and doing a type constraint of `vm`, because our one value of type `'a cls` gives us _both_ effects we wanted - it carries the type constraint _and_ we can pattern match on it to get our table name.

```ocaml
module Ref : sig
  type 'c t
  val string_of : 'c t -> string
  val of_string : 'c cls -> string -> 'c t (* Here we're using our new GADT *)
  val pp : Format.formatter -> 'c t -> unit
end = struct
  type 'c t = string
  let string_of v = v
  let of_string _ v = v (* We don't need the value of the GADT, just its type *)
  let pp fmt v = Format.pp_print_string fmt v
end
```

We have to update our String_to_DM functions that are creating Ref.t values:

```ocaml
module String_to_DM = struct
  let vbd_set str = Astring.String.cuts ~sep:"," str |> List.map (Ref.of_string VBD)
end
```

This is neat - we no longer need to constrain the type!

Let's pass the `cls` type to the field definition:

```ocaml
module Field : sig
  type ('c, 'f) t
  val construct : string -> 'c cls -> (string -> 'f) -> ('f -> string) -> ('c, 'f) t
  val name_of : ('c, 'f) t -> string
  val table_of : ('c, 'f) t -> string
  val string_of_value : ('c, 'f) t -> 'f -> string
  val value_of_string : ('c, 'f) t -> string -> 'f
end = struct
  type ('c,'f) t = {
    name : string;
    cls : 'c cls;
    value_of_string : string -> 'f;
    string_of_value : 'f -> string;
  }
  let name_of x = x.name
  let table_of x = match x.cls with | VM -> "VM" | VBD -> "VBD"
  let construct name cls value_of_string string_of_value = {name; cls; value_of_string; string_of_value;}
  let string_of_value x = x.string_of_value
  let value_of_string x = x.value_of_string
end
```

Unfortunately if we were just to type this in, we would run smack into the first and biggest issue with GADTs: They don't play nicely with type inference. It fails here:

```ocaml
utop # let table_of cls = match cls with | VM -> "VM" | VBD -> "VBD";;
Error: This pattern matches values of type [ `vbd ] cls
       but a pattern was expected which matches values of type [ `vm ] cls
```

where the problem is with the "| VBD" clause. What the type inference engine is trying to do is to unify the types of these constructors, but of course they are all of different type - which is the whole point! What we have to do is use a bit of syntax to keep the type of `c` in `c cls` abstract (although the type is known in each individual branch of the match). We need to rewrite it like this:

```ocaml
utop # let table_of : type c. c cls -> string = fun cls -> match cls with | VM -> "VM" | VBD -> "VBD";;
val table_of : 'c cls -> string = <fun>
```

On the plus side, though, we only need that one type constraint on that one function. All of the field definitions are now type-constraint free and, additionally, the type checker will tell us if we've made a mistake.

```ocaml
let name_label = Field.construct "name_label" VM (fun x -> x) (fun x -> x)
let vBDs = Field.construct "VBDs" VM String_to_DM.vbd_set DM_to_String.vbd_set
let vbd_vm = Field.construct "VM" VBD (Ref.of_string VM) (Ref.string_of)
let memory = Field.construct "memory" VM (int_of_string) (string_of_int)
```

Next on our hit list is to get rid of the String_to_DM and DM_to_String modules.

Full code of db4:

```ocaml
(* DB4 - Simple GADTs *)

module StringMap = Map.Make(String)

module type VAL = sig
  type t
end

module Make (V : VAL) = struct
  type t = V.t StringMap.t
  let empty = StringMap.empty
  let add = StringMap.add
  let find = StringMap.find
  let mem = StringMap.mem
  let remove = StringMap.remove
  let update key default f t =
    let cur = if mem key t then find key t else default in
    let newv = f cur in
    StringMap.add key newv t
  let iter = StringMap.iter
end

module Row = struct
  include Make(String)
end

module Table = struct
  include Make(Row)
end

module TableSet = struct
  include Make(Table)
end

module Database = struct
  type t = TableSet.t
  let empty = TableSet.empty
end

let get_field tblname objref fldname db =
  TableSet.find tblname db |>
  Table.find objref |>
  Row.find fldname

let set_field tblname objref fldname v : Database.t -> Database.t =
  (function _ -> v)
  |> Row.update fldname ""
  |> Table.update objref Row.empty
  |> TableSet.update tblname Table.empty

let dump db =
  TableSet.iter (fun tblname table ->
    Printf.printf "\n# TABLE: %s\n\n" tblname;
    Table.iter (fun objref row ->
      Printf.printf "## Object: %s\n" objref;
      Row.iter (fun fldname v ->
        Printf.printf "  %s: %s\n" fldname v) row) table) db


let db = ref Database.empty

type _ cls =
  | VM : [`vm] cls
  | VBD : [`vbd] cls

(* Phantom-type-using references *)
module Ref : sig
  type 'c t
  val string_of : 'c t -> string
  val of_string : 'c cls -> string -> 'c t
  val pp : Format.formatter -> 'c t -> unit
end = struct
  type 'c t = string
  let string_of v = v
  let of_string _ v = v
  let pp fmt v = Format.pp_print_string fmt v
end

(* The following 2 modules would normally be constructed from code generated by datamodel definitions *)

module DM_to_String = struct
  let vbd_set vbds = String.concat "," (List.map Ref.string_of vbds)
end

module String_to_DM = struct
  let vbd_set str = Astring.String.cuts ~sep:"," str |> List.map (Ref.of_string VBD)
end

module Field : sig
  type ('c, 'f) t
  val construct : string -> 'c cls -> (string -> 'f) -> ('f -> string) -> ('c, 'f) t
  val name_of : ('c, 'f) t -> string
  val table_of : ('c, 'f) t -> string
  val string_of_value : ('c, 'f) t -> 'f -> string
  val value_of_string : ('c, 'f) t -> string -> 'f
end = struct
  type ('c,'f) t = {
    name : string;
    cls : 'c cls;
    value_of_string : string -> 'f;
    string_of_value : 'f -> string;
  }
  let name_of x = x.name
  let table_of : type c. (c, _) t -> string = fun x -> match x.cls with | VM -> "VM" | VBD -> "VBD"
  let construct name cls value_of_string string_of_value = {name; cls; value_of_string; string_of_value;}
  let string_of_value x = x.string_of_value
  let value_of_string x = x.value_of_string
end

let set : 'c Ref.t -> ('c,'f) Field.t -> 'f -> unit = fun ref field v ->
  db := set_field (Field.table_of field) (Ref.string_of ref) (Field.name_of field) (Field.string_of_value field v) !db

let get : 'c Ref.t -> ('c,'f) Field.t -> 'f = fun ref field ->
  get_field (Field.table_of field) (Ref.string_of ref) (Field.name_of field) !db |> Field.value_of_string field

(* Example showing how we would use these in practice *)

let vm = Ref.of_string VM "OpaqueRef:abcde"
let vbd1 = Ref.of_string VBD "OpaqueRef:fghij"
let vbd2 = Ref.of_string VBD "OpaqueRef:12345"

let name_label = Field.construct "name_label" VM (fun x -> x) (fun x -> x)
let vBDs = Field.construct "VBDs" VM String_to_DM.vbd_set DM_to_String.vbd_set
let vbd_vm = Field.construct "VM" VBD (Ref.of_string VM) (Ref.string_of)
let memory = Field.construct "memory" VM (int_of_string) (string_of_int)

let _ =
  set vm name_label "my first vm";
  set vbd1 vbd_vm vm;
  set vbd2 vbd_vm vm;
  set vm vBDs [vbd1; vbd2];
  set vm memory 63356;
  dump !db
```
