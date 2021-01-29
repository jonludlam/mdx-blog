# GADTs for marshalling and unmarshalling

Our next task is to eliminate the need for the generated functions that turn our typed values into strings and back again. We're going to turn again to GADTs to help us do this. Let's consider how we declared the types of our API fields:

```ocaml
type ty =
  | String
  | Int
  | Float
  | Bool
  | DateTime
  | Set of ty
  | Map of ty * ty
  | Ref of string
  | Record of string
  | Enum of string * (string * string) list
```

We'll start by cutting this down to a couple of cases so it's easier to work with - we'll do Ints and Bools for now:

```ocaml
type ty =
  | Int
  | Bool
```

Now even with a value of type `ty` in our hands we still have no way to generically handle values of ocaml type `bool` or `int`. However, it we turn this into a GADT and carry the ocaml type alongside the value we might be able to do better. Let's declare ty as a GADT:

```ocaml
type _ ty =
  | Int : int ty
  | Bool : bool ty
```

This allows us now to write a function whose signature looks like this:

```ocaml
val shiny_marshal_function : 'a ty -> 'a -> string
```

so we can write `shiny_marshal_function Bool` and we're left with a `bool -> string` function, or `shiny_marshal_function Int` and we're left with an `int -> string` function. Let's write it!

```ocaml
let shiny_marshal_function : type a. a ty -> a -> string = function
  | Bool -> string_of_bool
  | Int -> string_of_int
```

Once again we have to declare `a` as locally abstract so that the type checker doesn't try to unify it across each of our `ty` constructors. And of course we can declare the inverse of this function to convert back from strings:

```ocaml
let shiny_unmarshal_function : type a. a ty -> string -> a = function
  | Bool -> bool_of_string
  | Int -> int_of_string
```

Let's extend our `ty` type a little to express more of the types in our datamodel:

```ocaml
type _ ty =
  | String : string ty
  | Int : int ty
  | Float : float ty
  | Bool : bool ty
  | DateTime : string ty
```

These are all fine. But what do we do about our `Set` ty? That's one where in our original definition it was `Set of ty` - how do we do that in our new syntax?

It turns out that the new thing of putting a type constraint on each constructor means it's really easy to do:

```ocaml
type _ ty =
...
    | Set : 'a ty -> 'b ty
```

Because, of course, `Set of ty` is used in a way that looks a lot like function application (`Set Int` for example), so it's  natural to use the function syntax to declare them.

Having said that, the type's not quite right yet. What we want on the right hand side is for the type parameter to have the type we want values to have. So if we've got `Set Int`, how do we want to represent this in a normal OCaml type? An int list might be a reasonable first go. The type of our constructor should look like this then:

```ocaml
type _ ty =
...
  | Set : 'a ty -> 'a list ty
```

And by a similar argument, our `Map` should be:

```ocaml
type _ ty =
...
  | Map : ('a ty * 'b ty) -> ('a * 'b) list ty
```

For example, a `Map (Int, String)` becomes a `(int * string) list ty`

Our marshalling and unmarshalling function now need to be recursive, but magically this all just works! I'm going to skip the types that all look similar for brevity, so we'll skip out Bool, Float and DateTime

```ocaml
type _ typ =
  | String : string typ
  | Int : int typ
  | List : 'a typ -> 'a list typ
  | Map : ('a typ * 'b typ) -> ('a * 'b) list typ

let rec to_string : type a. a typ -> a -> string = fun typ v ->
  match typ with
  | String -> v
  | Int -> string_of_int v
  | List ty -> String.concat "," (List.map (to_string ty) v)
  | Map (k_ty, v_ty) ->
    v
    |> List.map (fun (k,v) ->
         let k_str = to_string k_ty k in
         let v_str = to_string v_ty v in
         Printf.sprintf "%s|%s" k_str v_str)
    |> String.concat ";"

let rec of_string : type a. a typ -> string -> a = fun typ str ->
  match typ with
  | String -> str
  | Int -> int_of_string str
  | List ty -> Astring.String.cuts ~sep:"," str |> List.map (of_string ty)
  | Map (k_ty, v_ty) ->
    Astring.String.cuts ~sep:";" str |>
    List.map (fun s ->
      match Astring.String.cuts ~sep:"|" s with
      | [k;v] -> (of_string k_ty k, of_string v_ty v)
      | _ -> failwith (Printf.sprintf "Unmarshalling error: %s doesn't split correctly" s))
```

How about Refs? The original definition of `Ref` only had a string as an argument, and hence there's no useful type information there. That's an easy fix: we can use our `cls` type from the previous article, so, using `Refv` rather than `Ref` to avoid confusing the constructor with the module, we'll have `| Refv : 'a cls -> ..?`. As before, the right hand side needs to represent the OCaml type we'd like to use, which is an `'a Ref.t`. The type declaration therefore looks like this:

```ocaml
type _ ty =
...
  | Refv : 'a cls -> 'a Ref.t ty
```

But alas! with this addition to our ty definition we now get a compilation error!

```ocaml
utop # type _ typ =
  | String : string typ
  | Int : int typ
  | Refv : 'c cls -> 'c Ref.t typ
  | Map : ('a typ * 'b typ) -> ('a * 'b) list typ
  | List : 'a typ -> 'a list typ;;
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
```

utop helpfully underlines the entire type declaration so we know something in it is wrong. Thanks! Fortunately we know it worked before we added the `Refv` thing, so we know to look there. A bit of googling brings us to a [discussion of this issue on the caml list](https://sympa.inria.fr/sympa/arc/caml-list/2013-10/msg00189.html) and the problem turns out to be one of being able to decide whether types are the same or not. It's an interesting discussion and one that's worth reading. Happily the fix is a simple one: Wrap the Ref.t in a variant:

```ocaml
module Ref : sig
  type 'c t = S of string
  val string_of : 'c t -> string
  val of_string : 'c cls -> string -> 'c t
  val pp : Format.formatter -> 'c t -> unit
end = struct
  type 'c t = S of string
  let string_of (S v) = v
  let of_string _ v = (S v)
  let pp fmt (S v) = Format.pp_print_string fmt v
end
```

and now OCaml is happy with our `ty` definition. Next, we need to add to our `to_string` and `of_string` functions. The new bits are straightforward:

```ocaml
let rec to_string : type a. a typ -> a -> string = fun typ v ->
  match typ with
  ...
  | Ref _ -> Ref.string_of v

let rec of_string  : type a. a typ -> string -> a = fun typ v ->
  match typ with
  ...
  | Ref _ -> Ref.of_string v
```

With these all in place we can now remove the `to_string` and `of_string` from our field definition, and instead store the `ty` information and pass this into our generic marshal and unmarshal functions. We end up with a simpler definition of `Field`:

```ocaml
module Field = struct
  type ('a,'b) t = {
    name : string;
    cls : 'a cls;
    ty : 'b typ;
  }
  let name_of x = x.name
  let table_of : type a . (a, _) t -> string = fun x -> match x.cls with | VM -> "VM" | VBD -> "VBD"
end
```

and we call `to_string` and `of_string` in our database set and get functions:

```ocaml
let set : 'a Ref.t -> ('a,'b) Field.t -> 'b -> unit = fun ref field v ->
  db := set_field (Field.table_of field) (Ref.string_of ref) field.Field.name (to_string field.Field.ty v) !db

let get : 'a Ref.t -> ('a,'b) Field.t -> 'b = fun ref field ->
  get_field (Field.table_of field) (Ref.string_of ref) field.Field.name !db |> (of_string field.Field.ty)
```


Looking back at what we've just done, the Map and List to string convertions are looking pretty ugly. Additionally we're still suffering from the penalty of having to convert to and from strings to put things in the DB. Next time let's see whether we can improve the situation by storing typed data in our database rather than untyped strings.

Here's the final `db5.ml`:

```ocaml
(* DB4 - GADTs for marshalling *)

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
  type 'c t = S of string
  val string_of : 'c t -> string
  val of_string : 'c cls -> string -> 'c t
  val pp : Format.formatter -> 'c t -> unit
end = struct
  type 'c t = S of string
  let string_of (S v) = v
  let of_string _ v = (S v)
  let pp fmt (S v) = Format.pp_print_string fmt v
end

type _ typ =
  | String : string typ
  | Int : int typ
  | Refv : 'c cls -> 'c Ref.t typ
  | Set : 'a typ -> 'a list typ
  | Map : ('a typ * 'b typ) -> ('a * 'b) list typ

let rec to_string : type a. a typ -> a -> string = fun typ v ->
  match typ with
  | String -> v
  | Int -> string_of_int v
  | Refv _ -> Ref.string_of v
  | Set ty -> String.concat "," (List.map (to_string ty) v)
  | Map (k_ty, v_ty) ->
    v
    |> List.map (fun (k,v) ->
         let k_str = to_string k_ty k in
         let v_str = to_string v_ty v in
         Printf.sprintf "%s|%s" k_str v_str)
    |> String.concat ";"

let rec of_string : type a. a typ -> string -> a = fun typ str ->
  match typ with
  | String -> str
  | Int -> int_of_string str
  | Refv cls -> Ref.of_string cls str
  | Set ty -> Astring.String.cuts ~sep:"," str |> List.map (of_string ty)
  | Map (k_ty, v_ty) ->
    Astring.String.cuts ~sep:";" str |>
    List.map (fun s ->
      match Astring.String.cuts ~sep:"|" s with
      | [k;v] -> (of_string k_ty k, of_string v_ty v)
      | _ -> failwith (Printf.sprintf "Unmarshalling error: %s doesn't split correctly" s))

module Field : sig
  type ('c, 'f) t
  val construct : string -> 'c cls -> 'f typ -> ('c, 'f) t
  val name_of : ('c, 'f) t -> string
  val table_of : ('c, 'f) t -> string
end = struct
  type ('c,'f) t = {
    name : string;
    cls : 'c cls;
    ty : 'f typ;
  }
  let name_of x = x.name
  let table_of : type c. (c, _) t -> string = fun x -> match x.cls with | VM -> "VM" | VBD -> "VBD"
  let construct name cls ty = {name; cls; ty}
end

let set : 'a Ref.t -> ('a,'b) Field.t -> 'b -> unit = fun ref field v ->
  db := set_field (Field.table_of field) (Ref.string_of ref) field.Field.name (to_string field.Field.ty v) !db

let get : 'a Ref.t -> ('a,'b) Field.t -> 'b = fun ref field ->
  get_field (Field.table_of field) (Ref.string_of ref) field.Field.name !db |> (of_string field.Field.ty)

(* Example showing how we would use these in practice *)

let vm = Ref.of_string VM "OpaqueRef:abcde"
let vbd1 = Ref.of_string VBD "OpaqueRef:fghij"
let vbd2 = Ref.of_string VBD "OpaqueRef:12345"

let name_label = Field.construct "name_label" VM String
let vBDs = Field.construct "VBDs" VM (Set (Refv VBD))
let vbd_vm = Field.construct "VM" VBD (Refv VM)
let memory = Field.construct "memory" VM Int

let _ =
  set vm name_label "my first vm";
  set vbd1 vbd_vm vm;
  set vbd2 vbd_vm vm;
  set vm vBDs [vbd1; vbd2];
  set vm memory 63356;
  dump !db

```
