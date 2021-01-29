# XAPI's database: a typed journey

In this series of articles I'm going to write about the [database implemented in xapi](https://xapi-project.github.io/), and I'm going to use it as a pratical example to discuss and give examples of a few of the interesting developments in OCaml's type system over the years since we first wrote the DB. I've often personally found examples of these new features somewhat abstract and they can often raise baffling errors when you're actually trying to use them, so I'm structuring this as a journey rather than simply presenting the end product. Although I'm using xapi as a motivating example, nothing I have to talk about will have anything at all to do with virtualisation!

The database inside xapi is a key component in its architecture and is core to almost all of the operations that xapi performs. It stores almost all of the important information that xapi keeps track of, including all the VM metadata, storage setup, networking setup and other assorted bits of data. However, fundamentally it is a rather simple data structure consisting of 3 nested immutable maps with a few bells and whistles tacked on.

Roughly speaking the sorts of data we store in the database are individual fields of differently typed objects. Objects are indexed via a reference, usually of the form "OpaqueRef:xxxx..". For example, we might store the value `My first VM` in the `name_label` field of the `VM` object referenced by the value `OpaqueRef:1`.

Let's write it. We'll start by making a generic map module that can map from strings to arbitrary types:

```ocaml env=e1
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
```

In this code we create a functor called `Make` that takes a module whose purpose is simply to wrap a type t. The result of applying the functor to a module `V` is a new module that implements a map from a string to value of type V.t. We're restricting ourselves to have only a few functions from the standard StringMap, and we're implementing a conveniently typed `update` function that updates a single entry in the map. The type is chosen so that it will make our lives a little easier later.

As an example of this functor, we can implement a map from strings to integers in the following way:

```ocaml env=e1
module MyInt = struct
  type t = int
end

module StringToIntMap = Make(MyInt)
```

An important feature of this is that the generated module has in it a type `t` - that is, it satisfies the signature `VAL`, so we can pass that generated module back to the functor again to create a nested map. This is how we create the database:

```ocaml env=e1
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
```

Here we are creating a `Row` module that maps from strings (field names) to string values, a `Table` module that maps from strings (object references) to Rows and a `TableSet` that maps from strings (object types) to Tables.

Using this we can write a function to get something from the database:

```ocaml env=e1
let get_field tblname objref fldname db =
  TableSet.find tblname db
  |> Table.find objref
  |> Row.find fldname
```

and one to set values in the database:

```ocaml env=e1
let set_field tblname objref fldname v =
  (function _ -> v)
  |> Row.update fldname ""
  |> Table.update objref Row.empty
  |> TableSet.update tblname Table.empty
```

Let's just think about this one a sec. The `update` function in our modules take four parameters so every single one in the above function is partially applied, even when all the named arguments to the `set_field` call have been supplied. Let's rewrite it a bit to see more clearly what's going on:

```ocaml env=e1
let set_field tblname objref fldname v db =
  let field_update_fn _ = v in
  (* We update an field in the row by replacing it with our new value. We don't care what the previous value of the field was *)

  let row_update_fn old_row = Row.update fldname "" field_update_fn old_row in
  (* We update a row by taking the old row and applying the field_update function to the entry corresponding to `fldname` *)

  let table_update_fn old_table = Table.update objref Row.empty row_update_fn old_table in
  (* We update a table by taking the old table and applying the row update function to the entry corresponding to `objref` *)

  TableSet.update tblname Table.empty table_update_fn db
  (* We update a tableset by applying the table update function to the table corresponding to `tblname` *)
```

I have written in the explicit db parameter to make it even clearer.

We can write a database dump function to see the state of the database:

```ocaml env=e1
let dump db =
  Printf.printf "(*";
  TableSet.iter (fun tblname table ->
    Printf.printf "\n+ TABLE: %s\n\n" tblname;
    Table.iter (fun objref row ->
      Printf.printf "++ Object: %s\n" objref;
      Row.iter (fun fldname v ->
        Printf.printf "  %s: %s\n" fldname v) row) table) db;
  Printf.printf "*)\n"
```

Let's look at an example of how this might actually used:

```ocaml env=e1
# let vm = "OpaqueRef:abcde"
val vm : string = "OpaqueRef:abcde"
# let vbd1 = "OpaqueRef:fghij"
val vbd1 : string = "OpaqueRef:fghij"
# let vbd2 = "OpaqueRef:12345"
val vbd2 : string = "OpaqueRef:12345"
# let db = ref Database.empty
val db : '_weak1 StringMap.t ref = {contents = <abstr>}
# let _ =
  db := set_field "VM" vm "name_label" "my first vm" !db;
  db := set_field "VBD" vbd1 "VM" vm !db;
  db := set_field "VBD" vbd2 "VM" vm !db;
  db := set_field "VM" vm "VBDs" (String.concat "," [vbd1;vbd2]) !db;
  db := set_field "VM" vm "memory" "63356" !db;
  dump !db
(*
+ TABLE: VBD

++ Object: OpaqueRef:12345
  VM: OpaqueRef:abcde
++ Object: OpaqueRef:fghij
  VM: OpaqueRef:abcde

+ TABLE: VM

++ Object: OpaqueRef:abcde
  VBDs: OpaqueRef:fghij,OpaqueRef:12345
  memory: 63356
  name_label: my first vm
*)
- : unit = ()
```

Great! We've got a working database! Next article we'll see how we can hide this mad free-for-all untyped world by layering a nicely typed interface over the top of this database.
