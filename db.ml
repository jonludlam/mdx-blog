(* db1.ml - first attempt at an untyped database *)

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

(* Usage *)

let vm = "OpaqueRef:abcde"
let vbd1 = "OpaqueRef:fghij"
let vbd2 = "OpaqueRef:12345"

let db = ref Database.empty

let _ =
  db := set_field "VM" vm "name_label" "my first vm" !db;
  db := set_field "VBD" vbd1 "VM" vm !db;
  db := set_field "VBD" vbd2 "VM" vm !db;
  db := set_field "VM" vm "VBDs" (String.concat "," [vbd1;vbd2]) !db;
  db := set_field "VM" vm "memory" "63356" !db;
  dump !db
