(* DB2 *)

(* Example database *)


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
  include Make(Rpc)
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
  |> Row.update fldname (Rpc.String "")
  |> Table.update objref Row.empty
  |> TableSet.update tblname Table.empty

let dump db =
  TableSet.iter (fun tblname table ->
    Printf.printf "# TABLE: %s\n\n" tblname;
    Table.iter (fun objref row ->
      Printf.printf "## Object: %s\n" objref;
      Row.iter (fun fldname v ->
        Printf.printf "  %s: %s\n" fldname v) row) table) db


(* Example usage *)

let db = ref Database.empty

type _ cls =
  | VM : [`vm] cls
  | VBD : [`vbd] cls

let table_of : type a. a cls -> string = function
  | VM -> "VM"
  | VBD -> "VBD"

module Ref : sig
  type 'a t = S of string
  val string_of : 'a t -> string
  val of_string : 'a cls -> string -> 'a t
  val table_of : 'a t -> string
  val pp : Format.formatter -> 'a t -> unit
end = struct
  type 'a t = S of string
  let string_of (S v) = v
  let of_string _ v = (S v)
  let table_of r = "VM"
  let pp fmt (S v) = Format.pp_print_string fmt v
end

let r = Ref.of_string VM "Opaquerefwhatever"
exception RTTI


type _ typ =
  | String : string typ
  | Int : int64 typ
  | Refv : 'a cls -> 'a Ref.t typ
  | List : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ

let rec to_rpc : type a. a typ -> a -> Rpc.t = fun typ v ->
  match typ with
  | String -> Rpc.String v
  | Int -> Rpc.Int v
  | Refv _ -> Rpc.String (Ref.string_of v)
  | List ty -> Rpc.Enum (List.map (to_rpc ty) v)
  | Map (ty1,ty2) ->
    let dict =
      List.map (fun (v1,v2) ->
        Jsonrpc.to_string (to_rpc ty1 v1),
        to_rpc ty2 v2) v
    in
    Rpc.Dict dict

let rec of_rpc : type a. a typ -> Rpc.t -> a = fun typ v ->
  match typ, v with
  | String, Rpc.String str -> str
  | Int, Rpc.Int i -> i
  | Refv _, Rpc.String s -> Ref.S s
  | List ty, Rpc.Enum vs -> List.map (of_rpc ty) vs
  | Map (ty1, ty2), Rpc.Dict kvs ->
    List.map (fun (k,v) ->
      Jsonrpc.of_string k |> of_rpc ty1,
      of_rpc ty2 v
      ) kvs
  | _, _ -> raise RTTI

(* Better: *)

module Field = struct
  type ('a,'b) t = {
    name : string;
    cls : 'a cls;
    ty : 'b typ;
  }
  let name_of x = x.name
end

let field ~name ~cls ~ty = Field.{name; cls; ty}

let name_label = field ~name:"name_label" ~cls:VM ~ty:String
let vBDs = field ~name:"VBDs" ~cls:VM ~ty:(List (Refv VBD))

let set : 'a Ref.t -> ('a,'b) Field.t -> 'b -> unit = fun ref field v ->
  let open Field in
  db := set_field (table_of field.cls) (Ref.string_of ref) field.name (to_rpc field.ty v) !db

let get : 'a Ref.t -> ('a,'b) Field.t -> 'b = fun ref field ->
  let open Field in
  get_field (table_of field.cls) (Ref.string_of ref) field.name !db |> (of_rpc field.ty)

