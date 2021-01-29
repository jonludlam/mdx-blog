(* DB7 *)

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


exception RTTI

type _ cls = ..

module Ref : sig
  type 'a t = S of string
  val string_of : 'a t -> string
  val of_string : 'a cls -> string -> 'a t
  val pp : Format.formatter -> 'a t -> unit
end = struct
  type 'a t = S of string
  let string_of (S v) = v
  let of_string _ v = (S v)
  let pp fmt (S v) = Format.pp_print_string fmt v
end

type _ typ =
  | String : string typ
  | Int : int64 typ
  | Refv : 'a cls -> 'a Ref.t typ
  | List : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | Record : 'a record -> 'a rval typ

and 's record = {
  r_fields : 's boxed_field list;
  r_empty : 's;
}

and 's boxed_field =
  B : ('s, 'f) field -> 's boxed_field

and ('s, 'f) field = {
  f_name : string;
  f_cls : 's cls;
  f_ty : 'f typ;
  f_get : 's -> 'f;
  f_set : 'f -> 's -> 's;
}

and 's rval = {
  v : 's;
}

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
  | Record r ->
    let d = List.fold_left (fun a f' -> match f' with B f -> (f.f_name, StringMap.find f.f_name v.f_vals)::a) [] r.r_fields in
    Rpc.Dict d

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
  | Record r, Rpc.Dict fs ->
    let v =
      List.fold_left
        (fun a f' ->
          match f' with B f -> f.f_set (of_rpc f.f_ty (List.assoc f.f_name fs)) a
          )
        r.r_empty r.r_fields
    in { v }
  | _, _ -> raise RTTI

type vm = {
  name_label : string;
  vBDs : vbd Ref.t list;
}
and vbd = {
  vM : vm Ref.t list;
}
let empty_vm = {name_label=""; vBDs=[]}

type _ cls += VM : vm cls
type _ cls += VBD : vbd cls

let table_of : type a. a cls -> string = function
  | VM -> "VM"
  | VBD -> "VBD"
  | _ -> failwith "Unknown class!"

(* Better: *)

let field ~name ~cls ~ty ~fget ~fset = {f_name=name; f_cls=cls; f_ty=ty; f_get=fget; f_set=fset}

let name_label = field ~name:"name_label" ~cls:VM ~ty:String ~fget:(fun vm -> vm.name_label) ~fset:(fun x vm -> {vm with name_label=x})
let vBDs = field ~name:"VBDs" ~cls:VM ~ty:(List (Refv VBD)) ~fget:(fun vm -> vm.vBDs) ~fset:(fun x vm -> {vm with vBDs=x})

let vm : vm rval typ = Record {r_fields = [B name_label; B vBDs]; r_empty=empty_vm}

let get_obj : 'a rval -> ('a, 'b) field -> 'b = fun v field -> field.f_get v.v

let set_obj : 'a rval -> ('a, 'b) field -> 'b -> 'a rval = fun v field x -> {v=field.f_set x v.v}

let set : 'a Ref.t -> ('a,'b) field -> 'b -> unit = fun ref field v ->
  db := set_field (table_of field.f_cls) (Ref.string_of ref) field.f_name (to_rpc field.f_ty v) !db

let get : 'a Ref.t -> ('a,'b) field -> 'b = fun ref field ->
  get_field (table_of field.f_cls) (Ref.string_of ref) field.f_name !db |> (of_rpc field.f_ty)

