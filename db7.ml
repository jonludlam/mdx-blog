(* DB7 - Records *)

(* Records *)


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
module Value = struct
  type t =
    | Int : int -> t
    | Float : float -> t
    | String : string -> t
    | List : t list -> t
    | Map : (t * t) list -> t
    | Dict : (string * t) list -> t

  let rec to_string : t -> string = fun v ->
    match v with
    | String s -> Printf.sprintf "\"%s\"" s
    | Int i -> Printf.sprintf "%d" i
    | Float f -> Printf.sprintf "%f" f
    | List xs -> Printf.sprintf "[%s]" (String.concat ";" (List.map to_string xs))
    | Map kvs -> Printf.sprintf "[%s]" (String.concat ";" (List.map (fun (k,v) -> Printf.sprintf "(%s,%s)" (to_string k) (to_string v)) kvs))
    | Dict kvs -> Printf.sprintf "[%s]" (String.concat ";" (List.map (fun (k,v) -> Printf.sprintf "(%s,%s)" k (to_string v)) kvs))
end

module Row = struct
  include Make(Value)
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
  |> Row.update fldname (Value.String "")
  |> Table.update objref Row.empty
  |> TableSet.update tblname Table.empty

let db = ref Database.empty

type _ cls =
  | VM : [`vm] cls
  | VBD : [`vbd] cls

(* Phantom-type-using references *)
module Ref : sig
  type 'c t = private S: string -> 'c t
  val string_of : 'c t -> string
  val of_string : 'c cls -> string -> 'c t
  val pp : Format.formatter -> 'c t -> unit
end = struct
  type 'c t = S : string -> 'c t
  let string_of (S v) = v
  let of_string _ v = (S v)
  let pp fmt (S v) = Format.pp_print_string fmt v
end

type _ typ =
  | String : string typ
  | Int : int typ
  | Refv : 'c cls -> 'c Ref.t typ
  | Set : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | Record : 'a record -> 'a rval typ

and 's record = {
  fields : 's boxed_field list
}

and 's boxed_field =
  B : ('s, 'f) field -> 's boxed_field

and ('s, 'f) field = {
  field_name : string;
  typ : 'f typ;
}

and 's rval = {
  fvals : Value.t StringMap.t
}

exception RTTE

let rec to_value : type a. a typ -> a -> Value.t = fun typ v ->
  match typ with
  | String -> Value.String v
  | Int -> Value.Int v
  | Refv _ -> Value.String (Ref.string_of v)
  | Set ty -> Value.List (List.map (to_value ty) v)
  | Map (ty1,ty2) ->
    let dict =
      List.map (fun (v1,v2) ->
        to_value ty1 v1,
        to_value ty2 v2) v
    in
    Value.Map dict
  | Record r ->
    let d = List.fold_left
      (fun a f' ->
        match f' with
        | B f -> (f.field_name, StringMap.find f.field_name v.fvals)::a)
      [] r.fields in
    Value.Dict d

let rec of_value : type a. a typ -> Value.t -> a = fun typ v ->
  match typ, v with
  | String, Value.String str -> str
  | Int, Value.Int i -> i
  | Refv cls, Value.String s -> Ref.of_string cls s
  | Set ty, Value.List vs -> List.map (of_value ty) vs
  | Map (ty1, ty2), Value.Map kvs ->
    List.map (fun (k,v) ->
      of_value ty1 k,
      of_value ty2 v
      ) kvs
  | Record r, Value.Dict fs ->
    let fvals =
      List.fold_left
        (fun a f' ->
          match f' with B f -> StringMap.add f.field_name (List.assoc f.field_name fs) a
          )
        StringMap.empty r.fields
    in { fvals }
  | _, _ -> raise RTTE


(* Better: *)

module Field = struct
  type ('c,'f) t = {
    name : string;
    cls : 'c cls;
    ty : 'f typ;
  }
  let construct name cls ty = {name; cls; ty}
  let name_of x = x.name
  let table_of : type c. (c, _) t -> string = fun x -> match x.cls with | VM -> "VM" | VBD -> "VBD"
  let to_value f v = to_value f.ty v
  let of_value f v = of_value f.ty v
end

let set : 'a Ref.t -> ('a,'b) Field.t -> 'b -> unit = fun ref field v ->
  let open Field in
  db := set_field (table_of field) (Ref.string_of ref) (name_of field) (to_value field v) !db

let get : 'a Ref.t -> ('a,'b) Field.t -> 'b = fun ref field ->
  let open Field in
  get_field (table_of field) (Ref.string_of ref) (name_of field) !db |> (of_value field)

(* Example showing how we would use these in practice *)
let dump db =
  TableSet.iter (fun tblname table ->
    Printf.printf "\n# TABLE: %s\n\n" tblname;
    Table.iter (fun objref row ->
      Printf.printf "## Object: %s\n" objref;
      Row.iter (fun fldname v ->
        Printf.printf "  %s: %s\n" fldname (Value.to_string v)) row) table) db

let vm1 = Ref.of_string VM "OpaqueRef:abcde"
let vbd1 = Ref.of_string VBD "OpaqueRef:fghij"
let vbd2 = Ref.of_string VBD "OpaqueRef:12345"

let name_label = Field.construct "name_label" VM String
let vBDs = Field.construct "VBDs" VM (Set (Refv VBD))
let vbd_vm = Field.construct "VM" VBD (Refv VM)
let memory = Field.construct "memory" VM Int

let of_field : type s. (s, _) Field.t -> s boxed_field = fun f ->
  B { field_name=f.Field.name; typ = f.Field.ty }

let vm : [`vm] rval typ = Record {fields = [of_field name_label; of_field vBDs]}

let _ =
  set vm1 name_label "my first vm";
  set vbd1 vbd_vm vm1;
  set vbd2 vbd_vm vm1;
  set vm1 vBDs [vbd1; vbd2];
  set vm1 memory 63356;
  dump !db

