(* DB5 - GADTs for marshalling *)

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
  val to_string : ('c, 'f) t -> 'f -> string
  val of_string : ('c, 'f) t -> string -> 'f
end = struct
  type ('c,'f) t = {
    name : string;
    cls : 'c cls;
    ty : 'f typ;
  }
  let name_of x = x.name
  let table_of : type c. (c, _) t -> string = fun x -> match x.cls with | VM -> "VM" | VBD -> "VBD"
  let construct name cls ty = {name; cls; ty}
  let to_string f v = to_string f.ty v
  let of_string f v = of_string f.ty v
end

let set : 'a Ref.t -> ('a,'b) Field.t -> 'b -> unit = fun ref field v ->
  let open Field in
  db := set_field (table_of field) (Ref.string_of ref) (name_of field) (to_string field v) !db

let get : 'a Ref.t -> ('a,'b) Field.t -> 'b = fun ref field ->
  let open Field in
  get_field (table_of field) (Ref.string_of ref) (name_of field) !db |> (of_string field)

(* Example showing how we would use these in practice *)
let dump db =
  TableSet.iter (fun tblname table ->
    Printf.printf "\n# TABLE: %s\n\n" tblname;
    Table.iter (fun objref row ->
      Printf.printf "## Object: %s\n" objref;
      Row.iter (fun fldname v ->
        Printf.printf "  %s: %s\n" fldname v) row) table) db

let vm1 = Ref.of_string VM "OpaqueRef:abcde"
let vbd1 = Ref.of_string VBD "OpaqueRef:fghij"
let vbd2 = Ref.of_string VBD "OpaqueRef:12345"

let name_label = Field.construct "name_label" VM String
let vBDs = Field.construct "VBDs" VM (Set (Refv VBD))
let vbd_vm = Field.construct "VM" VBD (Refv VM)
let memory = Field.construct "memory" VM Int

let _ =
  set vm1 name_label "my first vm";
  set vbd1 vbd_vm vm1;
  set vbd2 vbd_vm vm1;
  set vm1 vBDs [vbd1; vbd2];
  set vm1 memory 63356;
  dump !db

