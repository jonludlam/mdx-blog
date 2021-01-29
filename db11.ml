(* DB11 - efficient ref->obj map *)

(* Example usage *)

exception RTTI

type _ cls = ..

module Ref : sig
  type 'a t = S of ('a cls * string)
  val string_of : 'a t -> string
  val of_string : 'a cls -> string -> 'a t
  val cls_of : 'a t -> 'a cls
  val compare : 'a t -> 'a t -> int
  val pp : Format.formatter -> 'a t -> unit
end = struct
  type 'a t = S of ('a cls * string)
  let string_of (S (cls,v)) = v
  let of_string cls v = (S (cls,v))
  let cls_of (S (cls,v)) = cls
  let compare (S (_,a)) (S (_,b)) = String.compare a b
  let pp fmt (S (cls,v)) = Format.pp_print_string fmt v
end


module RefMap : sig
  type 'b inner
  type ('a, 'b) t = S of 'b inner

  val mem : 'a Ref.t -> ('a, 'b) t -> bool
  val add : 'a Ref.t -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val update : 'a Ref.t -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val find : 'a Ref.t -> ('a, 'b) t -> 'b
  val empty : ('a, 'b) t
end = struct
  module M = Map.Make(String)
  type 'b inner = 'b M.t
  type ('a, 'b) t = S of 'b M.t

  let wrap x = S x

  let mem key (S map) = M.mem (Ref.string_of key) map
  let add key v (S map) = M.add (Ref.string_of key) v map |> wrap
  let update key v m =
    let S map = m in
    let key' = Ref.string_of key in
 (* 4.06 only   M.update key (function | Some x as res -> if x=v then res else Some v | None -> Some v) map *)
    let unchanged =
      match M.find_opt key' map with
      | None -> false
      | Some v' -> v = v'
    in
    if unchanged then m else M.add key' v (M.remove key' map) |> wrap
  let find key (S map) = M.find (Ref.string_of key) map
  let empty = S (M.empty)
end


type _ typ =
  | String : string typ
  | Int : int64 typ
  | Refv : 'a cls -> 'a Ref.t typ
  | List : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | RefMap : 'a cls * 'b typ -> ('a, 'b) RefMap.t typ
  | Record : 'a record -> 'a typ

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

type vm = {
  name_label : string;
  vBDs : vbd Ref.t list;
  other_config : (string * string) list;
}
and vbd = {
  vM : vm Ref.t;
}

type db = {
  vms : (vm, vm) RefMap.t;
  vbds : (vbd, vbd) RefMap.t;
}

type _ cls += VM : vm cls
type _ cls += VBD : vbd cls

type _ cls += DB : db cls

let empty_vm = {name_label=""; vBDs=[]; other_config=[]}
let empty_vbd = {vM=Ref.of_string VM "null"}


let db = ref { vms=RefMap.empty; vbds=RefMap.empty; }

(* Better: *)

let field ~name ~cls ~ty ~fget ~fset = {f_name=name; f_cls=cls; f_ty=ty; f_get=fget; f_set=fset}

let name_label = field ~name:"name_label" ~cls:VM ~ty:String ~fget:(fun vm -> vm.name_label) ~fset:(fun x vm -> {vm with name_label=x})
let vBDs = field ~name:"VBDs" ~cls:VM ~ty:(List (Refv VBD)) ~fget:(fun vm -> vm.vBDs) ~fset:(fun x vm -> {vm with vBDs=x})
let other_config = field ~name:"other_config" ~cls:VM ~ty:(Map (String, String)) ~fget:(fun vm -> vm.other_config) ~fset:(fun x vm -> {vm with other_config=x})
let vM = field ~name:"vM" ~cls:VBD ~ty:(Refv VM) ~fget:(fun vbd -> vbd.vM) ~fset:(fun x vbd -> {vM=x})

let vm = Record {r_fields = [B name_label; B vBDs; B other_config]; r_empty=empty_vm}
let vbd = Record {r_fields = [B vM]; r_empty=empty_vbd}

let db_vms = field ~name:"vms" ~cls:DB ~ty:(RefMap (VM, vm)) ~fget:(fun db -> db.vms) ~fset:(fun x db -> {db with vms=x})
let db_vbds = field ~name:"vbds" ~cls:DB ~ty:(RefMap (VBD, vbd)) ~fget:(fun db -> db.vbds) ~fset:(fun x db -> {db with vbds=x})

let get_obj : 'a -> ('a, 'b) field -> 'b = fun v field -> field.f_get v
let set_obj : 'a -> ('a, 'b) field -> 'b -> 'a = fun v field x -> field.f_set x v

let find_objs : type a. a cls -> (db, (a, a) RefMap.t) field = function
  | VM -> db_vms
  | VBD -> db_vbds
  | _ -> failwith "Invalid type"

let add : 'a Ref.t -> 'a -> unit = fun ref x ->
  let f' = find_objs (Ref.cls_of ref) in
  let objs = get_obj !db f' in
  db := set_obj !db f' (RefMap.add ref x objs)

let set : 'a Ref.t -> ('a,'b) field -> 'b -> unit = fun ref field v ->
  let f' = find_objs field.f_cls in
  let objs = get_obj !db f' in
  let new_obj = set_obj (RefMap.find ref objs) field v in
  db := set_obj !db f' (RefMap.update ref new_obj objs)

let get : 'a Ref.t -> ('a,'b) field -> 'b = fun ref field ->
  let f' = find_objs field.f_cls in
  let objs = get_obj !db f' in
  get_obj (RefMap.find ref objs) field

let add_to : 'a Ref.t -> ('a, ('c * 'd) list) field -> 'c -> 'd -> unit = fun ref field k v ->
  let f' = find_objs field.f_cls in
  let objs = get_obj !db f' in
  let o = RefMap.find ref objs in
  let kvs = get_obj o field |> List.filter (fun (k',_) -> k <> k') |> fun x -> (k,v)::x in
  let o' = set_obj o field kvs in
  db := set_obj !db f' (RefMap.update ref o' objs)


