(* DB12 - uniform access *)

(* Example usage *)
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
  type ('a, 'b) t = {
    map: 'b inner;
  }
  val mem : 'a Ref.t -> ('a, 'b) t -> bool
  val add : 'a Ref.t -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val update : 'a Ref.t -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val find : 'a Ref.t -> ('a, 'b) t -> 'b
  val keys : ('a, 'b) t -> string list
  val empty : ('a, 'b) t
end = struct
  module M = Map.Make(String)
  type 'b inner = 'b M.t
  type ('a, 'b) t = {
    map: 'b M.t;
  }
  let mem key m = M.mem (Ref.string_of key) m.map
  let add key v m = { map = M.add (Ref.string_of key) v m.map }
  let update key v m =
    let key' = Ref.string_of key in
    {map=M.add key' v (M.remove key' m.map)}
  let find key m = M.find (Ref.string_of key) m.map
  let keys m = M.fold (fun k _ acc -> k::acc) m.map []
  let empty = {map=M.empty}
end


type _ typ =
  | String : string typ
  | Int : int64 typ
  | Refv : 'a cls -> 'a Ref.t typ
  | List : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | RefMap : 'a cls * 'b typ -> ('a, 'b) RefMap.t record typ
  | Record : 'a record -> 'a record typ

and 's record = {
  r_fields : 's -> 's boxed_field list;
  r : 's;
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
  vms : (vm, vm record) RefMap.t record;
  vbds : (vbd, vbd record) RefMap.t record;
}

let mk_record_val : 'a -> 'a record -> 'a record = fun v rv -> { rv with r = v; }

type _ cls += VM : vm cls
type _ cls += VMs : (vm, vm record) RefMap.t cls
type _ cls += VBD : vbd cls
type _ cls += VBDs : (vbd, vbd record) RefMap.t cls

type _ cls += DB : db cls

let empty_vm = {name_label=""; vBDs=[]; other_config=[]}
let empty_vbd = {vM=Ref.of_string VM "null"}



let field ~name ~cls ~ty ~fget ~fset = {f_name=name; f_cls=cls; f_ty=ty; f_get=fget; f_set=fset}

let get_obj : ('a, 'b) field -> 'a record -> 'b = fun field v -> field.f_get v.r
let add_obj : ('a, 'b) field -> 'b -> 'a record -> 'a record = fun field x v -> {v with r = field.f_set x v.r}
let update_obj : ('a, 'b) field -> ('b -> 'b) -> 'a record -> 'a record = fun field f v -> {v with r = field.f_set (f (field.f_get v.r)) v.r}

let objfield ref cls ty = {f_name=Ref.string_of ref; f_cls=cls; f_ty=ty; f_get=(fun objs -> RefMap.find ref objs); f_set=(fun obj objs -> RefMap.update ref obj objs)}

let name_label = field ~name:"name_label" ~cls:VM ~ty:String ~fget:(fun vm -> vm.name_label) ~fset:(fun x vm -> {vm with name_label=x})
let vBDs = field ~name:"VBDs" ~cls:VM ~ty:(List (Refv VBD)) ~fget:(fun vm -> vm.vBDs) ~fset:(fun x vm -> {vm with vBDs=x})
let other_config = field ~name:"other_config" ~cls:VM ~ty:(Map (String, String)) ~fget:(fun vm -> vm.other_config) ~fset:(fun x vm -> {vm with other_config=x})
let vM = field ~name:"vM" ~cls:VBD ~ty:(Refv VM) ~fget:(fun vbd -> vbd.vM) ~fset:(fun x vbd -> {vM=x})

let vm_record = {r_fields = (fun _ -> [B name_label; B vBDs; B other_config]); r=empty_vm}
let vm = Record vm_record
let vbd_record = {r_fields = (fun _ -> [B vM]); r=empty_vbd}
let vbd = Record vbd_record

let vms : (vm,vm record) RefMap.t record = {r_fields = (fun r -> List.map (fun ref -> B (objfield (Ref.of_string VM ref) VMs vm)) (RefMap.keys r)); r = RefMap.empty}
let vbds : (vbd,vbd record) RefMap.t record = {r_fields = (fun r -> List.map (fun ref -> B (objfield (Ref.of_string VBD ref) VBDs vbd)) (RefMap.keys r)); r = RefMap.empty}

let db_vms = field ~name:"vms" ~cls:DB ~ty:(RefMap (VM, vm)) ~fget:(fun db -> db.vms) ~fset:(fun x db -> {db with vms=x})
let db_vbds = field ~name:"vbds" ~cls:DB ~ty:(RefMap (VBD, vbd)) ~fget:(fun db -> db.vbds) ~fset:(fun x db -> {db with vbds=x})

let db = {r_fields = (fun _ -> [B db_vms; B db_vbds]); r={vms=(mk_record_val RefMap.empty vms); vbds=(mk_record_val RefMap.empty vbds)}}

let maindb = ref db

let find_objs : type a. a cls -> (a, a record) RefMap.t cls * (db, (a, a record) RefMap.t record) field * (a record typ) = function
  | VM -> VMs, db_vms, vm
  | VBD -> VBDs, db_vbds, vbd
  | _ -> failwith "Invalid type"

let operate : 'a Ref.t -> ((db, ('a, 'a record) RefMap.t record) field -> (('a, 'a record) RefMap.t, 'a record) field -> 'b) -> 'b = fun ref f ->
  let cls = Ref.cls_of ref in
  let fs',f',f_ty' = find_objs cls in
  let objfield = objfield ref fs' f_ty' in
  f f' objfield

let add : 'a Ref.t -> 'a record -> unit = fun ref x ->
  (* Construct a 'field' type to access the object via the 'field_name' of 'ref' *)
  operate ref (fun tablefield objfield ->
    let mydb = !maindb in
    let new_db = update_obj tablefield (add_obj objfield x) mydb in
    maindb := new_db)

let update : 'a Ref.t -> ('a,'b) field -> ('b -> 'b) -> unit = fun ref field f ->
  operate ref (fun tablefield objfield ->
    let mydb = !maindb in
    let new_db = update_obj tablefield (update_obj objfield (update_obj field (f))) mydb in
    maindb := new_db)

let get : 'a Ref.t -> ('a,'b) field -> 'b = fun ref field ->
  operate ref (fun tablefield objfield ->
    !maindb |> get_obj tablefield |> get_obj objfield |> get_obj field)

let set : 'a Ref.t -> ('a,'b) field -> 'b -> unit = fun ref field x ->
  update ref field (fun _ -> x)

let add_to : 'a Ref.t -> ('a, ('c * 'd) list) field -> 'c -> 'd -> unit = fun ref field k v ->
  update ref field (fun x -> (k,v)::List.remove_assoc k x)


(* Let's see what neat things we can do now *)

let rec marshal : type a. a typ -> a -> Rpc.t = fun ty x ->
  match ty with
  | String -> Rpc.String x
  | Int -> Rpc.Int x
  | Refv cls -> Rpc.String (Ref.string_of x)
  | List ty' -> Rpc.Enum (List.map (marshal ty') x)
  | Map (ty1, ty2) -> Rpc.Dict (List.map (fun (k,v) -> ((match marshal ty1 k with | Rpc.String x -> x | _ -> failwith "Expecting stringish keys"), marshal ty2 v)) x)
  | RefMap (cls, ty) ->
    let fields = x.r_fields x.r in
    Rpc.Dict (List.map (function B field -> (field.f_name, marshal field.f_ty (field.f_get x.r))) fields)
  | Record _ ->
    let fields = x.r_fields x.r in
    Rpc.Dict (List.map (function B field -> (field.f_name, marshal field.f_ty (field.f_get x.r))) fields)

let dump_db db = Printf.sprintf "%s\n" (marshal (Record db) db |> Jsonrpc.to_string)

let vm1 = Ref.of_string VM "vm1"
let vm2 = Ref.of_string VM "vm2"
let vbd1 = Ref.of_string VBD "vbd1"
let _ =
  add vm1 (mk_record_val empty_vm vm_record);
  add vm2 (mk_record_val empty_vm vm_record);
  add vbd1 (mk_record_val empty_vbd vbd_record);
  set vm1 name_label "my new name";
  add_to vm2 other_config "key" "value";
  dump_db !maindb


