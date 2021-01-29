(* DB13 - events *)

(* Example usage *)
type _ cls = ..

module Stat = struct
  type t = {
    created : int64;
    modified : int64;
    deleted : int64;
  }
  let make g = { created = g; modified = g; deleted = 0L }
  let update g t = { t with modified = g }
  let update_named g name times =
    let t = List.assoc name times in
    (name,update g t)::(List.remove_assoc name times)
end

module RefMap : sig
  type 'a inner
  type 'a t = { m : 'a inner }

  val mem : string -> 'a t -> bool
  val add : string -> 'a -> 'a t -> 'a t
  val remove : string -> 'a t -> 'a t
  val update : string -> 'a -> 'a t -> 'a t
  val find : string -> 'a t -> 'a
  val keys : 'a t -> string list
  val empty : 'a t
end = struct
  module M = Map.Make(String)
  type 'a inner = 'a M.t
  type 'a t = { m : 'a M.t }

  let mem key m = M.mem key m.m
  let add key v m = {m = M.add key v m.m}
  let remove key m = {m = M.remove key m.m}
  let update key v m =
    let key' = key in
    {m = M.add key' v (M.remove key' m.m)}
  let find key m = M.find key m.m
  let keys m = M.fold (fun k _ acc -> k::acc) m.m []
  let empty = {m=M.empty}
end


type _ typ =
  | String : string typ
  | Int : int64 typ
  | Refv : 'a cls -> 'a ref typ
  | List : 'a typ -> 'a list typ
  | Option : 'a typ -> 'a option typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | RefMap : 'a typ -> 'a RefMap.t typ
  | Record : 'a record -> 'a typ

and 's record = {
  r_fields : 's -> 's boxed_field list;
  r_times : (string * Stat.t) list;
(*  r : 's; *)
}

and 's boxed_field =
  B : ('s, 'f, 'k) field -> 's boxed_field

and 'a refs = 'a RefMap.t

and 'a ref = Ref : ('a RefMap.t, 'a option, [`rw]) field * 'a cls -> 'a ref

and _ rw =
  | RO : [`ro] rw
  | RW : [`rw] rw

and ('s, 'f, 'r) field = {
  f_name : string;
  f_ty : 'f typ;
  f_rw : 'r rw;
  f_cls : 's cls;
  f_get : 's -> 'f;
  f_set : 'f -> 's -> 's;
}

and ('a, 'b) otm_pair = (('a, 'b ref, [`rw]) field * ('b, 'a ref list, [`ro]) field)

and relation = Rel : ('a, 'b) otm_pair -> relation

let make_ref cls cls' ty name = Ref ({
  f_get = (fun r -> try Some (RefMap.find name r) with Not_found -> Printf.printf "No ref for %s (refs are: [%s])\n%!" name (String.concat ";" (RefMap.keys r)); None);
  f_set = (fun v r -> match v with | Some v -> RefMap.update name v r | None -> RefMap.remove name r);
  f_name = name;
  f_rw = RW;
  f_cls = cls';
  f_ty = Option ty;
}, cls)

let compose : ('a, 'b, [`rw]) field -> ('b, 'c, 'd) field -> ('a, 'c, 'd) field = fun f1 f2 ->
  { f_name = (Printf.sprintf "%s.%s" f1.f_name f2.f_name);
    f_ty = f2.f_ty;
    f_get = (fun x -> f2.f_get (f1.f_get x));
    f_set = (fun x r -> f1.f_set (f2.f_set x (f1.f_get r)) r);
    f_cls = f1.f_cls;
    f_rw = f2.f_rw;
  }

let opt_compose : ('a, 'b option, [`rw]) field -> ('b, 'c, 'd) field -> ('a, 'c, 'd) field = fun f1 f2 ->
  { f_name = (Printf.sprintf "%s?.%s" f1.f_name f2.f_name);
    f_ty = f2.f_ty;
    f_get = (fun x -> match f1.f_get x with | Some y -> f2.f_get y | None -> failwith (Printf.sprintf "Missing row: %s" f1.f_name));
    f_set = (fun x r -> match f1.f_get r with | Some y -> f1.f_set (Some (f2.f_set x y)) r | None -> r);
    f_cls = f1.f_cls;
    f_rw = f2.f_rw;
  }

let name_of_ref = function | Ref (x,_) -> x.f_name

let cls_of_ref = function | Ref (x,cls) -> cls

let field ~name ~ty ~rw ~cls ~fget ~fset = {f_name=name; f_ty=ty; f_cls=cls; f_rw=rw; f_get=fget; f_set=fset} 

type vm = {
  vM_uuid : string;
  vM_name_label : string;
  vM_VBDs : vbd ref list;
  vM_other_config : (string * string) list;
}
and vbd = {
  vBD_VM : vm ref;
}

type db = {
  vms : vm RefMap.t;
  vbds : vbd RefMap.t;
}

type _ cls += VM : vm cls
type _ cls += VMs : vm refs cls
type _ cls += VBD : vbd cls
type _ cls += VBDs : vbd refs cls
type _ cls += DB : db cls

module VM = struct
  let uuid =
    field
    ~name:"uuid"
    ~rw:RO
    ~cls:VM
    ~ty:String
    ~fget:(fun vm -> vm.vM_uuid)
    ~fset:(fun x vm -> {vm with vM_uuid=x})

  let name_label =
    field
      ~name:"name_label"
      ~cls:VM
      ~rw:RW
      ~ty:String
      ~fget:(fun vm -> vm.vM_name_label)
      ~fset:(fun x vm -> {vm with vM_name_label=x})

  let vBDs =
    field
      ~name:"VBDs"
      ~cls:VM
      ~rw:RO 
      ~ty:(List (Refv VBD))
      ~fget:(fun vm -> vm.vM_VBDs)
      ~fset:(fun x vm -> {vm with vM_VBDs=x})
  
  let other_config =
    field
      ~name:"other_config"
      ~rw:RW
      ~cls:VM
      ~ty:(Map (String, String))
      ~fget:(fun vm -> vm.vM_other_config)
      ~fset:(fun x vm -> {vm with vM_other_config=x})

  let empty_vm = {vM_uuid=""; vM_name_label=""; vM_VBDs=[]; vM_other_config=[]}
  let vm_record = let fields = [B uuid; B name_label; B vBDs; B other_config] in {r_fields = (fun _ -> fields); (*r=empty_vm;*) r_times=[]}
  let vm = Record vm_record
  let null_ref = make_ref VM VMs vm "OpaqueRef:NULL"
  let make_ref name = make_ref VM VMs vm name
end

module VBD = struct
  let vM =
    field
      ~name:"vM"
      ~cls:VBD
      ~ty:(Refv VM)
      ~rw:RW
      ~fget:(fun vbd -> vbd.vBD_VM)
      ~fset:(fun x vbd -> {vBD_VM=x})

  let empty_vbd = {vBD_VM=VM.null_ref}
  let vbd_record = let fields = [B vM] in {r_fields = (fun _ -> fields); (* r=empty_vbd; *) r_times=[]}
  let vbd = Record vbd_record
  let null_ref = make_ref VBD VBDs vbd "OpaqueRef:NULL"
  let make_ref name = make_ref VBD VBDs vbd name
end


let vbd_vm : (vbd, vm) otm_pair = (VBD.vM, VM.vBDs)
let rels = [ Rel vbd_vm ]

module DB = struct
  let vms = field ~name:"vms" ~cls:DB ~rw:RW ~ty:(RefMap VM.vm) ~fget:(fun db -> db.vms) ~fset:(fun x db -> {db with vms=x})
  let vbds = field ~name:"vbds" ~cls:DB ~rw:RW ~ty:(RefMap VBD.vbd) ~fget:(fun db -> db.vbds) ~fset:(fun x db -> {db with vbds=x})

  let empty = {
    vms=RefMap.empty;
    vbds=RefMap.empty;
  }

  let find_objs : type a. a cls -> (db, a RefMap.t, _) field = function
    | VM -> vms
    | VBD -> vbds
    | _ -> failwith "Invalid type"

  let db_record = let fields = [ B vms; B vbds ] in {r_fields = (fun _ -> fields); r_times=[] (* r=db *)}
end
  
let maindb = ref (0L, DB.empty)




let operate : 'a ref -> ((db, 'a RefMap.t, _) field -> ('a RefMap.t, 'a option, _) field -> 'b) -> 'b = fun ref f ->
  let cls = cls_of_ref ref in
  let f' = DB.find_objs cls in
  match ref with | Ref (f'', _) -> 
    f f' f''

let fld_update : (db, 'a, _) field -> ('a -> 'a) -> unit = fun fld f ->
  let (gen,mydb) = !maindb in
  let gen' = Int64.add gen 1L in
  maindb := (gen', fld.f_set (f (fld.f_get mydb)) mydb)


let add : 'a ref -> 'a -> unit = fun ref x ->
  (* Construct a 'field' type to access the object via the 'field_name' of 'ref' *)
  operate ref (fun db_field refmap_field ->
    fld_update (compose db_field refmap_field) (fun _ -> Some x))

let remove : 'a ref -> unit = fun ref ->
  operate ref (fun db_field refmap_field ->
    fld_update (compose db_field refmap_field) (fun _ -> None))

let update : 'a ref -> ('a,'b,_) field -> ('b -> 'b) -> unit = fun ref field f ->
  operate ref (fun tablefield objfield ->
    let fld = opt_compose (compose tablefield objfield) field in
    fld_update fld f)

let get : 'a ref -> ('a,'b,_) field -> 'b = fun ref field ->
  operate ref (fun tablefield objfield ->
    let fld = opt_compose (compose tablefield objfield) field in
    snd !maindb |> fld.f_get)

type (_,_) eq = Eq : ('a, 'a) eq

(*
  | String : string typ
  | Int : int64 typ
  | Refv : 'a cls -> 'a Ref.t typ
  | List : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | RefMap : 'a cls * 'b typ -> ('a, 'b) RefMap.t record typ
  | Record : 'a record -> 'a record typ
*)
let rec eq_cls : type a b. a cls -> b cls -> (a,b) eq option =
  fun c1 c2 ->
  match c1, c2 with
  | VM, VM -> Some Eq
  | VMs, VMs -> Some Eq
  | VBD, VBD -> Some Eq
  | VBDs, VBDs -> Some Eq
  | _, _ -> None
and eq_ty : type a b. a typ -> b typ -> (a,b) eq option =
  fun t1 t2 ->
  match t1, t2 with
  | String, String -> Some Eq
  | Int, Int -> Some Eq
  | Refv x, Refv y -> begin
    match eq_cls x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | List x, List y -> begin
    match eq_ty x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | Map (x,y), Map (a,b) -> begin
    match eq_ty x a, eq_ty y b with
    | Some Eq, Some Eq -> Some Eq
    | _, _ -> None
    end
  | RefMap x, RefMap a -> begin
    match eq_ty x a with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | Record x, Record y -> None
  | Option x, Option y -> begin
    match eq_ty x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | _, _ -> None
and eq_rw : type a b. a rw -> b rw -> (a,b) eq option =
  fun r1 r2 ->
  match r1, r2 with
  | RW, RW -> Some Eq
  | RO, RO -> Some Eq
  | _, _ -> None

let eq_field : type a b c d e f. (a,b,c) field -> (d,e,f) field -> ((a * b * c),(d * e * f)) eq option =
  fun f1 f2 ->
    match eq_cls f1.f_cls f2.f_cls, eq_ty f1.f_ty f2.f_ty, eq_rw f1.f_rw f2.f_rw with
    | Some Eq, Some Eq, Some Eq -> Some Eq
    | _, _, _ -> None


let setrel : type a b. a ref -> (a, b) otm_pair -> b ref -> unit = fun refo (fieldo, fieldm) refm ->
  let before = get refo fieldo in
  update refo fieldo (fun _ -> refm);
  begin
    try 
      update before fieldm (List.filter (fun r -> r <> refo));
    with _ -> ()
  end;
  begin
    try
      update refm fieldm (fun x -> refo :: x)
    with _ -> ()
  end

let set : type a b. a ref -> (a,b,[`rw]) field -> b -> unit = fun ref field x ->
  let is_rel =
    List.fold_left (fun acc r ->
      match r with
      | Rel (f1,f2) -> begin
        match eq_field field f1 with
        | Some Eq -> setrel ref (f1,f2) x; true
        | _ -> acc
        end) false rels
  in
  if not is_rel then update ref field (fun _ -> x)

let add_to : 'a ref -> ('a, ('c * 'd) list, [`rw]) field -> 'c -> 'd -> unit = fun ref field k v ->
  update ref field (fun x -> (k,v)::List.remove_assoc k x)



(* Let's see what neat things we can do now *)

let rec marshal : type a. a typ -> a -> Rpc.t = fun ty x ->
  match ty with
  | String -> Rpc.String x
  | Int -> Rpc.Int x
  | Refv cls -> (match x with Ref (r,_) -> Rpc.String (r.f_name))
  | List ty' -> Rpc.Enum (List.map (marshal ty') x)
  | Map (ty1, ty2) -> Rpc.Dict (List.map (fun (k,v) -> ((match marshal ty1 k with | Rpc.String x -> x | _ -> failwith "Expecting stringish keys"), marshal ty2 v)) x)
  | RefMap ty ->
    let fields = RefMap.keys x in
    Rpc.Dict (List.map (function field -> (field, marshal ty (RefMap.find field x))) fields)
  | Record r ->
    let fields = r.r_fields x in
    Rpc.Dict (List.map (function B field -> (field.f_name, marshal field.f_ty (field.f_get x))) fields)


let dump_db db = Printf.printf "%s\n" (marshal (Record DB.db_record) db |> Jsonrpc.to_string)

let vm1 = VM.make_ref "vm1"
let vm2 = VM.make_ref "vm2"
let vbd1 = VBD.make_ref "vbd1"
let _ =
  add vm1 VM.empty_vm;
  Printf.printf "here...\n%!";
  dump_db (snd !maindb);  
  add vm2 VM.empty_vm;
  Printf.printf "here2...\n%!";
  dump_db (snd !maindb);  
  add vbd1 VBD.empty_vbd;
  Printf.printf "here3...\n%!";
  dump_db (snd !maindb);  
  set vm1 VM.name_label "my new name";
  Printf.printf "here4...\n%!";
  dump_db (snd !maindb);  
  add_to vm2 VM.other_config "key" "value";
  dump_db (snd !maindb);
  set vbd1 VBD.vM vm1;
  dump_db (snd !maindb)

