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

module StatTree = struct
  type 'a t = Node of Stat.t * 'a option * (string * 'a t) list

  let empty = Node (Stat.make 0L, None, [])

  let rec get_stat : string list -> 'a t -> Stat.t = fun path t ->
    match path, t with
    | [], Node (st,_,_) -> st
    | s::ss, Node (st,_,children) -> get_stat ss (List.assoc s children)

  let rec update_stat : int64 -> string list -> 'a option -> 'a t option -> 'a t = fun g path a t ->
    match path, t with
    | [], Some (Node (st, x, xs)) -> Node (Stat.update g st, a, xs)
    | [], None -> Node (Stat.make g, a, [])
    | s::ss, None ->
      Node (Stat.make g, None, [s, update_stat g ss a None])
    | s::ss, Some (Node (st, thisa, xs)) ->
      let mine,others = List.partition (fun (x,n) -> x=s) xs in
      let new_mine =
        match mine with
        | [] -> (s, update_stat g ss a None)
        | [(_,n)] -> (s, update_stat g ss a (Some n))
        | _ -> failwith "Multiple keys in update_stat"
      in
      Node (Stat.update g st, thisa, new_mine::others)

  let rec fold_over_recent : int64 -> ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b = fun g f acc t ->
    match t with
    | Node (st, v, children) ->
      if st.Stat.modified >= g then
        match v,children with
        | Some x , [] -> List.fold_right (fun (_,x) acc -> fold_over_recent g f acc x) children (f x acc)
        | Some x , children when st.Stat.created >= g -> f x acc
        | _, _ -> List.fold_right (fun (_,x) acc -> fold_over_recent g f acc x) children acc
      else acc


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
  | Dummy : 'a typ

and 's record = {
  r_fields : 's -> 's boxed_field list;
}

and 's boxed_field =
  B : ('s, 'f, 'k) field -> 's boxed_field

and 'a refs = 'a RefMap.t

and 'a ref = Ref : ('a RefMap.t, 'a option, [`rw]) field * 'a cls -> 'a ref

and _ rw =
  | RO : [`ro] rw
  | RW : [`rw] rw

and ('s, 'f, 'r) field = {
  f_name : string list;
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
  f_name = [name];
  f_rw = RW;
  f_cls = cls';
  f_ty = Option ty;
}, cls)

let compose : ('a, 'b, [`rw]) field -> ('b, 'c, 'd) field -> ('a, 'c, 'd) field = fun f1 f2 ->
  { f_name = f1.f_name @ f2.f_name;
    f_ty = f2.f_ty;
    f_get = (fun x -> f2.f_get (f1.f_get x));
    f_set = (fun x r -> f1.f_set (f2.f_set x (f1.f_get r)) r);
    f_cls = f1.f_cls;
    f_rw = f2.f_rw;
  }

let opt_compose : ('a, 'b option, [`rw]) field -> ('b, 'c, 'd) field -> ('a, 'c, 'd) field = fun f1 f2 ->
  { f_name = f1.f_name @ f2.f_name;
    f_ty = f2.f_ty;
    f_get = (fun x -> match f1.f_get x with | Some y -> f2.f_get y | None -> failwith (Printf.sprintf "Missing row: %s" (String.concat "." f1.f_name)));
    f_set = (fun x r -> match f1.f_get r with | Some y -> f1.f_set (Some (f2.f_set x y)) r | None -> r);
    f_cls = f1.f_cls;
    f_rw = f2.f_rw;
  }

let name_of_ref = function | Ref (x,_) -> x.f_name

let cls_of_ref = function | Ref (x,cls) -> cls

let field ~name ~ty ~rw ~cls ~fget ~fset = {f_name=[name]; f_ty=ty; f_cls=cls; f_rw=rw; f_get=fget; f_set=fset} 

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

type wrapped_field = Fld : (db, _, _) field -> wrapped_field

type _ cls += Stats : wrapped_field StatTree.t cls

let map_fld : type b c. (db, b, c) field -> (wrapped_field StatTree.t, int64, [`rw]) field = fun f ->
  let path = f.f_name in
  { f_name = f.f_name;
    f_ty = Int;
    f_get = (fun st -> let s = StatTree.get_stat path st in s.Stat.modified);
    f_set = (fun g st -> StatTree.update_stat g path (Some (Fld f)) (Some st));
    f_rw = RW;
    f_cls = Stats;
    }

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
  let vm_record = let fields = [B uuid; B name_label; B vBDs; B other_config] in {r_fields = (fun _ -> fields); }
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
  let vbd_record = let fields = [B vM] in {r_fields = (fun _ -> fields);}
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

  let db_record = let fields = [ B vms; B vbds ] in {r_fields = (fun _ -> fields);}
end
  
let maindb = ref (0L, DB.empty, StatTree.empty)
let getdb () = let (_,db,_) = !maindb in db



let operate : 'a ref -> ((db, 'a RefMap.t, _) field -> ('a RefMap.t, 'a option, _) field -> 'b) -> 'b = fun ref f ->
  let cls = cls_of_ref ref in
  let f' = DB.find_objs cls in
  match ref with | Ref (f'', _) -> 
    f f' f''

let fld_update : (db, 'a, _) field -> ('a -> 'a) -> unit = fun fld f ->
  let (gen,mydb,st) = !maindb in
  let gen' = Int64.add gen 1L in
  let otherfld = map_fld fld in
  maindb := (gen', fld.f_set (f (fld.f_get mydb)) mydb, otherfld.f_set gen' st)


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
    let (_,db,_) = !maindb in fld.f_get db)

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
let eq_rw : type a b. a rw -> b rw -> (a,b) eq option =
  fun r1 r2 ->
  match r1, r2 with
  | RW, RW -> Some Eq
  | RO, RO -> Some Eq
  | _, _ -> None

let eq_cls : type a b. a cls -> b cls -> (a,b) eq option =
  fun c1 c2 ->
  match c1, c2 with
  | VM, VM -> Some Eq
  | VMs, VMs -> Some Eq
  | VBD, VBD -> Some Eq
  | VBDs, VBDs -> Some Eq
  | _, _ -> None

type eqtest = Test : ('a cls -> 'b cls -> ('a,'b) eq option) -> eqtest

let rec eq_ty : type a b c d. (c cls -> d cls -> (c,d) eq option) -> a typ -> b typ -> (a,b) eq option =
  fun zz t1 t2 ->
  match t1, t2 with
  | String, String -> Some Eq
  | Int, Int -> Some Eq
  | Refv x, Refv y -> begin
    match zz x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | List x, List y -> begin
    match eq_ty zz x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | Map (x,y), Map (a,b) -> begin
    match eq_ty zz x a, eq_ty zz y b with
    | Some Eq, Some Eq -> Some Eq
    | _, _ -> None
    end
  | RefMap x, RefMap a -> begin
    match eq_ty zz x a with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | Record x, Record y -> None
  | Option x, Option y -> begin
    match eq_ty zz x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
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
  | Dummy -> Rpc.String "dummy"
  | String -> Rpc.String x
  | Int -> Rpc.Int x
  | Refv cls -> (match x with Ref (r,_) -> Rpc.String (List.hd (List.rev r.f_name)))
  | List ty' -> Rpc.Enum (List.map (marshal ty') x)
  | Map (ty1, ty2) -> Rpc.Dict (List.map (fun (k,v) -> ((match marshal ty1 k with | Rpc.String x -> x | _ -> failwith "Expecting stringish keys"), marshal ty2 v)) x)
  | RefMap ty ->
    let fields = RefMap.keys x in
    Rpc.Dict (List.map (function field -> (field, marshal ty (RefMap.find field x))) fields)
  | Record r ->
    let fields = r.r_fields x in
    Rpc.Dict (List.map (function B field -> (List.hd field.f_name, marshal field.f_ty (field.f_get x))) fields)
  | Option ty -> (match x with | Some x -> Rpc.Enum [marshal ty x] | None -> Rpc.Enum [])

let dump_db db = (marshal (Record DB.db_record) db)

let dump_since g db st =
  let update_acc fld acc =
    let rec update_dict d p =
      match p,d with
      | [], _ -> marshal fld.f_ty (fld.f_get db)
      | x::xs, Rpc.Dict dict ->
        let cur,others = List.partition (fun (x',_) -> x=x') dict in
        let subdict = match cur with | [] -> Rpc.Dict [] | [(_,r)] -> r | _ -> failwith "Multiple bindings" in
        let new_binding = update_dict subdict xs in
        begin
          match new_binding with
          | Rpc.Enum [y] -> Rpc.Dict ((x,y)::others)
          | Rpc.Enum [] -> Rpc.Dict others
          | e -> Rpc.Dict ((x,e)::others)
        end
      | x::xs, _ -> failwith "Can't update non-dict with sub-fields"
    in
    update_dict acc fld.f_name
  in
  StatTree.fold_over_recent g (fun fld acc -> match fld with Fld f -> update_acc f acc) (Rpc.Dict []) st

let vm1 = VM.make_ref "vm1"
let vm2 = VM.make_ref "vm2"
let vbd1 = VBD.make_ref "vbd1"
let _ =
  add vm1 VM.empty_vm;
  Printf.printf "here...\n%!";
  dump_db (getdb ());  
  add vm2 VM.empty_vm;
  Printf.printf "here2...\n%!";
  dump_db (getdb ());  
  add vbd1 VBD.empty_vbd;
  Printf.printf "here3...\n%!";
  dump_db (getdb ());  
  set vm1 VM.name_label "my new name";
  Printf.printf "here4...\n%!";
  dump_db (getdb ());  
  add_to vm2 VM.other_config "key" "value";
  dump_db (getdb ());
  set vbd1 VBD.vM vm1;
  dump_db (getdb ())

