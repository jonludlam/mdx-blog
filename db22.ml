(* DB13 - events *)

(* Example usage *)
type _ cls = ..

type 'a cls += Unnamed : 'a cls

type (_,_) eq = Eq : ('a, 'a) eq

type eqcls = { eq : 'a 'b. ('a cls -> 'b cls -> ('a, 'b) eq option) }

type stringofcls = { pr : 'a. 'a cls -> string }

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
      if st.Stat.modified > g then
        match v,children with
        | Some x , [] -> List.fold_right (fun (_,x) acc -> fold_over_recent g f acc x) children (f x acc)
        | Some x , children when st.Stat.created > g -> f x acc
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

type _ cls += RefMap : 'a cls -> 'a RefMap.t cls

type _ typ =
  | String : string typ
  | Int : int64 typ
  | Refv : 'a typ -> 'a ref typ
  | List : 'a typ -> 'a list typ
  | Option : 'a typ -> 'a option typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | RefMap : 'a typ -> 'a RefMap.t typ
  | Record : 'a record -> 'a typ

and 's record = {
  r_fields : 's -> 's boxed_field list;
  r_cls : 's cls;
  r_empty : 's
}

and 's boxed_field =
  B : ('s, 'f, 'k) field -> 's boxed_field

and 'a refs = 'a RefMap.t

and 'a ref = Ref : ('a RefMap.t, 'a option, [`rw]) field -> 'a ref | NullRef : 'a typ -> 'a ref

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

let rec string_of_typ : type a. stringofcls -> a typ -> string = fun {pr} t ->
  match t with
  | String -> "string"
  | Int -> "int"
  | Refv x -> Printf.sprintf "refv(%s)" (string_of_typ {pr} x)
  | List x -> Printf.sprintf "list(%s)" (string_of_typ {pr} x)
  | Option x -> Printf.sprintf "option(%s)" (string_of_typ {pr} x)
  | Map (x,y) -> Printf.sprintf "map(%s,%s)" (string_of_typ {pr} x) (string_of_typ {pr} y)
  | RefMap ty -> Printf.sprintf "refmap(%s)" (string_of_typ {pr} ty)
  | Record r -> Printf.sprintf "record(%s)" (match List.hd (r.r_fields r.r_empty) with | B fld -> pr fld.f_cls)

let eq_rw : type a b. a rw -> b rw -> (a,b) eq option =
  fun r1 r2 ->
  match r1, r2 with
  | RW, RW -> Some Eq
  | RO, RO -> Some Eq
  | _, _ -> None

let make_ref cls ty name = Ref ({
  f_get = (fun r -> try Some (RefMap.find name r) with Not_found -> Printf.printf "No ref for %s (refs are: [%s])\n%!" name (String.concat ";" (RefMap.keys r)); None);
  f_set = (fun v r -> match v with | Some v -> RefMap.update name v r | None -> RefMap.remove name r);
  f_name = [name];
  f_rw = RW;
  f_cls = RefMap cls;
  f_ty = Option ty;
})

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

let name_of_ref = function | Ref x -> x.f_name | NullRef _ -> ["OpaqueRef:NULL"]


let rec eq_ty : type a b. eqcls -> a typ -> b typ -> (a,b) eq option =
  fun {eq} t1 t2 ->
  match t1, t2 with
  | String, String -> Some Eq
  | Int, Int -> Some Eq
  | Refv x, Refv y -> begin
    match eq_ty {eq} x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | List x, List y -> begin
    match eq_ty {eq} x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | Map (x,y), Map (a,b) -> begin
    match eq_ty {eq} x a, eq_ty {eq} y b with
    | Some Eq, Some Eq -> Some Eq
    | _, _ -> None
    end
  | RefMap x, RefMap a -> begin
    match eq_ty {eq} x a with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | Record x, Record y -> None
  | Option x, Option y -> begin
    match eq_ty {eq} x y with
    | Some Eq -> Some Eq
    | _ -> None
    end
  | _, _ -> None

let eq_field : type a b c d e f. eqcls -> (a,b,c) field -> (d,e,f) field -> ((a * b * c),(d * e * f)) eq option =
  fun {eq} f1 f2 ->
    match eq f1.f_cls f2.f_cls, eq_ty {eq} f1.f_ty f2.f_ty, eq_rw f1.f_rw f2.f_rw with
    | Some Eq, Some Eq, Some Eq -> Some Eq
    | _, _, _ -> None



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
and db = {
  vms : vm RefMap.t;
  vbds : vbd RefMap.t;
}

type _ cls += VM : vm cls
type _ cls += VBD : vbd cls
type _ cls += DB : db cls

module All = struct 
  let rec vm_uuid =
    field
    ~name:"uuid"
    ~rw:RO
    ~cls:VM
    ~ty:String
    ~fget:(fun vm -> vm.vM_uuid)
    ~fset:(fun x vm -> {vm with vM_uuid=x})

  and vm_name_label =
    field
      ~name:"name_label"
      ~cls:VM
      ~rw:RW
      ~ty:String
      ~fget:(fun vm -> vm.vM_name_label)
      ~fset:(fun x vm -> {vm with vM_name_label=x})

  and vm_vBDs = {f_name=["VBDs"]; f_cls=VM; f_rw=RO; f_ty=(List (Refv vbd)); f_get=(fun vm -> vm.vM_VBDs); f_set=(fun x vm -> {vm with vM_VBDs=x})}

  and vm_other_config =
    field
      ~name:"other_config"
      ~rw:RW
      ~cls:VM
      ~ty:(Map (String, String))
      ~fget:(fun vm -> vm.vM_other_config)
      ~fset:(fun x vm -> {vm with vM_other_config=x})

  and vm_empty = {vM_uuid=""; vM_name_label=""; vM_VBDs=[]; vM_other_config=[]}
  and vm_record = let fields = [B vm_uuid; B vm_name_label; B vm_vBDs; B vm_other_config] in {r_fields = (fun _ -> fields); r_empty = vm_empty; r_cls=VM}
  and vm = Record vm_record

  and vBD_vM = {f_name=["vM"]; f_cls=VBD; f_rw=RW; f_ty=(Refv vm); f_get=(fun vbd -> vbd.vBD_VM); f_set=(fun x vbd -> {vBD_VM=x})}

  and vbd_empty = {vBD_VM=(NullRef vm)}
  and vbd_record = let fields = [B vBD_vM] in {r_fields = (fun _ -> fields); r_empty = vbd_empty; r_cls=VBD}
  and vbd = Record vbd_record
  and vbd_make_ref x = make_ref VBD vbd x

end

module VM = struct
  let uuid = All.vm_uuid
  let name_label = All.vm_name_label
  let vBDs = All.vm_vBDs
  let other_config = All.vm_other_config
  let empty = All.vm_empty
  let vm_record = All.vm_record
  let vm = All.vm
  let vm_make_ref x = make_ref VM vm x
end

module VBD = struct
  let vM = All.vBD_vM
  let empty = All.vbd_empty
  let vbd_record = All.vbd_record
  let vbd = All.vbd
  let vbd_make_ref x = make_ref VBD vbd x
end

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

  let db_record = let fields = [ B vms; B vbds ] in {r_fields = (fun _ -> fields); r_empty = {vms=RefMap.empty; vbds=RefMap.empty}; r_cls=DB}
end

let mkref : type a. a cls -> string -> a ref =
  fun cls v ->
    match cls with
    | VM -> make_ref VM VM.vm v
    | VBD -> make_ref VBD VBD.vbd v
    | _ -> failwith "Unknown reference type"

let maindb = ref (0L, DB.empty, StatTree.empty)
let getdb () = let (_,db,_) = !maindb in db


let typ_of_ref = function | Ref x -> (match x.f_ty with Option x -> x | _ -> failwith "This shouldn't happen by construction") | NullRef x -> x

let cls_of_typ = function | Record r -> Some r.r_cls | _ -> None

let cls_of_ref r = r |> typ_of_ref |> cls_of_typ

let operate : 'a ref -> ((db, 'a RefMap.t, _) field -> ('a RefMap.t, 'a option, _) field -> 'b) -> 'b = fun ref f ->
  match cls_of_ref ref with
  | Some cls -> begin
    let f' = DB.find_objs cls in
    match ref with
    | Ref f'' -> f f' f''
    | NullRef _ -> failwith "Null reference"
    end
  | None -> 
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

let get_obj : 'a ref -> 'a option = fun ref ->
  operate ref (fun tablefield objfield -> let fld = compose tablefield objfield in let (_,db,_) = !maindb in fld.f_get db)

(*
  | String : string typ
  | Int : int64 typ
  | Refv : 'a cls -> 'a Ref.t typ
  | List : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | RefMap : 'a cls * 'b typ -> ('a, 'b) RefMap.t record typ
  | Record : 'a record -> 'a record typ
*)

let eq_cls : type a b. a cls -> b cls -> (a,b) eq option =
  fun c1 c2 ->
  match c1, c2 with
  | VM, VM -> Some Eq
  | VBD, VBD -> Some Eq
  | _, _ -> None

let pr_cls : type a. a cls -> string = fun x ->
  match x with
  | VM -> "VM"
  | VBD -> "VBD"
  | DB -> "DB"
  | _ -> "Unknown"

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
        match eq_field {eq=eq_cls} field f1 with
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
  | Refv cls -> (match x with Ref r -> Rpc.String (List.hd (List.rev r.f_name)) | NullRef _ -> Rpc.String "OpaqueRef:NULL")
  | List ty' -> Rpc.Enum (List.map (marshal ty') x)
  | Map (ty1, ty2) -> Rpc.Dict (List.map (fun (k,v) -> ((match marshal ty1 k with | Rpc.String x -> x | _ -> failwith "Expecting stringish keys"), marshal ty2 v)) x)
  | RefMap ty ->
    let fields = RefMap.keys x in
    Rpc.Dict (List.map (function field -> (field, marshal ty (RefMap.find field x))) fields)
  | Record r ->
    let fields = r.r_fields x in
    Rpc.Dict (List.map (function B field -> (List.hd field.f_name, marshal field.f_ty (field.f_get x))) fields)
  | Option ty -> (match x with | Some x -> Rpc.Enum [marshal ty x] | None -> Rpc.Enum [])

let rec unmarshal : type a. a typ -> a option -> Rpc.t -> a = fun ty cur rpc ->
  match ty,rpc with
  | String, Rpc.String x -> x
  | Int, Rpc.Int i -> i
  | Refv cls, String s -> if s = "OpaqueRef:NULL" then NullRef cls else mkref cls s
  | List ty, Rpc.Enum xs -> List.map (unmarshal ty None) xs
  | Map (ty1, ty2), Rpc.Dict xs -> List.map (fun (x,y) -> (unmarshal ty1 None (Rpc.String x), unmarshal ty2 None y)) xs 
  | RefMap ty, Rpc.Dict xs ->
    let cur' = match cur with | Some c -> c | None -> RefMap.empty in
    List.fold_left
      (fun refmap (r,v) ->
        let curv = try Some (RefMap.find r refmap) with _ -> None in
        RefMap.add r (unmarshal ty curv v) refmap) cur' xs
  | Record r, Rpc.Dict xs ->
    let cur' = match cur with | Some c -> c | None -> r.r_empty in
    List.fold_left (fun x (B fld) ->
      let curv = Some (fld.f_get x) in
      match List.assoc_opt (List.hd fld.f_name) xs with
      | Some v ->
        let v' = unmarshal fld.f_ty curv v in
        fld.f_set v' x
      | None -> x) cur' (r.r_fields r.r_empty)
  | Option ty, Rpc.Enum x ->
    (match x with
    | [x'] -> Some (unmarshal ty None x')
    | _ -> None)
  | _, _ -> failwith (Printf.sprintf "error: %s" (Jsonrpc.to_string rpc))
 
let dump_db db = (marshal (Record DB.db_record) db)

let dump_since g maindb =
  let (new_gen, db, st) = maindb in 
  let update_acc : type a. (db, a, _) field -> Rpc.t -> Rpc.t = fun fld acc ->
    let rec update_dict d p =
      match p,d with
      | [], _ -> marshal fld.f_ty (fld.f_get db)
      | x::xs, Rpc.Dict dict ->
        let cur,others = List.partition (fun (x',_) -> x=x') dict in
        let subdict = match cur with | [] -> Rpc.Dict [] | [(_,r)] -> r | _ -> failwith "Multiple bindings" in
        let new_binding = update_dict subdict xs in
        begin
          match fld.f_ty, new_binding with
          | Option _, Rpc.Enum [y] -> Rpc.Dict ((x,y)::others)
          | Option _, Rpc.Enum [] -> Rpc.Dict others
          | _,e -> Rpc.Dict ((x,e)::others)
        end
      | x::xs, _ -> failwith "Can't update non-dict with sub-fields"
    in
    update_dict acc fld.f_name
  in
  (new_gen, StatTree.fold_over_recent g (fun fld acc -> match fld with Fld f -> update_acc f acc) (Rpc.Dict []) st)

let compare_db db1 db2 =
  let db = Record DB.db_record in
  let rec is_equal : type a. a typ -> a -> a -> bool = fun ty f1 f2 ->
    Printf.printf "Checking typ: %s (%s vs %s)" (string_of_typ {pr=pr_cls} ty) (Jsonrpc.to_string (marshal ty f1)) (Jsonrpc.to_string (marshal ty f2));
    let result = match ty with
    | Int -> f1 = f2
    | String -> f1 = f2
    | Refv _cls -> begin
      match f1, f2 with
      | Ref f1, Ref f2 -> f1.f_name = f2.f_name
      | NullRef _, NullRef _ -> true
      | _, _ -> false
      end
    | List ty' -> List.fold_left2 (fun acc x y -> acc && is_equal ty' x y) true f1 f2
    | Option ty' -> begin match f1, f2 with | Some x, Some y -> is_equal ty' x y | None, None -> true | _,_ -> false end
    | Map (ty1,ty2) -> List.fold_left2 (fun acc (x1,x2) (y1,y2) -> acc && (is_equal ty1 x1 y1) && (is_equal ty2 x2 y2)) true f1 f2
    | RefMap ty ->
      let keys1 = RefMap.keys f1 in
      let keys2 = RefMap.keys f2 in
      let f1_sub_f2 = List.fold_left (fun acc k -> try acc && is_equal ty (RefMap.find k f1) (RefMap.find k f2) with _ -> false) true keys2 in
      let f2_sub_f1 = List.fold_left (fun acc k -> try acc && is_equal ty (RefMap.find k f1) (RefMap.find k f2) with _ -> false) true keys1 in
      f1_sub_f2 && f2_sub_f1
    | Record r ->
      let rec inner acc f =
        match f with
        | B fld -> acc && (is_equal fld.f_ty (fld.f_get f1) (fld.f_get f2))
      in List.fold_left inner true (r.r_fields f1)
    in
    Printf.printf "result=%b\n%!" result;
    if not result then failwith "bah!";
    result
  in is_equal db db1 db2

let vm1 = mkref VM "vm1"
let vm2 = mkref VM "vm2"
let vbd1 = mkref VBD "vbd1"

let _ =
  let gen = 0L in
  let remotedb = DB.empty in
  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unmarshal (Record DB.db_record) (Some remotedb) updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);


  add vm1 VM.empty_vm;

  Printf.printf "Here1\n%!";

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unmarshal (Record DB.db_record) (Some remotedb) updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here2\n%!";

  add vm2 VM.empty_vm;

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unmarshal (Record DB.db_record) (Some remotedb) updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here3\n%!";

  add vbd1 VBD.empty_vbd;

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unmarshal (Record DB.db_record) (Some remotedb) updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here4\n%!";

  set vm1 VM.name_label "my new name";

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unmarshal (Record DB.db_record) (Some remotedb) updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here5\n%!";

  add_to vm2 VM.other_config "key" "value";

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unmarshal (Record DB.db_record) (Some remotedb) updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here6\n%!";


  set vbd1 VBD.vM vm1;

  let (gen,updates) = dump_since gen !maindb in

  Printf.printf "Rpc: %s\n%!" (Rpc.to_string updates);
  let remotedb = unmarshal (Record DB.db_record) (Some remotedb) updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  
  Printf.printf "Here7\n%!";

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "local:  %s\n\n%!" (dump_db (getdb ()) |> Jsonrpc.to_string);
  Printf.printf "remote: %s\n\n%!" (dump_db remotedb |> Jsonrpc.to_string)

