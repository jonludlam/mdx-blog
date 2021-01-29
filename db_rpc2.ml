(* DB13 - events *)

type vm_stat = {
  vm_domid : string;
  vM_name_label : string;
  vM_VBDs : vbd Rpc.Types.ref list;
  vM_other_config : (string * string) list;
}
and vbd = {
  vBD_VM : vm Rpc.Types.ref;
} [@@deriving rpcty]

module VM = struct
  let uuid = vm_vM_uuid
  let name_label = vm_vM_name_label
  let vBDs = vm_vM_VBDs
  let other_config = vm_vM_other_config
  let vm = typ_of_vm
  let empty = { vM_uuid=""; vM_name_label=""; vM_VBDs = []; vM_other_config=[]}
end

module VBD = struct
  let vM = vbd_vBD_VM
  let vbd = typ_of_vbd
  let empty = { vBD_VM=NullRef VM}
end

type ('a, 'b) otm_pair = (('b Rpc.Types.ref, 'a) Rpc.Types.field * ('a Rpc.Types.ref list, 'b) Rpc.Types.field)
type relation = Rel : ('a, 'b) otm_pair -> relation

let vbd_vm : (vbd, vm) otm_pair = (VBD.vM, VM.vBDs)
let rels = [ Rel vbd_vm ]

type db = {
  vms : vm Refmap.t;
  vbds : vbd Refmap.t;
} [@@deriving rpcty]

module DB = struct
  let vms = db_vms
  let vbds = db_vbds

  let empty = {
    vms=Refmap.empty;
    vbds=Refmap.empty;
  }

  let find_objs : type a. a Rpc.Types.cls -> (a Refmap.t, db) Rpc.Types.field = function
    | VM -> vms
    | VBD -> vbds
    | _ -> failwith "Invalid type"

  let db = db
end

let mkref : type a. a Rpc.Types.cls -> string -> a Rpc.Types.ref =
  fun cls v ->
    match cls with
    | VM -> Rpc.Types.make_ref VM VM.vm v
    | VBD -> Rpc.Types.make_ref VBD VBD.vbd v
    | _ -> failwith "Unknown reference type"

let maindb = ref (0L, DB.empty, Stat.StatTree.empty)
let getdb () = let (_,db,_) = !maindb in db


let operate : 'a Rpc.Types.ref -> (('a Refmap.t, db) Rpc.Types.field -> ('a option, 'a Refmap.t) Rpc.Types.field -> 'b) -> 'b = fun ref f ->
  let cls = Rpc.Types.cls_of_ref ref in
  let f' = DB.find_objs cls in
  match ref with
  | Rpc.Types.Ref f'' -> f f' f''
  | Rpc.Types.NullRef _ -> failwith "Null reference"

let fld_update : ('a, db) Rpc.Types.field -> ('a -> 'a) -> unit = fun fld f ->
  let (gen,mydb,st) = !maindb in
  let gen' = Int64.add gen 1L in
  let otherfld = Stat.map_fld fld in
  maindb := (gen', fld.fset (f (fld.fget mydb)) mydb, otherfld.fset gen' st)


let add : 'a Rpc.Types.ref -> 'a -> unit = fun ref x ->
  (* Construct a 'field' type to access the object via the 'field_name' of 'ref' *)
  operate ref (fun db_field refmap_field ->
    fld_update (Rpc.Types.compose db_field refmap_field) (fun _ -> Some x))

let remove : 'a Rpc.Types.ref -> unit = fun ref ->
  operate ref (fun db_field refmap_field ->
    fld_update (Rpc.Types.compose db_field refmap_field) (fun _ -> None))

let update : 'a Rpc.Types.ref -> ('b,'a) Rpc.Types.field -> ('b -> 'b) -> unit = fun ref field f ->
  operate ref (fun tablefield objfield ->
    let fld = Rpc.Types.opt_compose (Rpc.Types.compose tablefield objfield) field in
    fld_update fld f)

let get : 'a Rpc.Types.ref -> ('b, 'a) Rpc.Types.field -> 'b = fun ref field ->
  operate ref (fun tablefield objfield ->
    let fld = Rpc.Types.(opt_compose (compose tablefield objfield) field) in
    let (_,db,_) = !maindb in fld.fget db)

let get_obj : 'a Rpc.Types.ref -> 'a option = fun ref ->
  operate ref (fun tablefield objfield -> let fld = Rpc.Types.compose tablefield objfield in let (_,db,_) = !maindb in fld.fget db)

let setrel : type a b. a Rpc.Types.ref -> (a, b) otm_pair -> b Rpc.Types.ref -> unit = fun refo (fieldo, fieldm) refm ->
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

let set : type a b. a Rpc.Types.ref -> (b,a) Rpc.Types.field -> b -> unit = fun ref field x ->
  let is_rel =
    List.fold_left (fun acc r ->
      match r with
      | Rel (f1,f2) -> begin
        match Rpc.Types.(eq_field field f1) with
        | Some Rpc.Types.Eq -> setrel ref (f1,f2) x; true
        | _ -> acc
        end) false rels
  in
  if not is_rel then update ref field (fun _ -> x)

let add_to : 'a Rpc.Types.ref -> ( ('c * 'd) list, 'a) Rpc.Types.field -> 'c -> 'd -> unit = fun ref field k v ->
  update ref field (fun x -> (k,v)::List.remove_assoc k x)



(* Let's see what neat things we can do now *)

let rec marshal : type a. a Rpc.Types.typ -> a -> Rpc.t = Rpcmarshal.marshal 

let dump_db db = (marshal (typ_of_db) db)
let dump_since gen (newgen,db,st) = (newgen,Stat.dump_since gen db st)

let compare_db db1 db2 =
  let db1' = Rpcmarshal.marshal typ_of_db db1 |> Jsonrpc.to_string in
  let db2' = Rpcmarshal.marshal typ_of_db db2 |> Jsonrpc.to_string in
  db1' = db2'

let vm1 = mkref VM "vm1"
let vm2 = mkref VM "vm2"
let vbd1 = mkref VBD "vbd1"

let unm rdb updates = match Rpcmarshal.unmarshal_partial typ_of_db rdb updates with Ok x -> x | Error (`Msg m) -> failwith (Printf.sprintf "Failed to unmarshal_partial: %s" m)

let _ =
  let gen = 0L in
  let remotedb = DB.empty in
  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unm remotedb updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);


  add vm1 VM.empty;

  Printf.printf "Here1\n%!";

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unm remotedb updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here2\n%!";

  add vm2 VM.empty;

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unm remotedb updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here3\n%!";

  add vbd1 VBD.empty;

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unm remotedb updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here4\n%!";

  set vm1 VM.name_label "my new name";

  let (gen,updates) = dump_since gen !maindb in
  Printf.printf "RPC: %s\n" (Jsonrpc.to_string updates);
  let remotedb = unm remotedb updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here5\n%!";

  add_to vm2 VM.other_config "key" "value";

  let (gen,updates) = dump_since gen !maindb in
  let remotedb = unm remotedb updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "Here6\n%!";


  set vbd1 VBD.vM vm1;

  let (gen,updates) = dump_since gen !maindb in

  Printf.printf "Rpc: %s\n%!" (Rpc.to_string updates);
  let remotedb = unm remotedb updates in 

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  
  Printf.printf "Here7\n%!";

  let (_,db,_) = !maindb in
  Printf.printf "compare: %b\n%!" (compare_db remotedb db);

  Printf.printf "local:  %s\n\n%!" (dump_db (getdb ()) |> Jsonrpc.to_string);
  Printf.printf "remote: %s\n\n%!" (dump_db remotedb |> Jsonrpc.to_string)

