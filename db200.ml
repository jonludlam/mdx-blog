(* DB13 - events *)

(* Example usage *)


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
  | List : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | RefMap : 'a typ -> 'a RefMap.t typ
  | Option : 'a typ -> 'a option typ
  | Record : 'a record -> 'a typ

and 's record = {
  r_fields : 's -> 's boxed_field list;
  r : 's;
}

and 's boxed_field =
  B : ('s, 'f) field -> 's boxed_field

and _ rw =
  | RO : [`ro] rw
  | RW : [`rw] rw

and ('s, 'f) field = {
  f_name : string;
  f_ty : 'f typ;
  f_get : 's -> 'f;
  f_set : 'f -> 's -> 's;
}

and 'a ref = Ref : ('a RefMap.t, 'a option) field -> 'a ref

type record2 = {
  r2i : int64;
}

let record2_r2i = {
  f_name = "r2i"; f_ty = Int; f_get = (fun x -> x.r2i); f_set = (fun i r -> {r2i = i})
}

let record2 = Record {
  r_fields = (fun _ -> [B record2_r2i]);
  r = { r2i = 0L };
}

type record1 = {
  r1s : string;
  r2s : record2 RefMap.t;
}

let record1_r1s = {
  f_name = "r1s"; f_ty = String; f_get = (fun x -> x.r1s); f_set = (fun s r -> {r with r1s = s})
}

let record1_r2s = {
  f_name = "r2"; f_ty = RefMap record2; f_get = (fun x -> x.r2s); f_set = (fun s r -> {r with r2s = s})
}

let record1 = Record {
  r_fields = (fun _ -> [B record1_r1s; B record1_r2s]);
  r = {r1s=""; r2s=RefMap.empty}
}

let make_ref ty name = {
  f_get = (fun r -> try Some (RefMap.find name r) with Not_found -> None);
  f_set = (fun v r -> match v with | Some v -> RefMap.update name v r | None -> RefMap.remove name r);
  f_name = name;
  f_ty = Option ty;
}

let compose f1 f2 =
  { f_name = (Printf.sprintf "%s.%s" f1.f_name f2.f_name);
    f_ty = f2.f_ty;
    f_get = (fun x -> f2.f_get (f1.f_get x));
    f_set = (fun x r -> f1.f_set (f2.f_set x (f1.f_get r)) r)
  }

let opt_compose : ('a, 'b option) field -> ('b, 'c) field -> ('a, 'c) field = fun f1 f2 ->
  { f_name = (Printf.sprintf "%s?.%s" f1.f_name f2.f_name);
    f_ty = f2.f_ty;
    f_get = (fun x -> match f1.f_get x with | Some y -> f2.f_get y | None -> failwith "Missing row");
    f_set = (fun x r -> match f1.f_get r with | Some y -> f1.f_set (Some (f2.f_set x y)) r | None -> r);
  }

let myr2 = {r2i = 65L}
let myr1 = {r2s=RefMap.empty; r1s="hello"}

let get r f = f.f_get r

let add : ('a, 'b RefMap.t) field -> ('b RefMap.t, 'b option) field -> 'b -> 'a -> 'a =
  fun refmap_field ref obj a ->
    let refmap = refmap_field.f_get a in
    let refmap' = RefMap.add ref.f_name obj refmap in
    refmap_field.f_set refmap' a
 
let _ =
  let ref1 = make_ref record2 "OpaqueRef:1" in
  let _ref2 = make_ref record2 "OpaqueRef:2" in

  let myr1' = add record1_r2s ref1 {r2i = 100L} myr1 in

  Printf.printf "%s\n" (get myr1' record1_r1s);
  Printf.printf "%Ld\n" (get myr1' (opt_compose (compose record1_r2s ref1) record2_r2i))
