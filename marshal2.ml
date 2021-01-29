
type _ cls =
  | VM : [`vm] cls
  | VBD : [`vbd] cls

module Ref : sig
  type 'a t = S of string
  val string_of : 'a t -> string
  val of_string : 'a cls -> string -> 'a t
  val table_of : 'a t -> string
  val pp : Format.formatter -> 'a t -> unit
end = struct
  type 'a t = S of string
  let string_of (S v) = v
  let of_string _ v = (S v)
  let table_of r = "VM"
  let pp fmt (S v) = Format.pp_print_string fmt v
end

module V = struct
  type t =
    | Int of int
    | String of string
    | List of t list
    | Struct of (string * t) list
end

type 'a strct = { s_cls : 'a cls;
                  fields : (string * V.t) list }

module T = struct
  type _ typ =
    | Int : int typ
    | String : string typ
    | Refv : 'a cls -> 'a Ref.t typ
    | List : 'a typ -> 'a list typ
    | Struct : 'a cls -> 'a strct typ

  type ('a, 'b) field = {
    name : string;
    cls : 'a cls;
    ty : 'b typ;
  }
end

let rec unmarshal : type a. a T.typ -> V.t -> a = fun t v ->
  match t, v with
  | T.Int, V.Int v -> v
  | T.String, V.String s -> s
  | T.Refv cls, V.String s -> Ref.of_string cls s
  | T.List t, V.List vs -> List.map (unmarshal t) vs
  | T.Struct s_cls, V.Struct fields -> {fields; s_cls}

let rec marshal : type a. a T.typ -> a -> V.t = fun t v ->
  match t with
  | T.Int -> V.Int v
  | T.String -> V.String v
  | T.Refv cls -> V.String (Ref.string_of v)
  | T.List t -> V.List (List.map (marshal t) v)
  | T.Struct cls -> V.Struct v.fields

exception RTTI

let get : type a b. a strct -> (a, b) T.field -> b = fun s f ->
  match f.T.ty, List.assoc_opt f.T.name s.fields  with
  | T.Int, Some (V.Int v) -> v
  | T.String, Some (V.String v) -> v
  | T.List t, Some (V.List vs) -> List.map (unmarshal t) vs
  | _ -> raise RTTI

let set : type a b. a strct -> (a, b) T.field -> b -> a strct = fun s f v ->
  {s with fields = (f.T.name, marshal f.T.ty v)::(List.filter (fun (n,_) -> n <> f.T.name) s.fields)}

(* Example *)


let field ~name ~cls ~ty = T.{name; cls; ty}
let name_label = field ~name:"name_label" ~cls:VM ~ty:String
let vBDs = field ~name:"VBDs" ~cls:VM ~ty:T.(List (Refv VBD))

let empty_vm = {s_cls=VM; fields=[]}
let empty_vbd = {s_cls=VBD; fields=[]}

let vbd1_ref = Ref.of_string VBD "ref:1"
let vbd2_ref = Ref.of_string VBD "ref:2"

let _ =
  let vm = set empty name_label "name" in
  let vm = set empty vBDs [vbd1_ref; vbd2_ref] in
  vm
