(* DB7 - Records *)

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


type _ typ =
  | String : string typ
  | Int : int typ
  | Set : 'a typ -> 'a list typ
  | Map : 'a typ * 'b typ -> ('a * 'b) list typ
  | Record : 'a record -> 'a typ

and 's record = {
  r_fields : 's boxed_field list;
  r_empty : 's;
}

and 's boxed_field =
  B : ('s, 'f) field -> 's boxed_field

and ('s, 'f) field = {
  f_name : string;
  f_ty : 'f typ;
  f_get : 's -> 'f;
  f_set : 'f -> 's -> 's;
}

exception RTTE

let rec to_value : type a. a typ -> a -> Value.t = fun typ v ->
  match typ with
  | String -> Value.String v
  | Int -> Value.Int v
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
        | B f -> (f.f_name, to_value f.f_ty (f.f_get v))::a)
      [] r.r_fields in
    Value.Dict d

let rec of_value : type a. a typ -> Value.t -> a = fun typ v ->
  match typ, v with
  | String, Value.String str -> str
  | Int, Value.Int i -> i
  | Set ty, Value.List vs -> List.map (of_value ty) vs
  | Map (ty1, ty2), Value.Map kvs ->
    List.map (fun (k,v) ->
      of_value ty1 k,
      of_value ty2 v
      ) kvs
  | Record r, Value.Dict fs ->
    let a =
      List.fold_left
        (fun a f' ->
          match f' with B f -> f.f_set (of_value f.f_ty (List.assoc f.f_name fs)) a
          )
        r.r_empty r.r_fields
    in a
  | _, _ -> raise RTTE

type example = 
  { ex_int : int;
    ex_string : string;
    ex_string_list : string list; }

let ex_int : (example, int) field =
  { f_name = "ex_int";
    f_ty = Int;
    f_get = (fun x -> x.ex_int);
    f_set = (fun x a -> {a with ex_int = x}) }

let ex_float : (example, string) field =
  { f_name = "ex_float";
    f_ty = String;
    f_get = (fun x -> x.ex_string);
    f_set = (fun x a -> {a with ex_string = x}) }

let ex_string_list : (example, string list) field =
  { f_name = "ex_string_list";
    f_ty = Set String;
    f_get = (fun x -> x.ex_string_list);
    f_set = (fun x a -> {a with ex_string_list = x}) }

let example : example record =
  { r_fields = [ B ex_int; B ex_float; B ex_string_list ];
    r_empty = { ex_int = 0; ex_string = ""; ex_string_list = [] } }


