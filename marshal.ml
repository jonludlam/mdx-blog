(* Basic marshalling / unmarshalling *)

module V = struct
  type t =
    | Int of int
    | String of string
    | List of t list

  (* Example values *)

  let val1 = Int 10
  let val2 = String "foo"
  let val3 = List [Int 10; String "foo"]
end

module T = struct
  type _ typ =
    | Int : int typ
    | String : string typ
    | List : 'a typ -> 'a list typ
end

let rec marshal : type a. a T.typ -> a -> V.t = fun t v ->
  match t with
  | T.Int -> V.Int v
  | T.String -> V.String v
  | T.List t -> V.List (List.map (marshal t) v)

let rec unmarshal : type a. a T.typ -> V.t -> a = fun t v ->
  match t, v with
  | T.Int, V.Int v -> v
  | T.String, V.String v -> v
  | T.List t, V.List vs -> List.map (unmarshal t) vs

(* Example use *)
let v1 = marshal T.Int 10
let v2 = marshal T.String "hello"
let v3 = marshal T.(List Int) [1;2;3;4]


(* Different marshaller *)
let rec jsonmarshal : type a. a T.typ -> a -> string = fun t v ->
  match t with
  | T.Int -> Printf.sprintf "%d" v
  | T.String -> Printf.sprintf "\"%s\"" v
  | T.List t -> Printf.sprintf "[%s]" (String.concat "," (List.map (jsonmarshal t) v))


