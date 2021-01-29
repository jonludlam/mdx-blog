(* Miminal repro *)

type _ cls = I : int cls | S : string cls

type (_,_) eq = Eq : ('a, 'a) eq

let eq_cls : type a b. a cls -> b cls -> (a,b) eq option = fun a b ->
  match a,b with
  | I, I -> Some Eq
  | S, S -> Some Eq
  | _, _ -> None

type _ nested = Cls : 'a cls -> 'a nested
              | Other : float nested

let eq_nested : type a b. a nested -> b nested -> (a,b) eq option = fun a b ->
  match a,b with
  | Cls a, Cls b -> begin
    match eq_cls a b with
    | Some Eq -> Some Eq
    | None -> None
    end
  | Other, Other -> Some Eq
  | _, _ -> None

type wrapped_check = { test: 'a 'b. ('a cls -> 'b cls -> ('a,'b) eq option) }

let eq_notworking : wrapped_check -> c nested -> d nested -> (c,d) eq option = fun {test} c d ->
  match c,d with
  | Cls a, Cls b -> begin
    match test with
    | Check f -> begin
        match f a b with
        | Some Eq -> Some Eq
        | None -> None
      end
    end
  | Other, Other -> Some Eq
  | _, _ -> None
