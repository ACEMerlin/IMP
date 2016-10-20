type var = string

type aexp =
  | Int of int
  | Var of var
  | Plus of aexp * aexp
  | Times of aexp * aexp

type bexp =
  | True
  | False
  | Less of aexp * aexp

type com =
  | Seq of com * com
  | Assign of var * aexp
  | Aexp of aexp
  | If of bexp * com * com
  | While of bexp * com

let rec str_aexp e = 
  match e with
    | Int m -> 
      string_of_int m
    | Var x -> 
      x
    | Plus(a1,a2) -> 
      "(" ^ str_aexp a1 ^ "+" ^ str_aexp a2 ^ ")"
    | Times(a1,a2) -> 
      "(" ^ str_aexp a1 ^ "*" ^ str_aexp a2 ^ ")"

let rec str_bexp e =
  match e with
    | True -> "ture"
    | False -> "false"
    | Less(a1,a2) ->
      "(" ^ str_aexp a1 ^ "<" ^ str_aexp a2 ^ ")"

let rec str_com e =
  match e with
    | Assign(x,a1) -> 
      "(" ^ x ^ ":=" ^ str_aexp a1 ^ ")"
    | Seq(c1,c2) ->
      "(" ^ str_com c1 ^ ";" ^ str_com c2 ^ ")"
    | Aexp a1 ->
      str_aexp a1
    | If(b1,c1,c2) ->
      "(if" ^ str_bexp b1 ^ "then" ^ str_com c1 ^ "else" ^ str_com c2 ^ ")"
    | While(b1,c1) ->
      "(while" ^ str_bexp b1 ^ "do" ^ str_com c1 ^ ")"

module S = 
  Set.Make(struct 
    type t = var 
    let compare = Pervasives.compare 
  end)

type store = S.t * (var -> int)

let initial_store = 
  (S.empty, fun x -> failwith (x ^ " not bound"))

let lookup (d,f) x = 
  f x 

let update (d,f) x n = 
  (S.add x d,
   fun y -> if x = y then n else f y)

let strStore (d,f) = 
  S.fold 
    (fun x acc -> x ^ "=" ^ string_of_int (f x) ^ " " ^ acc)
    d
    ""