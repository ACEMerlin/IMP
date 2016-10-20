type var = string

type exp =
    Int of int
  | Var of var
  | Plus of exp * exp
  | Times of exp * exp
  | Assign of var * exp * exp

let rec strExp e = 
  match e with
    | Int m -> 
      string_of_int m
    | Var x -> 
      x
    | Plus(e1,e2) -> 
      "(" ^   strExp(e1) ^ "+" ^ strExp(e2) ^ ")"
    | Times(e1,e2) -> 
      "(" ^   strExp(e1) ^ "*" ^ strExp(e2) ^ ")"
    | Assign(x,e1,e2) -> 
      "(" ^ x ^ ":=" ^ strExp e1 ^ ";" ^ strExp e2 ^ ")"

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
