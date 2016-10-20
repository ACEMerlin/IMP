open Ast
  
let rec eval_aexp (s,e) : int = 
  match e with
    | Int(n) -> n
    | Var(x) -> lookup s x
    | Plus(a1,a2) -> eval_aexp (s,a1) + eval_aexp (s,a2)
    | Times(a1,a2) -> eval_aexp (s,a1) * eval_aexp (s,a2)

let rec eval_bexp (s,e) : bool =
  match e with
    | True -> true
    | False -> false
    | Less(a1,a2) -> eval_aexp (s,a1) < eval_aexp (s,a2)

let rec eval_com (s,e) : store =
  match e with
    | Assign(x, a1) ->
      let n1 = eval_aexp (s,a1) in
      update s x n1
    | Seq(c1,c2) ->
      let s' = eval_com (s,c1) in
      eval_com (s', c2)
    | Aexp a1 ->
      s
    | If(b1,c1,c2) ->
      if eval_bexp (s,b1) then eval_com (s,c1) else eval_com (s,c2)
    | While(b1,c1) ->
      if eval_bexp (s,b1) then eval_com (s,Seq(c1,While(b1,c1))) else s