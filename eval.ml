open Ast
  
let rec eval (s,e) : store * int = 
  match e with
    | Int(n) -> 
      (s,n)
    | Var(x) -> 
      (s,lookup s x)
    | Plus(e1,e2) -> 
      let (s',n1) = eval (s,e1) in 
      let (s'',n2) = eval (s',e2) in 
      (s'', n1 + n2)
    | Times(e1,e2) -> 
      let (s',n1) = eval (s,e1) in 
      let (s'',n2) = eval (s',e2) in 
      (s'', n1 * n2)
    | Assign(x, e1, e2) -> 
      let (s',n1) = eval (s,e1) in 
      let s'' = update s' x n1 in 
      eval (s'', e2)
