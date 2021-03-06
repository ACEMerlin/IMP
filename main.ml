let printf = Printf.printf

let main = 
  if Array.length Sys.argv <> 2 then
    begin 
      printf "Usage: interp <file>\n"; 
      exit 0
    end
  else
    begin
      let file = open_in (Sys.argv.(1)) in 
      let lexbuf = Lexing.from_channel file in 
      let e = 
        try Parser.program Lexer.token lexbuf
        with Parsing.Parse_error ->
          let pos = lexbuf.Lexing.lex_curr_p in
          printf "Syntax error at line %d\n"
            pos.Lexing.pos_lnum;
          exit 1 in 
      printf "Program: %s\n" (Ast.str_com e);
      printf "Evaluating the program...\n%!";
      let final_store = Eval.eval_com (Ast.initial_store,e) in 
      printf "Final store: %s\n" (Ast.strStore final_store)
    end