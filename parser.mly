%{
open Ast
open Printf
open Lexing
%}

%token <int> INT
%token <string> VAR
%token PLUS TIMES LPAREN RPAREN ASSIGN SEMI EOF

%type <Ast.exp> exp 

%start exp
 
%%


exp:
  | VAR ASSIGN pexp SEMI exp { Assign($1,$3,$5) }
  | pexp                     { $1 } 

pexp:
  | texp PLUS pexp           { Plus($1, $3) }
  | texp                     { $1 }

texp:
  | aexp TIMES texp          { Times($1, $3) }
  | aexp                     { $1 } 

aexp: 
  | INT                      { Int $1 }
  | VAR                      { Var $1 }
  | LPAREN exp RPAREN        { $2 }
