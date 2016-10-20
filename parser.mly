%{
open Ast
open Printf
open Lexing
%}

%token <int> INT
%token <string> VAR
%token PLUS TIMES LPAREN RPAREN ASSIGN SEMI EOF
%token TRUE FALSE LESS
%token IF ELSE THEN
%token WHILE DO LB RB

%type <Ast.com> program 

%start program
 
%%

program :
  | com SEMI program             { Seq($1,$3) } 
  | com SEMI                     { $1 }
  | WHILE bexp DO LB program RB program { Seq(While($2,$5),$7) }

com :  
  | VAR ASSIGN aexp              { Assign($1,$3) }
  | IF bexp THEN com ELSE com    { If($2,$4,$6) }
  | aexp                         { Aexp $1 }

bexp :
  | TRUE                         { True }
  | FALSE                        { False }
  | aexp LESS aexp               { Less($1,$3) }

aexp :
  | aexp PLUS aexp               { Plus($1,$3) }
  | aexp TIMES aexp              { Times($1,$3) }
  | INT                          { Int $1 }
  | VAR                          { Var $1 }
  | LPAREN aexp RPAREN           { $2 }