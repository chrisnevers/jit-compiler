%{
  open Ast
%}

%token EOF
%token FUN
%token ARROW
%token LPAREN RPAREN
%token APP
%token ADD
%token SUB
%token LET EQ IN
%token <int> NUM
%token <string> ID

%nonassoc LPAREN ID NUM
%left APP
%left ADD
%nonassoc USUB

%start main

%type <Ast.m> main

%%

main:
  | e=exp EOF                       { e }

exp:
  | ID                              { Var ($1) }
  | NUM                             { Num ($1) }
  | exp ADD exp                     { Op ("+", [$1; $3]) }
  | exp SUB exp                     { Op ("-", [$1; $3]) }
  | FUN ID ARROW exp                { Abs ($2, $4) }
  | LPAREN exp RPAREN               { $2 }
  | exp exp %prec APP               { App ($1, $2) }
  | SUB exp %prec USUB              { Op ("-", [$2]) }
  | LET ID EQ exp IN exp            { App (Abs ($2, $6), $4) }
