%{
  open Ast

  let _pair l r = Op ("mkpair", [l; r])
  let rec mk_pair l r =
    match r with
    | x :: [] -> _pair l x
    | x :: tl -> _pair l (mk_pair x tl)

%}

%token EOF
%token FUN
%token ARROW
%token LPAREN RPAREN
%token APP
%token ADD
%token SUB
%token COMMA
%token FST SND
%token IF ELSE
%token LET EQ IN
%token <int> NUM
%token <bool> BOOL
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
  | FST exp                         { Op ("fst", [$2]) }
  | SND exp                         { Op ("snd", [$2]) }
  | b=BOOL                          { Bool (if b then true else false) }
  | LPAREN exp COMMA exp RPAREN     { _pair $2 $4 }
  | LPAREN exp COMMA es=separated_list(COMMA, exp) RPAREN
                                    { mk_pair $2 es }
  | exp ADD exp                     { Op ("+", [$1; $3]) }
  | exp SUB exp                     { Op ("-", [$1; $3]) }
  | FUN ID ARROW exp                { Abs ($2, $4) }
  | LPAREN exp RPAREN               { $2 }
  | exp exp %prec APP               { App ($1, $2) }
  | SUB exp %prec USUB              { Op ("-", [$2]) }
  | LET ID EQ exp IN exp            { App (Abs ($2, $6), $4) }
  | IF exp exp ELSE exp
    { App (App (App (Abs ("v", Abs ("t", Abs ("f",
      App (App (Var "v", Var "t"), Var "f")))), $2), $3), $5) }
