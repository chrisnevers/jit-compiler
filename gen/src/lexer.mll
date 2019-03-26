{
  open Parser
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | "λ"|"\\"|"fun"  { FUN }
  | "->"|"→"|"."    { ARROW }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "+"             { ADD }
  | ['0'-'9']+ as n { NUM (int_of_string n)}
  | (['a'-'z''A'-'Z']|['0'-'9'])+ as id
                    { ID id }
  | eof             { EOF }
