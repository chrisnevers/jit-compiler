{
  open Parser
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | "λ"|"\\"|"fun"  { FUN }
  | "let"           { LET }
  | "="             { EQ }
  | "in"            { IN }
  | "if"            { IF }
  | "else"          { ELSE }
  | "true"          { BOOL true }
  | "false"         { BOOL false }
  | "->"|"→"|"."    { ARROW }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "+"             { ADD }
  | "-"             { SUB }
  | ","             { COMMA }
  | "fst"           { FST }
  | "snd"           { SND }
  | ['0'-'9']+ as n { NUM (int_of_string n)}
  | (['a'-'z''A'-'Z']|['0'-'9'])+ as id
                    { ID id }
  | eof             { EOF }
