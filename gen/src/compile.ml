open Lexer
open Parser
open Printf
open Ast

exception GenError of string
let gen_error msg = raise (GenError msg)

let write_to_file fn s =
  let channel = open_out fn in
  output_string channel s;
  close_out channel

let env : (string, int) Hashtbl.t = Hashtbl.create 10

module StringSet = Set.Make (String)

let pc = ref 0

let var_ctr = ref 0

let var_to_id var =
  let open Hashtbl in
  match find_opt env var with
  | Some i -> i
  | None -> incr var_ctr;
    add env var !var_ctr;
    !var_ctr

let p bytes s =
  let str = s in
  pc := !pc + bytes;
  str

let to_binary len n =
  if n = 0 then "0" else
  let rec aux acc n =
    if n = 0 then acc
    else aux (string_of_int (n land 1) :: acc) (n lsr 1)
  in
  match len with
  | 8 -> sprintf "%08d" (int_of_string (String.concat "" (aux [] n)))
  | 32 -> sprintf "%032d" (int_of_string (String.concat "" (aux [] n)))
  | 64 -> sprintf "%064d" (int_of_string (String.concat "" (aux [] n)))

let gen_op0 op =
  let line_no = !pc in
  let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000110" in
  let s2 = p 1 op in
  line_no, s1 ^ s2

let rec gen exp =
  match exp with
  | Num n ->
    let line_no = !pc in
    let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000001" in
    let s2 = p 4 (to_binary 32 n) in
    line_no, s1 ^ s2
  | Bool true ->
    let line_no = !pc in
    let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000111" in
    line_no, s1
  | Bool false ->
    let line_no = !pc in
    let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000001000" in
    line_no, s1
  | Var "read" -> gen_op0 "00110101"
  | Var v ->
    let line_no = !pc in
    let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000010" in
    let s2 = p 4 (to_binary 32 @@ var_to_id v) in
    line_no, s1 ^ s2
  | App (m, n) ->
    let ml, res1 = gen m in
    let nl, res2 = gen n in
    let line_no = !pc in
    let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000011" in
    let s2 = p 8 (to_binary 64 ml) in
    let s3 = p 8 (to_binary 64 nl) in
    line_no, res1 ^ res2 ^ s1 ^ s2 ^ s3
  | Abs (m, n) ->
    let nl, res = gen n in
    let line_no = !pc in
    let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000100" in
    let s2 = p 8 (to_binary 64 nl) in
    let s3 = p 4 (to_binary 32 @@ var_to_id m) in
    line_no, res ^ s1 ^ s2 ^ s3
  | If (c, t, e) ->
    let cl, res1 = gen c in
    let tl, res2 = gen t in
    let el, res3 = gen e in
    let line_no = !pc in
    let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000001010" in
    let s2 = p 8 (to_binary 64 cl) in
    let s3 = p 8 (to_binary 64 tl) in
    let s4 = p 8 (to_binary 64 el) in
    line_no, res1 ^ res2 ^ res3 ^ s1 ^ s2 ^ s3 ^ s4
  | Op (o, es) -> match o with
    | "-" -> begin match List.length es with
      | 1 -> gen_op1 "00110100" es
      | 2 -> gen_op2 "00110010" es
      | _ -> gen_error "- should be unary or binary op"
      end
    | "+" -> gen_op2 "00110011" es
    | "mkpair" -> gen_op2 "00110110" es
    | "fst" -> gen_op1 "00110111" es
    | "snd" -> gen_op1 "00111000" es
    | ">" -> gen_op2 "00111001" es
    | ">=" -> gen_op2 "00111010" es
    | "<" -> gen_op2 "00111011" es
    | "<=" -> gen_op2 "00111100" es
    | "=" -> gen_op2 "00111101" es
    | _ -> gen_error "unknown primitive operation"


and gen_op1 op es =
  let [l] = es in
  let ll, res1 = gen l in
  let line_no = !pc in
  let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000110" in
  let s2 = p 8 (to_binary 64 ll) in
  let s3 = p 4 (to_binary 32 1) in
  let s4 = p 1 op in
  line_no, res1 ^ s1 ^ s2 ^ s3 ^ s4

and gen_op2 op es =
  let [l; r] = es in
  let ll, res1 = gen l in
  let rl, res2 = gen r in
  let line_no = !pc in
  let s1 = p 8 "0000000000000000000000000000000000000000000000000000000000000110" in
  let s2 = p 8 (to_binary 64 ll) in
  let s3 = p 8 (to_binary 64 rl) in
  let s4 = p 4  (to_binary 32 2) in
  let s5 = p 1 op in
  line_no, res1 ^ res2 ^ s1 ^ s2 ^ s3 ^ s4 ^ s5

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let compile filename =
  let null    = p 8 "0000000000000000000000000000000000000000000000000000000000000000" in
  let str     = load_file filename in
  let buffer  = Lexing.from_string str in
  try
    let ast = main token buffer in
    let res = gen ast in
    let l, res = res in
    let out =
      (* to_binary 32 !pc ^ *)
      to_binary 32 (l) ^
      null ^ res in
    write_to_file "tmp.byte" out
  with
  | Parser.Error ->
    let position = Lexing.lexeme_start_p buffer in
    Printf.eprintf "Parse Error (Line %d : Col %d): %s\n"
      position.pos_lnum position.pos_cnum (Lexing.lexeme buffer)

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Usage: ./compile {filename}"
  else compile Sys.argv.(1)

