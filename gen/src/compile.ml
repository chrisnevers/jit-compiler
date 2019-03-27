open Lexer
open Parser
open Ast

exception GenError of string
let gen_error msg = raise (GenError msg)

let write_to_file fn s =
  let channel = open_out fn in
  output_string channel s;
  close_out channel

(* type exp =
  | Var of string
  | Abs of string * exp
  | App of exp * exp
  | Num of int
  | Op  of exp * exp list *)

let env : (string, int) Hashtbl.t = Hashtbl.create 10

module StringSet = Set.Make (String)

let rec free_vars exp =
  let _rec e = free_vars e in
  let open StringSet in
  match exp with
  | Var id -> singleton id
  | Abs (id, e) -> remove id (_rec e)
  | App (m, n) -> union (_rec m) (_rec n)
  | Op (o, [m]) -> _rec m
  | Num _ -> empty

let pc = ref 0

let var_ctr = ref 0

let var_to_id var =
  let open Hashtbl in
  match find_opt env var with
  | Some i -> i
  | None -> incr var_ctr;
    add env var !var_ctr;
    !var_ctr

let p s =
  let str = s ^ "\n" in
  incr pc;
  str

let to_binary n =
  if n = 0 then "0" else
  let rec aux acc n =
  if n = 0 then acc
  else aux (string_of_int (n land 1) :: acc) (n lsr 1)
  in
  String.concat "" (aux [] n)

let gen_op0 op =
  let line_no = !pc in
  let s1 = p "0110" in
  let s2 = p op in
  line_no, s1 ^ s2

let rec gen exp =
  match exp with
  | Num n ->
    let line_no = !pc in
    let s1 = p "0001" in
    let s2 = p (to_binary n) in
    line_no, s1 ^ s2
  | Var "read" -> gen_op0 "110101"
  | Var v ->
    let line_no = !pc in
    let s1 = p "0010" in
    let s2 = p (to_binary @@ var_to_id v) in
    line_no, s1 ^ s2
  | App (m, n) ->
    let ml, res1 = gen m in
    let nl, res2 = gen n in
    let line_no = !pc in
    let s1 = p "0011" in
    let s2 = p (to_binary ml) in
    let s3 = p (to_binary nl) in
    line_no, res1 ^ res2 ^ s1 ^ s2 ^ s3
  | Abs (m, n) ->
    let nl, res = gen n in
    let line_no = !pc in
    let s1 = p "0100" in
    let s2 = p (to_binary @@ var_to_id m) in
    let s3 = p (to_binary nl) in
    line_no, res ^ s1 ^ s2 ^ s3
  | Op (o, es) -> match o with
    | "-" -> begin match List.length es with
      | 1 -> gen_op1 "110100" es
      | 2 -> gen_op2 "110010" es
      | _ -> gen_error "- should be unary or binary op"
      end
    | "+" -> gen_op2 "110011" es
    | _ -> gen_error "unknown primitive operation"


and gen_op1 op es =
  let [l] = es in
  let ll, res1 = gen l in
  let line_no = !pc in
  let s1 = p "0110" in
  let s2 = p op in
  let s3 = p (to_binary ll) in
  line_no, res1 ^ s1 ^ s2 ^ s3

and gen_op2 op es =
  let [l; r] = es in
  let ll, res1 = gen l in
  let rl, res2 = gen r in
  let line_no = !pc in
  let s1 = p "0110" in
  let s2 = p op in
  let s3 = p (to_binary ll) in
  let s4 = p (to_binary rl) in
  line_no, res1 ^ res2 ^ s1 ^ s2 ^ s3 ^ s4

let add_ex = Op ("+", [Num 1; Num 2])
let add2_ex = App (Abs ("x", Op ("+", [Num 1; Var "x"])), Num 5)
let abs_ex = Abs ("x", Var "x")
let app_ex = App (Abs ("x", Var "x"), Num 6)
let nest_app_ex = App (App (Abs ("x", Abs ("y", Var "y")), Num 4), Num 5)
let num_ex = Num 5

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let compile filename =
  let null    = p "0000" in
  let str     = load_file filename in
  let buffer  = Lexing.from_string str in
  try
    let ast = main token buffer in
    let res = gen ast in
    let l, res = res in
    let out =
      to_binary !pc ^ "\n" ^
      to_binary (l) ^ "\n" ^
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

