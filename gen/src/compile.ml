open Lexer
open Parser
open Printf
open Binary_helper
open Ast

exception GenError of string
let gen_error msg = raise (GenError msg)

let rec output ch instrs =
  match instrs with
  | h :: t -> output_byte ch h; output ch t
  | [] -> close_out ch

let write_to_file fn s =
  let channel = open_out_bin fn in
  output channel s

(*
  Env - (exp, int)
  Int is number of object
 *)
let env : (string, int) Hashtbl.t = Hashtbl.create 10

let pc = ref 0
let var_ctr = ref 0
let instrs = ref []

let var_to_id var =
  let open Hashtbl in
  match find_opt env var with
  | Some i -> i
  | None -> incr var_ctr;
    add env var !var_ctr;
    !var_ctr

(* Set leftmost bit of bytecode to 1 to indicate its a pointer *)
let _address i = (i * 2) + 1

let p s =
  instrs := s :: !instrs;
  incr pc

(**
  Tags - 8 byte in length
*)
let byte_tag tag =
  p (match tag with
  | `Nul -> 0
  | `Num -> 1
  | `Var -> 2
  | `App -> 3
  | `Abs -> 4
  | `Op  -> 6
  | `True -> 7
  | `False -> 8
  | `If -> 10
  );
  for i = 1 to 7 do p 0 done

(**
  Ints - 4 bytes in length. Represented in Little Endian
*)
let byte_int n =
  let bits = pad 32 (to_bits n) in
  let bytes = List.rev @@ to_bytes bits 4 in
  List.iter (fun b -> p b) bytes

(**
  Pointers - 8 bytes in length. Represented in Little Endian
*)
let byte_ptr n =
  let n = 2 * n + 1 in
  let bits = pad 64 (to_bits n) in
  let bytes = List.rev @@ to_bytes bits 8 in
  List.iter (fun b -> p b) bytes

let gen_op0 op =
  let line_no = !pc in
  byte_tag `Op;
  byte_ptr 0;
  byte_int 0;
  byte_int op;
  line_no

let rec gen exp =
  match exp with
  | Num n ->
    let line_no = !pc in
    byte_tag `Num;
    byte_int n;
    line_no
  | Bool true ->
    let line_no = !pc in
    byte_tag `True;
    line_no
  | Bool false ->
    let line_no = !pc in
    byte_tag `False;
    line_no
  | Var "read" -> gen_op0 53
  | Var v ->
    let line_no = !pc in
    byte_tag `Var;
    byte_int @@ var_to_id v;
    line_no
  | App (m, n) ->
    let ml = gen m in
    let nl = gen n in
    let line_no = !pc in
    byte_tag `App;
    byte_ptr ml;
    byte_ptr nl;
    line_no
  | If (c, t, e) ->
    let cl = gen c in
    let tl = gen t in
    let el = gen e in
    let line_no = !pc in
    byte_tag `If;
    byte_ptr cl;
    byte_ptr tl;
    byte_ptr el;
    line_no
  | Abs (m, n) ->
    let nl = gen n in
    let line_no = !pc in
    byte_tag `Abs;
    byte_ptr nl;
    byte_int @@ var_to_id m;
    line_no
  | Op (o, es) -> match o with
    | "-" -> begin match List.length es with
      | 1 -> gen_op1 52 es
      | 2 -> gen_op2 50 es
      | _ -> gen_error "- should be unary or binary op"
      end
    | "+" -> gen_op2 51 es
    | "mkpair" -> gen_op2 54 es
    | "fst" -> gen_op1 55 es
    | "snd" -> gen_op1 56 es
    | ">" -> gen_op2 57 es
    | ">=" -> gen_op2 58 es
    | "<" -> gen_op2 59 es
    | "<=" -> gen_op2 60 es
    | "=" -> gen_op2 61 es
    | _ -> gen_error "unknown primitive operation"


and gen_op1 op es =
  let [l] = es in
  let ll = gen l in
  let line_no = !pc in
  byte_tag `Op;
  byte_ptr (line_no + 24);
  byte_int 1;
  byte_int op;
  byte_ptr ll;
  line_no

and gen_op2 op es =
  let [l; r] = es in
  let ll = gen l in
  let rl = gen r in
  let line_no = !pc in
  byte_tag `Op;
  byte_ptr (line_no + 24);
  byte_int 2;
  byte_int op;
  byte_ptr ll;
  byte_ptr rl;
  line_no

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let compile filename =
  byte_tag `Nul;
  let str     = load_file filename in
  let buffer  = Lexing.from_string str in
  try
    let ast = main token buffer in
    let l   = gen ast in
    let old = List.rev !instrs in
    instrs := [];
    byte_int l;
    write_to_file "tmp.byte" (List.rev !instrs @ old)
  with
  | Parser.Error ->
    let position = Lexing.lexeme_start_p buffer in
    Printf.eprintf "Parse Error (Line %d : Col %d): %s\n"
      position.pos_lnum position.pos_cnum (Lexing.lexeme buffer)

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Usage: ./compile {filename}"
  else compile Sys.argv.(1)
