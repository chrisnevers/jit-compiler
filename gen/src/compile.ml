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

let rev str =
   let rec aux idx = match idx with
     0 -> Char.escaped (str.[0])
   | _ -> (Char.escaped str.[idx]) ^ (aux (idx-1)) in
  aux ((String.length str)-1)

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
  | `Op  -> 5
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
  let s1 = byte_tag `Op in
  byte_int op;
  line_no

let rec gen exp =
  match exp with
  | Num n ->
    let line_no = !pc in
    print_endline ("Num: " ^ string_of_int line_no);
    let s1 = byte_tag `Num in
    let s2 = byte_int n in
    line_no
  | Var "read" -> gen_op0 53
  | Var v ->
    let line_no = !pc in
    let s1 = byte_tag `Var in
    let s2 = byte_int @@ var_to_id v in
    line_no
  | App (m, n) ->
    let ml = gen m in
    let nl = gen n in
    let line_no = !pc in
    let s1 = byte_tag `App in
    let s2 = byte_ptr ml in
    let s3 = byte_ptr nl in
    line_no
  | Abs (m, n) ->
    let nl = gen n in
    let line_no = !pc in
    let s1 = byte_tag `Abs in
    let s2 = byte_int @@ var_to_id m in
    let s3 = byte_ptr nl in
    line_no
  | Op (o, es) -> match o with
    | "-" -> begin match List.length es with
      | 1 -> gen_op1 52 es
      | 2 -> gen_op2 50 es
      | _ -> gen_error "- should be unary or binary op"
      end
    | "+" -> gen_op2 51 es
    | _ -> gen_error "unknown primitive operation"


and gen_op1 op es =
  let [l] = es in
  let ll = gen l in
  let line_no = !pc in
  let s1 = byte_tag `Op in
  let s2 = byte_int op in
  let s3 = byte_ptr ll in
  line_no

and gen_op2 op es =
  let [l; r] = es in
  let ll = gen l in
  let rl = gen r in
  let line_no = !pc in
  let s1 = byte_tag `Op in
  let s2 = byte_int op in
  let s3 = byte_ptr ll in
  let s4 = byte_ptr rl in
  line_no

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let compile filename =
  let null    = byte_tag `Nul  in
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
