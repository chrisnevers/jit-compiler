exception GenError of string
let gen_error msg = raise (GenError msg)

let write_to_file fn s =
  let channel = open_out fn in
  output_string channel s;
  close_out channel

type exp =
  | Var of string
  | Abs of string * exp
  | App of exp * exp
  | Num of int
  | Op  of exp * exp list

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
  | Some i -> string_of_int i
  | None -> incr var_ctr;
    add env var !var_ctr;
    string_of_int !var_ctr

let p s =
  let str = s ^ "\n" in
  incr pc;
  str

let rec gen exp =
  match exp with
  | Num n ->
    let line_no = !pc in
    let s1 = p "1" in
    let s2 = p (string_of_int n) in
    line_no, s1 ^ s2
  | Var v ->
    let line_no = !pc in
    let s1 = p "2" in
    let s2 = p (var_to_id v) in
    line_no, s1 ^ s2
  | App (m, n) ->
    let ml, res1 = gen m in
    let nl, res2 = gen n in
    let line_no = !pc in
    let s1 = p "3" in
    let s2 = p (string_of_int ml) in
    let s3 = p (string_of_int nl) in
    line_no, res1 ^ res2 ^ s1 ^ s2 ^ s3
  | Abs (m, n) ->
    let nl, res = gen n in
    let line_no = !pc in
    let s1 = p "4" in
    let s2 = p (var_to_id m) in
    let s3 = p (string_of_int nl) in
    line_no, res ^ s1 ^ s2 ^ s3
  | Op (o, es) -> match o with
    | Var "+" ->
      let [l; r] = es in
      let ll, res1 = gen l in
      let rl, res2 = gen r in
      let line_no = !pc in
      let s1 = p "5" in
      let s2 = p "300" in
      let s3 = p (string_of_int ll) in
      let s4 = p (string_of_int rl) in
      line_no, res1 ^ res2 ^ s1 ^ s2 ^ s3 ^ s4
    | _ -> gen_error "unknown primitive operation"

let add_ex = Op (Var "+", [Num 1; Num 2])
let abs_ex = Abs ("x", Var "x")
let app_ex = App (Abs ("x", Var "x"), Num 6)
let nest_app_ex = App (App (Abs ("x", Abs ("y", Var "y")), Num 4), Num 5)
let num_ex = Num 5

let _ =
  let null = p "0" in
  let res = gen add_ex in
  let l, res = res in
  let out =
    string_of_int !pc ^ "\n" ^
    string_of_int (l) ^ "\n" ^
    null ^ res in
  write_to_file "tmp.byte" out

(*
  ISWIM

  Β_v : Restricts what values you can apply to functions. You
        can only pass constants or closures to functions
    V, U :=
      | λx.m
      | b


  Δ : (o_n b_1 ... b_n) => δ (o_n, b_1, ..., b_n)
  δ : (O x B) ... -> V
      Takes op and constant and returns value

  v = Β_v ∪ Δ

  →v  : one-step with compatible closure
  →→v : transitive, reflexive closure
  =v  : symetric closure

  eval (M) :=
    | b     if M =v b
    | `fun  if M =v λx.N

  compatible closure of R  = cc (R)
  (a, b) ⋲ cc (R)   if (apiece a, apiece b) ⋲ R
  ((1 + 1) + 1, 2 + 1) ⋲ cc (Arith)   if ((1+1), 2) ⋲ Arith

  C :=
    | ?
    | λx.C
    | C N
    | M C
    | o_n M ... C M ...

  (λy.(λx. x + y)) (1 + 2)
  (λy.(λx. x + y)) ?          ? = (1 + 2)

  (1 + 2) →v 3
  and so, ≡ (λy.λx.x + y) 3

  C := □ | λx.C | C N | M C | o_n M ... C M ...

  C[M] = C where □ is filled with M

  □[M]                  = M
  (λx.c)[M]             = λx.C[M]
  (C N)[M]              = (C[M] N)
  (N C)[M]              = (N C[M])
  (o_n M... C N ...)[L] = (o_n M ... C[L] N ...)


  M →v N if ∃ C . M = C[L] ^ N = C[K] ^ L v K

  M is related to N if theres a common piece thats the same
  except one thing thats different thats related to K.

  "contexts" = C
  programs with holes



 *)
