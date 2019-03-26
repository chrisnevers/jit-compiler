type m =
  | Var of string
  | Abs of string * m
  | App of m * m
  | Num of int
  | Op of string * m list

and e =
  | EMt
  | EClo of (m, m) Hashtbl.t

let rec str_m m =
  match m with
  | Var id -> id
  | Abs (id, e) -> "λ" ^ id ^ "." ^ str_m e
  | App (fn, e) -> str_m fn ^ " " ^ str_m e
  | Num i -> string_of_int i
  | Op (o, es) -> o ^ " " ^ String.concat " " (List.map str_m es)

and str_e e =
  match e with
  | EMt -> "∘"
  | EClo c -> "{ " ^ Hashtbl.fold (fun k v acc ->
      acc ^ "(" ^ str_m k ^ ", " ^ str_m v ^ ")"
    ) c "" ^ "}"

type k =
  | Mt
  | Fn of m * e * k
  | Ar of m * k
  | Pr of string * m list * m list * k

let rec str_k k =
  match k with
  | Mt -> "⊥"
  | Fn (n, e, k) -> "fn (" ^ str_m n ^ ", " ^ str_e e ^ ", " ^ str_k k ^ ")"
  | Ar (v, k) -> "ar (" ^ str_m v ^ ", " ^ str_k k ^ ")"
  | Pr (o, vs, ns, k) -> "op (" ^ o ^ ", [" ^ String.concat ", "
    (List.map str_m vs) ^ "], [" ^ String.concat ", " (List.map str_m ns) ^
    "], " ^ str_k k ^ ")"

type st =
  | ST of m * e * k

let str_st s =
  match s with
  | ST (m, e, k) -> str_m m ^ ", " ^ str_e e ^ ", " ^ str_k k
