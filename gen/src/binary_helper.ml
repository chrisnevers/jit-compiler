
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let to_bits x =
  let rec aux = function
  | 0 -> []
  | n -> n mod 2 :: aux (n / 2)
  in
  List.rev (aux x)

let pad n bits =
  let rec aux = function
  | 0 -> bits
  | n -> 0 :: (aux (n -1))
  in
  let len = List.length bits in
  if n - len > 0 then aux (n - len) else bits

let to_bytes bits n =
  let lbf = List.rev bits in
  let res = ref [] in
  for i = 0 to (n - 1) do
    let sum = ref 0 in
    for j = (i * 8) to (i * 8 + 7) do
      sum := (List.nth lbf j) * (pow 2 j) + !sum
    done;
    res := !sum / (pow 2 (i*8)) :: !res;
  done;
  !res

let to_string bytes =
  List.map (fun byte -> Bytes.of_string @@ string_of_int byte) bytes
