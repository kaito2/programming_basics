(* 目的: 2つのリストの長さが等しいかを判定 *)
(* equal_length : 'a list -> 'a list -> bool *)
(* MEMO: `length` 関数を使わないという制約あり *)
(* MEMO: `let func a b = match (a, b) with ...` とすればよかったのか *)
let rec equal_length lst1 lst2 = match (lst1, lst2) with
      ([], []) -> true
    | (_ :: rest1, _ :: rest2) -> equal_length rest1 rest2
    | _ -> false

(* tests *)
let t1 = equal_length [] [] = true
let t2 = equal_length [1] [1] = true
let t3 = equal_length [1; 2] [3; 4] = true
let t4 = equal_length [1] [] = false
let t4 = equal_length [] [1] = false
let t4 = equal_length [1; 2; 3] [4; 5] = false
