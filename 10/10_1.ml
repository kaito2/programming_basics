(* 10.1 *)
(* 目的: 与えられた整数を予め昇順に並んでいる整数のリストに挿入する *)
(* insert : int list -> int list *)
let rec insert num lst = match lst with
      [] -> [num]
    | first :: rest -> 
        if first < num 
            then first :: insert num rest
            else num :: lst

(* tests *)
let t1 = insert 1 [] = [1]
let t2 = insert 1 [0] = [0; 1]
let t3 = insert 5 [1; 3; 4; 7; 8] = [1; 3; 4; 5; 7; 8]

(* 10.2 *)
(* 目的: 整数のリストを昇順にソートする *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
      [] -> []
    | first :: rest -> insert first (ins_sort rest)

(* tests *)
let t2_1 = ins_sort [] = []
let t2_2 = ins_sort [1] = [1]
let t2_3 = ins_sort [3; 2; 1] = [1; 2; 3]
let t2_4 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8]
