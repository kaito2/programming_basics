(* 目的: 整数のリストから偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)
let rec even lst = match lst with
      [] -> []
    | first :: rest -> 
        if first mod 2 = 0 
            then first :: even rest
            else even rest

(* tests *)
let t1 = even [] = []
let t2 = even [1] = []
let t3 = even [2] = [2]
let t4 = even [1; 2; 3; 4; 5; 6] = [2; 4; 6]
