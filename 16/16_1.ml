(* 
 Purpose: Takes a list of integers 
 and returns a list of sums up to the i-th element. 
 *)
(* sum_list: int list -> int list *)
let sum_list lst = 
    let rec helper lst privisional_sum = match lst with
          [] -> []
        | first :: rest -> privisional_sum + first :: helper rest (privisional_sum + first)
    in helper lst 0

(* tests *)
let t1 = sum_list [] 
    = []
let t2 = sum_list [3; 2; 1; 4] 
    = [3; 5; 6; 10]
