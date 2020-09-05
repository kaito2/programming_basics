(* Purpose:  Apply f to the elements of lst in order from the left and join them. *)
(* fold_left:  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_left f provisional_result lst = match lst with
      [] -> provisional_result
    | first :: rest -> fold_left f (f provisional_result first) rest

(* learning tests *)
let lt1 = List.fold_left (fun s1 s2 -> s1 - s2) 0 [1; 2; 3]
let lt2 = List.fold_right (fun s1 s2 -> s1 - s2) [1; 2; 3] 0

(* tests *)
let t1 = fold_left (fun s1 s2 -> s1 - s2) 0 [1; 2; 3]
    = -6