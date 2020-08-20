(* 1 *)
(* identifier : 'a -> 'a *)
let identifier any = any

(* 2 *)
(* return : 'a -> 'b -> 'a *)
let return a = let f x = a in f

(* 3 *)
(* discard : 'a -> 'b -> 'b *)
let discard a = let f b = b in f
(* let discard2 a = identifier *)

(* 4 *)
(* apply_to : 'a -> ('a -> 'b) -> 'b *)
let apply_to a = let f g = g a in f

(* 5 *)
(* chain_apply : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let chain_apply a_to_b b_to_c = let f x = b_to_c (a_to_b x) in f
