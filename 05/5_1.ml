(* int *)
if 2 < 1 then 3 else 4 ;;

(* Error *)
(* if "true" then 3.14 else 2.72 ;; *) 

(* bool *)
if "a" == "b" then false else true ;;

(* Error *)
(* if true < false then 1 else "2" ;; *)

(* bool *)
if not (3 == 4) then 1 < 2 else 1 > 2 ;;

(* おまけ *)
let true_is_greater_than_false = true > false 