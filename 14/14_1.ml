(* ひとまず filter を作る *)
(* 目的: リストの中から p の条件を満たすもののリストを返す *)
(* filter : ('a -> bool) -> 'a list -> 'a list *)
let rec filter p lst = match lst with
      [] -> []
    | first :: rest -> 
        if p first
            then first :: filter p rest
            else filter p rest

(* tests *)
let is_positive n = n > 0
let t1 = filter is_positive [-1; 0; 1; 2] = [1; 2]

(* ======================================================= *)

(* 14.1 *)
(* 目的: 整数のリストを受け取り、偶数のもののリストを返す *)
(* even : int list -> int list *)
let is_even n = n mod 2 = 0
let select_even = filter is_even

(* learning tests *)
let lt1 = 8 mod 2 = 0

(* tests *)
let t2_1 = select_even [] = []
let t2_2 = select_even [1; 2; 3; 4; 5] = [2; 4]

(* ======================================================= *)

(* 14.2 *)
(* copy from ../10/10_6.ml *)
(* 学生ひとり分のデータ *)
type student_t = {
    name : string;
    score : int;
    rank : string;
}

let is_A_rank student = student.rank = "A"
let count_A students = List.length (filter is_A_rank students)

(* tests *)
let s = {name="かくしん1"; score=100; rank="S"}
let a = {name="かくしん2"; score=90; rank="A"}
let f = {name="かくしん3"; score=30; rank="F"}
let t1 = count_A [] = 0
let t2 = count_A [s; a; f] = 1
let t3 = count_A [s; a; s; a; f; a] = 3
