(* 目的: init からはじめて、 lst の要素を右から順に f を施しこむ *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init = match lst with
      [] -> init
    | first :: rest -> f first (fold_right f rest init)

(* ======================================================= *)

(* 14.3 *)
(* 目的: 文字列のリストを受け取り、連結した文字列を返す *)
(* helper function *)
let join_two_str s1 s2 = s1 ^ s2
(* concat : string list -> string *)
let cocat strings = fold_right join_two_str strings ""

(* tests *)
let t1 = cocat [] = ""
let t2 = cocat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
let t3 = cocat ["ヒル"; "クラ"; "イム"] = "ヒルクライム"

(* ======================================================= *)

(* copy from ../10/10_6.ml *)
(* 学生ひとり分のデータ *)
type student_t = {
    name : string;
    score : int;
    rank : string;
}

(* 14.4 *)
(* 目的: student_t のリストを受け取り、全員の得点の合計を返す *)
let add_student_score student sum = sum + student.score
(* student_sum : student_t list -> int *)
let student_sum students = fold_right add_student_score students 0

(* tests *)
let s100 = {name="かくしん1"; score=100; rank="S"}
let s90 = {name="かくしん2"; score=90; rank="A"}
let s30 = {name="かくしん3"; score=30; rank="F"}
let t2_1 = student_sum [] = 0
let t2_2 = student_sum [s100] = 100
let t2_3 = student_sum [s100; s90; s30] = 220
