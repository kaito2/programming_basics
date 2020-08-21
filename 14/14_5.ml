(* 14.1 ~ 14.4 の局所関数定義ver. *)

(* 14.1 *)
(* 目的: 整数のリストを受け取り、偶数のもののリストを返す *)
(* select_even : int list -> int list *)
let select_even = 
    let is_even n = n mod 2 = 0
    in List.filter is_even

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

(* 目的: student_t のリストを受け取り、成績がAのリストを返す *)
(* count_A : student_t list -> student_t list *)
let count_A students = 
    let is_A_rank student = student.rank = "A"
    in List.length (List.filter is_A_rank students)

(* tests *)
let s = {name="かくしん1"; score=100; rank="S"}
let a = {name="かくしん2"; score=90; rank="A"}
let f = {name="かくしん3"; score=30; rank="F"}
let t1 = count_A [] = 0
let t2 = count_A [s; a; f] = 1
let t3 = count_A [s; a; s; a; f; a] = 3

(* ======================================================= *)

(* 14.3 *)
(* 目的: 文字列のリストを受け取り、連結した文字列を返す *)
(* concat : string list -> string *)
let cocat strings = 
    let join_two_str s1 s2 = s1 ^ s2
    in List.fold_right join_two_str strings ""

(* tests *)
let t1 = cocat [] = ""
let t2 = cocat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
let t3 = cocat ["ヒル"; "クラ"; "イム"] = "ヒルクライム"

(* ======================================================= *)

(* 14.4 *)
(* 目的: student_t のリストを受け取り、全員の得点の合計を返す *)
(* student_sum : student_t list -> int *)
let student_sum students = 
    let add_student_score student sum = sum + student.score
    in List.fold_right add_student_score students 0

(* tests *)
let s100 = {name="かくしん1"; score=100; rank="S"}
let s90 = {name="かくしん2"; score=90; rank="A"}
let s30 = {name="かくしん3"; score=30; rank="F"}
let t2_1 = student_sum [] = 0
let t2_2 = student_sum [s100] = 100
let t2_3 = student_sum [s100; s90; s30] = 220

