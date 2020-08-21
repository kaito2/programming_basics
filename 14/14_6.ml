(* copy from ../10/10_6.ml *)
(* 学生ひとり分のデータ *)
type student_t = {
    name : string;
    score : int;
    rank : string;
}

(* 目的: 学生リスト lst のうち成績が target_rank の人の数を返す *)
(* count : string -> student_t list -> int *)
let count target_rank lst = 
    let is_target_score student = student.rank = target_rank
    in List.length (List.filter is_target_score lst)

(* tests *)
let s = {name="かくしん1"; score=100; rank="S"}
let a = {name="かくしん2"; score=90; rank="A"}
let f = {name="かくしん3"; score=30; rank="F"}
let t1 = count "A" [] = 0
let t2 = count "A" [s; a; f] = 1
let t3 = count "A" [s; a; s; a; f; a] = 3
