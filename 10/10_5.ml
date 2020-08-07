(* 学生ひとり分のデータ *)
type student_t = {
    name : string;
    score : int;
    rank : string;
}

(* 目的: student_t のリストを受け取り、最高得点のレコードを返す *)
(* maximum_score_student : student_t list -> student_t *)
let rec maximum_score_student students = match students with
      [] -> {name="unknown"; score=min_int; rank="F"}
    | ({score=first_score} as first) :: rest -> 
        (* Extract score with match expression. *)
        if first_score > (match (maximum_score_student rest) with {score=score} -> score)
            then first
            else maximum_score_student rest

(* tests *)
(* tests *)
let s1 = {name="かくしん1"; score=100; rank="S"}
let s2 = {name="かくしん2"; score=90; rank="A"}
let s3 = {name="かくしん3"; score=30; rank="F"}
(* TODO: Raise exception. *)
let t1 = maximum_score_student [] = {name="unknown"; score=min_int; rank="F"}
let t2 = maximum_score_student [s1] = s1
let t3 = maximum_score_student [s1; s2; s3] = s1
