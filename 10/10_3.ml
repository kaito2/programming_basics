(* 学生ひとり分のデータ *)
type student_t = {
    name : string;
    score : int;
    rank : string;
}

(* 目的: 成績の降順で並んでいる生徒のリスト(sorted_students)に生徒(student)を挿入する *)
(* insert_student : student_t -> student_t list -> student_t list *)
let rec insert_student ({score=score} as student) sorted_students = 
    match sorted_students with
          [] -> [student]
        | ({ score=first_score } as first) :: rest -> 
            if first_score > score
                then first :: insert_student student rest
                else student :: sorted_students

(* tests *)
let s1 = {name="かくしん1"; score=100; rank="S"}
let s2 = {name="かくしん2"; score=90; rank="A"}
let s3 = {name="かくしん3"; score=30; rank="F"}
let t1_1 = insert_student s1 [] = [s1]
let t1_2 = insert_student s1 [s2] = [s1; s2]
let t1_3 = insert_student s2 [s1; s3] = [s1; s2; s3]

(* 目的: 学生のリストを score の昇順でソートする *)
(* student_sort : student_t list -> student_t list *)
let rec sort_students students = match students with
      [] -> []
    | first :: rest -> insert_student first (sort_students rest)

(* tests *)
let t2_1 = sort_students [] = []
let t2_2 = sort_students [s1] = [s1]
let t2_3 = sort_students [s2; s1] = [s1; s2]
let t2_4 = sort_students [s3; s2; s1] = [s1; s2; s3]
