(* copy from ../08/8_3.ml *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

(* 目的: person_t のリストの中から target_blood_type で指定した血液型の人数を返す *)
(* count_blood_type : person_t list -> string -> int *)
let rec count_blood_type persons target_blood_type = match persons with
      [] -> 0
    | {blood_type=b} :: rest -> 
        if b = target_blood_type
            then 1 + count_blood_type rest target_blood_type
            else count_blood_type rest target_blood_type


(* examples *)
let kakushin_a1 = {
    name="かくしんa1";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
}

let kakushin_a2 = {
    name="かくしんa2";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
}

let kakushin_b1 = {
    name="かくしんb1";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="B";
}

(* tests *)
let t1 = count_blood_type [] "A" = 0
let t2 = count_blood_type [kakushin_a1] "A" = 1
let t3 = count_blood_type [kakushin_a1; kakushin_a2; kakushin_b1] "A" = 2
let t4 = count_blood_type [kakushin_a1; kakushin_a2; kakushin_b1] "B" = 1
let t5 = count_blood_type [kakushin_a1; kakushin_a2; kakushin_b1] "AB" = 0
