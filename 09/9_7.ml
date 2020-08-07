(* copy from 8_3.ml *)
(* 人間を表す *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

(* 目的: person_t 型のリストから血液型がA型の数を返す *)
(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A lst = match lst with
      [] -> 0
    | { blood_type = b } :: rest -> 
        if b = "A" 
            then 1 + count_ketsueki_A rest
            else count_ketsueki_A rest


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
let t1 = count_ketsueki_A [] = 0
let t2 = count_ketsueki_A [kakushin_a1] = 1
let t3 = count_ketsueki_A [kakushin_a1; kakushin_a2; kakushin_b1] = 2
