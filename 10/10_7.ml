(* copy from 08/8_3.ml *)
(* 人間を表す *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

(* MEMO: `目的` にはドメイン知識(守るべきインタフェース)を入れる *)
(* 目的: person_t のリストを受け取り、血液型の人数のリストを返す *)
(* count_blood_types : person_t list -> int * int * int * int *)
(* returns (A, B, O, AB) *)
let rec count_blood_types persons = match persons with
      [] -> (0, 0, 0, 0)
    | {blood_type=blood_type} :: rest -> 
        let (a, b, o, ab) = count_blood_types rest in
            if blood_type = "A" then (a + 1, b, o, ab)
            else if blood_type = "B" then (a, b + 1, o, ab)
            else if blood_type = "O" then (a, b, o + 1, ab)
            else (a, b, o, ab + 1)

(* examples *)
let kakushin_a = {
    name="かくしんA";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
}

let kakushin_b = {
    name="かくしんB";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="B";
}

let kakushin_o = {
    name="かくしんO";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="O";
}

let kakushin_ab = {
    name="かくしんAB";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="AB";
}

(* tests *)
let t1 = count_blood_types [] = (0, 0, 0, 0)
let t2 = count_blood_types [kakushin_a] = (1, 0, 0, 0)
let t3 = count_blood_types [
    kakushin_a; kakushin_a; kakushin_a; kakushin_a; 
    kakushin_b; kakushin_b; kakushin_b; 
    kakushin_o; kakushin_o; 
    kakushin_ab
] = (4, 3, 2, 1)

(* 10.8 *)
(* 目的: person_t のリストを受け取り、最も多い血液型を返す *)
(* most_blood_type : person_t list -> string *)
let most_blood_type persons = match persons with
    (* TODO: Raise exception *)
      [] -> "EMPTY"
    | _ -> let (a, b, o, ab) = count_blood_types persons in
        if a > b && a > o && a > ab then "A"
        else if b > a && b > o && b > ab then "B"
        else if o > a && o > b && o > ab then "O"
        else "AB"

(* tests *)
(* TODO: Raise exception *)
let t2_1 = most_blood_type [] = "EMPTY"
let t2_2 = most_blood_type [kakushin_a] = "A"
let t2_3 = most_blood_type [kakushin_b] = "B"
let t2_4 = most_blood_type [
    kakushin_a; kakushin_a; kakushin_a; kakushin_a; 
    kakushin_b; kakushin_b; kakushin_b; 
    kakushin_o; kakushin_o; 
    kakushin_ab
] = "A"
