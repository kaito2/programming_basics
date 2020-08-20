(* copy from ../08/8_3.ml *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

(* 目的: 受け取った関数 f を lst の各要素に適用したリストを返す *)
(* my_map : ('a -> 'b) -> 'a list -> 'b list  *)
let rec map f lst = match lst with
      [] -> []
    | first :: rest -> f first :: map f rest

(* person_names のヘルパー関数 *)
let person_name person = match person with {name=n} -> n

(* 目的: person_t のリストを受け取り、name のリストを返す *)
(* person_names : person_t list -> string list *)
let person_names persons = map person_name persons

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
let t1 = person_names [] = []
let t2 = person_names [kakushin_a1] = ["かくしんa1"]
let t3 = person_names [kakushin_a1; kakushin_a2; kakushin_b1] = ["かくしんa1"; "かくしんa2"; "かくしんb1"]
