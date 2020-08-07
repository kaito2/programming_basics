(* copy from ../08/8_3.ml *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

let rec insert_by_name insert_person sorted_persons = 
    match sorted_persons with
          [] -> [insert_person]
        | ({name=n} as first) :: rest -> 
            let {name=insert_name} = insert_person in
                if insert_name > n 
                    then first :: insert_by_name insert_person rest
                    else insert_person :: sorted_persons

(* 目的: person_t のリストを名前の辞書順でソートする *)
(* sort_by_name : person_t list -> person_t list *)
let rec sort_by_name persons = 
    match persons with
          [] -> []
        | first :: rest -> insert_by_name first (sort_by_name rest)

(* examples *)
let kakushin_a = {
    name="かくしんあ";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
}
let kakushin_ka = {
    name="かくしんか";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
}
let kakushin_sa = {
    name="かくしんさ";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
} 

(* Learning test *)
let lt1 = "かくしんあ" < "かくしんか" = true
let {name=name_lt2} = kakushin_a 
let lt3 = name_lt2 = "かくしんあ"
(* tests *)
let t1 = sort_by_name [] = []
let t2 = sort_by_name [kakushin_a] = [kakushin_a]
let t3 = sort_by_name [kakushin_ka; kakushin_sa; kakushin_a]
    = [kakushin_a; kakushin_ka; kakushin_sa]
