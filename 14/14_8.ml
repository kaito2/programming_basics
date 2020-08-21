(* 14.8 *)
let expect63 = (fun x -> x * x - 1) 8

(* copy from ../08/8_3.ml *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

(* example *)
let kakushin = {
    name="かくしん";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
}

(* 14.9 *)
(* `.` accessor version *)
let expectKakushin = (fun person -> person.name) kakushin
(* match version *)
let expectKakushin2 = (fun person -> match person with {name=n} -> n) kakushin
(* assign version *)
let expectKakushin2 = (fun person -> let {name=n} = person in n) kakushin
