(* copy from ../08/8_3.ml *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

let kakushins = {
    name="かくしん1";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
} :: {
    name="かくしん2";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
} :: {
    name="かくしん3";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
} :: []