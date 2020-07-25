(* copy from 8_3.ml *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

(* プロファイルから血液型に関するメッセージを作成する *)
(* ketsueki_hyoji : person_t -> string *)
let ketsueki_hyoji person = match person with
    { name = n; blood_type = b} ->
        n ^ " さんの血液型は " ^ b ^ " です。"

(* tests *)
let t1 = ketsueki_hyoji {
    name="かくしん";
    height=180.;
    weight=65.;
    birthday=(12, 25);
    blood_type="A";
} = "かくしん さんの血液型は A です。"
(* テストケース省略 *)