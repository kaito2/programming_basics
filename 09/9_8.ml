(* copy from 08/8_3.ml *)
(* 人間を表す *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood_type : string;
}

(* copy from 05/5_3.ml *)
(* 目的: 誕生日(月と日)から星座を解決する *)
(* seiza : int -> int -> string *)
let seiza month day = 
  if month * 100 + day <= 119 then "山羊座"
  else if month * 100 + day <= 218 then "水瓶座"
  else if month * 100 + day <= 320 then "魚座"
  else if month * 100 + day <= 419 then "牡羊座"
  else if month * 100 + day <= 520 then "牡牛座"
  else if month * 100 + day <= 621 then "双子座"
  else if month * 100 + day <= 722 then "蟹座"
  else if month * 100 + day <= 822 then "獅子座"
  else if month * 100 + day <= 922 then "乙女座"
  else if month * 100 + day <= 1023 then "天秤座"
  else if month * 100 + day <= 1122 then "蠍座"
  else if month * 100 + day <= 1221 then "射手座"
  else "山羊座" 

(* 目的: 人間のリストを受け取り、乙女座の人間の名前のリストを返す *)
(* otomeza : person_t list -> string list *)
let rec otomeza lst = match lst with
      [] -> []
    | { name = name; birthday = (month, day) } :: rest -> 
        if seiza month day = "乙女座"
            then name :: otomeza rest
            else otomeza rest

(* examples *)
let kakushin_yagi = {
    name="かくしんyagi";
    height=180.;
    weight=65.;
    birthday=(1, 19);
    blood_type="A";
}

let kakushin_kani = {
    name="かくしんkani";
    height=180.;
    weight=65.;
    birthday=(7, 22);
    blood_type="A";
}

let kakushin_otome1 = {
    name="かくしんotome1";
    height=180.;
    weight=65.;
    birthday=(8, 23);
    blood_type="B";
}

let kakushin_otome2 = {
    name="かくしんotome2";
    height=180.;
    weight=65.;
    birthday=(9, 22);
    blood_type="B";
}

(* tests *)
let t1 = otomeza [] = []
let t2 = otomeza [kakushin_yagi; kakushin_kani] = []
let t3 = otomeza [kakushin_otome1] = ["かくしんotome1"]
let t4 = otomeza [kakushin_otome1; kakushin_otome2] = ["かくしんotome1"; "かくしんotome2"]