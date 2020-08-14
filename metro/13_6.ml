#use "eki.ml"
#use "12.ml"
(* 
目的: from_st と to_st が繋がっていれば to_st を更新し、
      つながっていなければ to_st をそのまま返す。 
 *)
(* update1 : station_t -> station_t -> station_t *)
let base_ekikan_list = global_ekikan_list
let update1 from_station to_station = 
    (* MEMO: let ... in は let が1行なら末尾に in をつけて、複数行なら行の始めにつけるときれい? *)
    let rec iter ekikan_list from_st to_st = match ekikan_list with
          [] -> to_st
        | ({kiten=f_kiten; shuten=f_shuten; kyori=f_kyori} as first) :: rest -> 
            let {name=to_name; shortest_distance_meter=to_sdm} = to_st in
            let {path=from_path} = from_st in
                (* Don't use . accesser !!! *)
                if from_st.name = f_kiten && to_st.name = f_shuten
                    then {name=to_name; shortest_distance_meter=f_kyori; path=(to_name :: from_path)}
                    else iter rest from_st to_st
    in iter base_ekikan_list from_station to_station

(* tests *)
let small_ekikan_list = [ 
    {kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2}; 
    {kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
]
let yoyogiuehara = {name="代々木上原"; shortest_distance_meter=0.; path=["代々木上原"]}
let yoyogikouen = {name="代々木公園"; shortest_distance_meter=infinity; path=[]}
let meijijinguumae = {name="明治神宮前"; shortest_distance_meter=infinity; path=[]}

let base_ekikan_list = small_ekikan_list
(* REVIEW: path に "代々木公園" を含めるか確認 *)
(* updated *)
let t1 = update1 yoyogiuehara yoyogikouen 
    = {name="代々木公園"; shortest_distance_meter=1.0; path=["代々木公園"; "代々木上原"]}
(* not updated *)
let t2 = update1 yoyogiuehara meijijinguumae 
    = {name="明治神宮前"; shortest_distance_meter=infinity; path=[]}
