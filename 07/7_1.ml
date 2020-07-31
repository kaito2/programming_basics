let sum ja ma en sc so = ja +. ma +. en +. sc +. so
let average ja ma en sc so = sum ja ma en sc so /. 5.

(* 国語, 数学, 英語, 理科, 社会, の5教科から合計点・平均点を算出する *)
(* goukei_to_heikin : float -> float -> float -> float -> float -> float * float *)
let goukei_to_heikin ja ma en sc so = (
    sum ja ma en sc so,
    average ja ma en sc so
)

(* tests *)
let t1 = goukei_to_heikin 1. 2. 3. 4. 5. = (15., 3.)
let t2 = goukei_to_heikin 1. 2. 5. 9. 10. = (27., 5.4)
let t3 = goukei_to_heikin 0. 0. 0. 0. 0. = (0., 0.)