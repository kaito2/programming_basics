(* x軸について対称な点の座標を返す *)
(* taisho_x : float * float -> float * float *)
let taisho_x coordinates = match coordinates with (x, y) -> (x, -. y)

(* tests *)
let t1 = taisho_x (0., 0.) = (0., 0.)
let t2 = taisho_x (1., 2.) = (1., -2.)
let t3 = taisho_x (2., 1.) = (2., -1.)
