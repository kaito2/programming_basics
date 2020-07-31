(* 2点の中点を算出する *)
(* chuten : (float * float) * (float * float) -> float * float *)
let chuten coordinates_pair = match coordinates_pair with ((x1, y1), (x2, y2)) 
    -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)

(* tests *)
let t1 = chuten ((0., 0.), (1., 1.)) = (0.5, 0.5)
let t2 = chuten ((1., 1.), (3., 3.)) = (2., 2.)
let t3 = chuten ((-1., -1.), (3., 3.)) = (1., 1.)
