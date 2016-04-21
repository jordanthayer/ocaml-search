(* $Id: ord_opt.ml,v 1.1 2003/09/27 01:13:46 ruml Exp ruml $
   
   numerical optimization over vectors
*)


let nearby_min f a b tol max_evals =
  (** returns point and number of evals *)
  let f = Cache.memoize f 5 in
  let better_p a b =
    (f a) < (f b)
  in
    Ord_opt.nearby_min better_p a b tol max_evals


let invert f y a b tol max_evals =
  (** returns input for [f] to give [y].  starts near [a] and [b]. *)
  let f x =
    Math.square ((f x) -. y)
  in
    nearby_min f a b tol max_evals


(* EOF *)
