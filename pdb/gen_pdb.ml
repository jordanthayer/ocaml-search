(** A modified version of uniform cost search which returns a hashtbl
    representing the pattern database -- Jordan 

    This actually makes an array based pattern database. (CMW)

*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  g : float;          (* Cost of reaching a node *)
  depth : int;
}

let wrap f = (fun n -> f n.data)
let ordered_p a b = a.g <= b.g


let make_expand expand =
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       g = g;
			       depth = n.depth + 1;}) (expand n.data n.g))


let init_lists size pred key initial equals hash  =
  let openlist = Dpq.create pred (fun _ _ -> ()) 100 initial
  and cost = Array.create size ~-.1.
  and dist = Array.create size ~-1 in
    Dpq.insert openlist initial;
    let kv = key initial in
      cost.(kv) <- 0.;
      dist.(kv) <- 0;
      openlist, cost, dist


let search size expand key initial equals hash =
  let openlist, cost, dist = (init_lists size ordered_p key
				initial equals hash) in
  let consider_child n =
    let kv = key n in
      Dpq.insert openlist n;
      cost.(kv) <- n.g;
      dist.(kv) <- n.depth in

  let rec expand_best () =
    if not (Dpq.empty_p openlist) then
      (let n = Dpq.extract_first openlist in
	 let children = expand n in
	   List.iter consider_child children;
	   expand_best ())
  in
    expand_best ();
    cost,dist


let search_dups size expand key initial equals hash =
(*
  This pattern database can be used to look up both h and d, which is
  why there are both integers and floats in it.
*)
  let openlist, cost, dist = (init_lists size ordered_p key
				initial equals hash) in
  let consider_child n =
    let state = key n in
    let prev_g = cost.(state) in
      if prev_g > n.g || prev_g < 0.
      then (cost.(state) <- n.g;
	    dist.(state) <- n.depth;
	    Dpq.insert openlist n) in

  let rec expand_best () =
    if not (Dpq.empty_p openlist) then
      (let n = Dpq.extract_first openlist in
	 let children = expand n in
	   List.iter consider_child children;
	   expand_best ())
  in
    expand_best ();
    cost,dist


let no_dups size expand key initial equals hash =
  search size (make_expand expand) (wrap key)
    {data = initial; g = 0.; depth = 0;} equals hash

let dups size expand key initial equals hash =
  search_dups size (make_expand expand) (wrap key)
    {data = initial; g = 0.; depth = 0;} equals hash

(* EOF *)
