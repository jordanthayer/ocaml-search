(** A KD tree for computing nearest neighbors or bounding box
    collisions.

    @author eaburns
    @since 2010-02-11
*)

module type LocatedObject = sig
  (** An object with some notion of location in space. *)

  type t

  val k : int
    (** [k] is the number of coordinates in an object's location. *)

  val coordinate : t -> int -> float
    (** [coordinate t dim] gets the [dim]th dimensional coordinate of
	the object. *)
end

module Make(Obj : LocatedObject) = struct

  type obj = Obj.t

  type t =
    | Leaf
    | Branch of t * obj * t


  let next_dimension d = (d + 1) mod Obj.k
    (** [next_dimension d] gets the next dimension to split on. *)


  let sort_on_dim dim objs =
    (** [sort_on_dim dim objs] sorts the array of objects on the given
	dimension. *)
    let cmp a b =
      let apos = Obj.coordinate a dim
      and bpos = Obj.coordinate b dim
      in if apos < bpos then ~-1 else if bpos < apos then 1 else 0
    in Array.sort cmp objs


  let rec make_tree objs dim =
    (** [make_tree objs dim] makes a K-D tree given an array of
	objects an the splitting dimension. *)
    let n = Array.length objs in
      if n = 0
      then Leaf
      else begin
	sort_on_dim dim objs;
	let mid = n / 2 in
	let before = Array.sub objs 0 mid
	and after = Array.sub objs (mid + 1) (n - (mid + 1))
	and dim' = next_dimension dim
	in Branch (make_tree before dim', objs.(mid), make_tree after dim')
      end


  let of_array objs = make_tree objs 0
      (** [of_array objs] makes a K-D tree given an array of objects.

	  The order of [objs] is changed by this function. *)

  let of_list objs = make_tree (Array.of_list objs) 0
    (** [of_list objs] makes a K-D tree given a list of objects. *)


  let left_of cur obj dim =
    (** [left_of cur obj dim] tests if [obj] should go to the left of
	[cur] for the given dimension [dim]. *)
    (Obj.coordinate obj dim) < (Obj.coordinate cur dim)


  let insert t obj =
    (** [insert t obj] inserts an object into the tree. *)
    let rec do_insert dim = function
	| Leaf -> Branch (Leaf, obj, Leaf)
	| Branch (l, o, r) when left_of o obj dim ->
	    Branch (do_insert (next_dimension dim) l, o, r)
	| Branch (l, o, r) ->
	    Branch (l, o, do_insert (next_dimension dim) r)
    in do_insert 0 t


  let sq_dist a b =
    (** [sq_dist a b] computes the square distance between [a] and
	[b]. *)
    let sum = ref 0. in
      for i = 0 to Obj.k - 1 do
	let ca = Obj.coordinate a i
	and cb = Obj.coordinate b i in
	  sum := !sum +. ((ca -. cb) ** 2.);
      done;
      !sum


  let nearest_neighbor t obj =
    (** [nearest_neigbhor t obj] finds the nearest neigbhor to
	[obj].

	Raises Not_found if the tree was empty. *)
    let sq_dist = sq_dist obj in
    let left_of cur dim = left_of cur obj dim in
    let rec do_nn dim = function
      | Branch (Leaf, o, Leaf) -> o, sq_dist o
      | Branch (Leaf, o, (Branch(_, ro, _) as r)) when left_of o dim ->
	  let dist_o = sq_dist o and dist_ro = sq_dist ro in
	    if dist_o < dist_ro
	    then o, dist_o
	    else do_nn (next_dimension dim) r
      | Branch ((Branch(_, lo, _) as l), o, Leaf) when not (left_of o dim) ->
	  let dist_o = sq_dist o and dist_lo = sq_dist lo in
	    if dist_o < dist_lo
	    then o, dist_o
	    else do_nn (next_dimension dim) l
      | Branch (l, o, r) when left_of o dim ->
	  begin
	    let dim' = next_dimension dim in
	    let best_n, best_dist = do_nn dim' l
	    and cur_dist = sq_dist o in
	    let nn, dist =
	      if best_dist < cur_dist then best_n, best_dist else o, cur_dist
	    in
	      match r with
		| Branch (_, ro, _) when (sq_dist ro) < dist -> do_nn dim' r
		| _ -> nn, dist
	  end
      | Branch (l, o, r) ->
	  begin
	    let dim' = next_dimension dim in
	    let best_n, best_dist = do_nn dim' r
	    and cur_dist = sq_dist o in
	    let nn, dist =
	      if best_dist < cur_dist then best_n, best_dist else o, cur_dist
	    in
	      match l with
		| Branch (_, lo, _) when (sq_dist lo) < dist -> do_nn dim' l
		| _ -> nn, dist
	  end
      | Leaf -> invalid_arg "do_nn: called with a leaf"
    in
      if t = Leaf
      then raise Not_found
      else do_nn 0 t

end

(* Scratch *)
(*

module Point = struct

  type t = { x : float; y : float }

  let k = 2

  let coordinate t = function
    | 0 -> t.x
    | 1 -> t.y
    | _ -> invalid_arg "Bad coordinate"


  let dist a b =
    ((a.x -. b.x) ** 2.) +. ((a.y -. b.y) ** 2.)

end

module Pt_kdtree = Make(Point)

let rand_pts =
  Array.init 100000 (fun _ ->
		       {
			 Point.x = Random.float 100.;
			 Point.y = Random.float 100.;

		       })
;;


let t = Pt_kdtree.of_array rand_pts
;;


let nearest_scan pt =
  Array.fold_left (fun (a, da) b ->
		     let db = Point.dist pt b
		     in if da < db then a, da else b, db)
    (rand_pts.(0), Point.dist pt rand_pts.(0)) rand_pts
*)
