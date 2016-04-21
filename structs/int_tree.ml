(** A compact method for storing integers in a tree.  Collapses ranges
    of integers into a single node.

    I have no idea if this is a good idea.:w

    TODO:

    * Balancing?  This seems to be non-trivial.

    @author eaburns
    @since 2010-03-11
*)

type t =
  | Nil
  | Node of t * range * t


and range = int * int


let empty = Nil
  (** [empty] the empty tree. *)


let debug = false


let rec insert_into_left right ((r_min, r_max) as range) = function
    (* [insert_into_left right range left] inserts a range where the
       right end of the range definitely lies to the right or this
       entire subtree.  [right] is always passed back as the right
       child of the resulting node. *)
  | Nil -> Node (Nil, range, right)

  | Node (left, (min, max), _)
      when r_min >= min && r_min <= (max + 1) ->
      (* Falls within the left node's range (engulf the right). *)
(*
      if debug then Printf.printf "\twithin left (%d, %d) -> (%d, %d)\n%!"
	min max min r_max;
*)
      Node (left, (min, r_max), right)

  | Node (left, (min, _), _)
      when r_min < min ->
      (* Engulfs this node and everything on its right. *)
(*
      if debug then Printf.printf "\tengulf left\n%!";
*)
      insert_into_left right range left

  | Node (left, ((_, max) as lower_range), r) ->
      (* The range falls on the right of this left node. *)
      assert ((max + 1) < r_min);
(*
      if debug then Printf.printf "\tright of left\n%!";
*)
      match insert_into_left Nil range r with
	| Node (residue, new_upper_range, Nil) ->
	    Node (Node(left, lower_range, residue), new_upper_range, right)
	| _ -> failwith "insert_into_left: impossible"


let rec insert_into_right left ((r_min, r_max) as range) = function
    (* [insert_into_right left range right] inserts a range where the
       left end of the range definitely lies to the left or this
       entire subtree. [left] is always passed back as the left child
       of the resulting node. *)
  | Nil -> Node (left, range, Nil)

  | Node (_, (min, max), right)
      when r_max >= (min - 1) && r_max <= max ->
      (* Falls within this right node's range (engulf the left). *)
(*
      if debug then Printf.printf "\twithin right\n%!";
*)
      Node (left, (r_min, max), right)

  | Node (_, (_, max), right)
      when r_max > max ->
      (* Engulfs this node and everything on its left. *)
(*
      if debug then Printf.printf "\tengulf right\n%!";
*)
      insert_into_right left range right

  | Node (l, ((min, _) as upper_range), right) ->
      (* The range falls on the left of this right node. *)
      assert (r_max < (min - 1));
(*
      if debug then Printf.printf "\tleft of right\n%!";
*)
      match insert_into_right Nil range l with
	| Node (Nil, new_lower_range, residue) ->
	    Node (left, new_lower_range, Node (residue, upper_range, right))
	| _ -> failwith "insert_into_right: impossible"


let rec insert_into_tree ((r_min, r_max) as range) = function
    (* [insert_into_tree range t] inserts the given range into the
       tree.  This should maintain the property that all ranges in the
       tree are disjoint. *)
  | Nil -> Node (Nil, range, Nil)

  | (Node (_, (min, max), _) as node)
      when min <= r_min && max >= r_max ->
      (* Same node, or within a node. *)
(*
      if debug then Printf.printf "same node\n%!";
*)
      node

  | Node (left, ((min, _) as upper_range), right)
      when r_max < (min - 1) ->
      (* Left of a node *)
(*
      if debug then Printf.printf "left of node\n%!";
*)
      Node (insert_into_tree range left, upper_range, right)

  | Node (left, ((_, max) as lower_range), right)
      when r_min > (max + 1) ->
      (* Right of a node *)
(*
      if debug then Printf.printf "right of node\n%!";
*)
      Node (left, lower_range, insert_into_tree range right)

  | Node (left, (min, max), right)
      when r_max >= (max + 1) && r_min <= (min - 1) ->
      (* Engulf a node *)
(*
      if debug then Printf.printf "engulf node (%d, %d)\n%!" min max;
*)
      begin match insert_into_left right range left with
	| Nil -> failwith "impossible"
	| Node (left, r, right) -> insert_into_right left r right
      end

  | Node (left, (min, max), right)
      when r_min <= (min - 1) && r_max >= (min - 1) ->
      (* In and to the left of a node. *)
(*
      if debug then Printf.printf "into and left of node\n%!";
*)
      insert_into_left right (r_min, max) left

  | Node (left, (min, max), right) ->
      assert (r_max >= (max + 1));
      assert (r_min <= (max + 1));
      (* In and to the right of a node. *)
(*
      if debug then Printf.printf "into and right of node\n%!";
*)
      insert_into_right left (min, r_max) right


let insert_range ((r_min, r_max) as r) tree =
  (** [insert_range r] inserts a new range of values into the
      tree. *)
  if r_min > r_max
  then invalid_arg "insert: invalid range (min > max)"
  else insert_into_tree r tree


let insert v = insert_into_tree (v, v)
  (** [insert v] inserts a value into the tree. *)


let rec min_range = function
    (** [min_range t] gets the range which contains the smallest
	maximum value. *)
  | Nil -> None
  | Node (l, range, _) ->
      match min_range l with
	| None -> Some range
	| v -> v


let rec max_range = function
    (** [max_range t] gets the range which contains the largest
	minimum value. *)
  | Nil -> None
  | Node (_, range, r) ->
      match max_range r with
	| None -> Some range
	| v -> v


let rec height = function
    (** [height t] gets the height of the tree. *)
  | Nil -> 0
  | Node (l, _, r) -> (max (height l) (height r)) + 1


let rec size = function
    (** [size t] gets the number of nodes in the tree. *)
  | Nil -> 0
  | Node (l, _, r) -> 1 + (size l) + (size r)


let rec mems ((r_min, r_max) as r) = function
    (** [mem r] tests if the range [r] is a member of the set of
	ranges in the tree. *)
  | Nil -> false
  | Node (_, (min, max), _) when r_min >= min && r_max <= max -> true
  | Node (left, _, right) -> (mems r left) || (mems r right)


let mem v = mems (v, v)
  (** [mem v] tests for the membership of a single value. *)


let rec fold f t init =
  (** [fold f t init] folds over the ranges in order of the smallest
      maximum values first. *)
  match t with
    | Nil -> init
    | Node (left, range, right) -> fold f right (f range (fold f left init))


let rec iter f = function
    (** [iter f] evaluates [f] on each range in order of the smallest
	maximum values first. *)
  | Nil -> ()
  | Node (left, range, right) ->
      iter f left;
      f range;
      iter f right


let rec print ch = function
  | Nil -> Printf.printf "Nil"
  | Node (l, (min, max), r) ->
      Printf.printf "Node (";
      print ch l;
      Printf.printf ", (%d, %d), " min max;
      print ch r;
      Printf.printf ")"


(*
(** {6 Debugging and Benchmarking} ****************************************)

let rec check = function
    (** [check t] makes sure that various properties of the tree
	hold. *)
  | Nil -> ()
  | Node (left, (min, max), right) ->
      assert (min <= max);
      begin match left with
	| Nil -> ()
	| Node (_, (_, lmax), _) -> assert (lmax < (min - 1));
      end;
      begin match right with
	| Nil -> ()
	| Node (_, (rmin, _), _) -> assert (rmin > (max + 1));
      end;
      check left;
      check right


let test () =
  (** [test ()] a small test function that inserts a bunch of random
      ranges, checking after each range is inserted. *)
  let t = ref empty in
  let range_list = ref [] in
  let max_height = ref 0 in
  let max_nodes = ref 0 in
    for n = 0 to 100000 do
      let min = Random.int 5000000 in
      let run = Random.int 100 in
      let range = min, min + run in
	t := insert_range range !t;
	range_list := range :: !range_list;
	max_height := max !max_height (height !t);
	max_nodes := max !max_nodes (size !t);
	List.iter (fun ((min, max) as r) ->
		     if not (mems r !t)
		     then begin
		       Printf.printf "[%d, %d] is not in the tree\n%!"
			 min max;
		       List.iter (fun (min, max) ->
				    Printf.printf "(%d, %d) " min max;)
			 (List.rev !range_list);
		       Printf.printf "\n";
		       print stdout !t;
		       assert false
		     end)
	  !range_list;
	check !t
    done;
    begin match min_range !t with
      | None -> ()
      | Some (min_min, min_max) ->
	  Printf.printf "The minimum range is [%d, %d]\n%!" min_min min_max;
    end;
    begin match max_range !t with
      | None -> ()
      | Some (max_min, max_max) ->
	  Printf.printf "The maximum range is [%d, %d]\n%!" max_min max_max
    end;
    Printf.printf "The maximum height of the tree was %d\n%!" !max_height;
    Printf.printf "The height of the final tree is %d\n%!" (height !t);
    Printf.printf "The maximum number of nodes was %d\n%!" !max_nodes;
    Printf.printf "The final number of nodes is %d\n%!" (size !t)


let benchmark () =
  (** [benchmark ()] a small test function that inserts a bunch of random
      ranges, checking after each range is inserted. *)
  let t = ref empty in
    for n = 0 to 100000 do
      let min = Random.int 5000000 in
      let run = Random.int 10 in
      let range = min, min + run in
	t := insert_range range !t;
    done

let with_time f =
  let start = Sys.time () in
  let result = f () in
  let finish = Sys.time () in
    result, (finish -. start)


let main () =
  let n = 100 in
  let times = Array.create n 0. in
    for i = 0 to n - 1 do
      let _, time = with_time benchmark in
	times.(i) <- time;
	Printf.printf "%d: %.2f\n%!" i time;
    done;
    let sum = Array.fold_left (+.) 0. times in
      Printf.printf "Mean time over %d runs is %.2f\n%!" n (sum /. (float n))

let _ = main ()
*)
