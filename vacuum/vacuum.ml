(** Vacuum world definition and heuristics *)

type t = {
  blocked : bool array array;
  mutable dirt : (int * int) list;
  start : int * int;
  instance : int;
}


type node = {
  pos : int * int;
  remaining_dirt : (int * int) list;
  (* root points to self *)
  mutable parent : node;
  mutable h : float;
}


let identical a b =
  a.pos = b.pos && a.remaining_dirt = b.remaining_dirt

type action =
  | NORTH
  | SOUTH
  | EAST
  | WEST
  | SUCK


(************** FUNCTIONS ***************)

let height b =
  Array.length b.blocked.(0)

let width b =
  Array.length b.blocked


let update_parent node new_parent =
  node.parent <- new_parent


let get_parent node =
  node.parent


let make_root w =
  let rec n = {pos = w.start;
	       remaining_dirt = w.dirt;
	       parent = n;
	       h = ~-.1.0;} in
    n

let goal_p node =
  let v = node.remaining_dirt == [] in
    if v then Verb.pe Verb.always "Found goal!\n";
    v


let no_goal_p _ = false

let ndirt w = List.length w.dirt
  (** [ndirt w] gets the number of dirt pieces in a world. *)

let has_dirt inst x y = List.mem (x, y) inst.dirt


let key w =
  let init_dirt = w.dirt
  and width = Array.length (w.blocked)
  and height = Array.length (w.blocked.(0)) in
  let max_dirt = List.length init_dirt
  and max_pos = width * height in
  let (|>) = Fn.(|>) in
  let lshift = ((float max_pos)
                |> log10
		|> ceil
		|> int_of_float
		|> Math.int_exp 10)
  and ie = Math.int_exp 2 max_dirt in
    if ie = 0
    then failwith "Too many dirts for key to work!"
    else (try (* this will fail if key is too large for ints *)
	    ignore (ie * lshift + max_pos);
	    (fun n -> 		let sum = ref 0 in
	       Array.iteri (fun i a -> if a then sum := !sum +
			      Math.int_exp 2 i)
		 (Array.of_list
		    (List.map (fun a -> List.mem a n.remaining_dirt)
		       init_dirt));
	       let x,y = n.pos in
		 !sum * lshift + x + y * width)
	  with _ -> failwith "Problem too large for int key")


let print_key i =
  Verb.pe Verb.always "%i" i

let key_to_string i =
  Wrutils.str "%i" i


let rec sol_length n =
  (*
  if identical n.parent n
  then 1
  else 1 + (sol_length n.parent)*)
  ~-1


let equals (i1,(x1,y1)) (i2,(x2,y2)) =
  i1 == i2 && x1 == x2 && y1 == y2

let check_cost board =
  let rec check_cost accum path =
    match path with
      | [] -> failwith "Empty solution?"
      | [(x2,y2)] -> accum
      | (x1,y1)::(x2,y2)::rest ->
	  let cost = 1. in
	    check_cost  (accum +. cost)
	      ((x2,y2)::rest) in
    check_cost


let manhattan (x1,y1) (x2,y2) =
  float_of_int ((abs (x1 - x2)) + (abs (y1 - y2)))


type edge = {
  weight : float;
  u : int * int;
  v : int * int;
}


let spanning_tree ?(d_calc = manhattan) verts =
  (** Calculates the minimum spanning tree of the vertex list. *)
  let ln = (List.length verts) in
  let uf = Duf.make ln in
  let cost = ref 0. in
    List.iter (fun c -> Duf.add uf c) verts;
    let dummy = {weight = 0.;
		 u = -1,-1;
		 v = -1,-1;} in
    let pq = (Dpq.create_with (fun e1 e2 -> e1.weight <= e2.weight) dummy) in
      (List.iter (fun (x1,y1) ->
		    List.iter (fun (x2,y2) ->
				 if x1 != x2 || y1 != y2
				 then Dpq.insert pq
				   {weight = d_calc (x1,y1) (x2,y2);
				    u = (x1,y1);
				    v = (x2,y2)})
		      verts) verts;
       while not (Dpq.empty_p pq) do
	 let e = Dpq.extract_first pq in
	   if not (Duf.same_p uf e.u e.v) then
	     (cost := !cost +. e.weight;
	      Duf.join uf (Duf.rep uf e.u) (Duf.rep uf e.v))
       done);
      (!cost +. (float_of_int (ln - 1)))


let heavy_spanning_tree ?(d_calc = manhattan) ?(start_weight = 1.) verts =
  (** Calculates the minimum spanning tree of the vertex list. *)
  let ln = (List.length verts) in
  let uf = Duf.make ln in
  let edges = ref [] in
    List.iter (fun c -> Duf.add uf c) verts;
    let dummy = {weight = 0.;
		 u = -1,-1;
		 v = -1,-1;} in
    let pq = (Dpq.create_with (fun e1 e2 -> e1.weight <= e2.weight) dummy) in
      (List.iter (fun (x1,y1) ->
		    List.iter (fun (x2,y2) ->
				 if x1 != x2 || y1 != y2
				 then Dpq.insert pq
				   {weight = d_calc (x1,y1) (x2,y2);
				    u = (x1,y1);
				    v = (x2,y2)})
		      verts) verts;
       while not (Dpq.empty_p pq) do
	 let e = Dpq.extract_first pq in
	   if not (Duf.same_p uf e.u e.v) then
	     (edges := e.weight :: !edges;
	      Duf.join uf (Duf.rep uf e.u) (Duf.rep uf e.v))
       done);
      let rec sum hsum dsum count = function
	| [] -> (let nd = ln - 1 in
		   hsum +. (float nd), float (dsum + nd))
	| hd::tl -> (sum
		       ((hd *. count) +. hsum)
		       ((truncate hd) + dsum)
		       (count +. 1.) tl) in
	sum 0. 0 start_weight
	  (List.sort (fun e1 e2 -> Math.fcompare e2 e1) !edges)


let spanning_tree_heuristic n =
  (** Calculates the minimum spanning tree of the remaining dirt and the
      current location *)
  let verts = n.pos :: n.remaining_dirt in
    spanning_tree verts


let make_heavy_spanning_tree_heuristic ?(memo = false) w =
  (** Calculates the minimum spanning tree of the remaining dirt and the
      current location *)
  let key = key w
  and max_dirt = List.length w.dirt in
  let calculate n =
    let rdirt = n.remaining_dirt in
    let weight = max_dirt - (List.length rdirt) + 1 in
      (heavy_spanning_tree ~start_weight:(float weight)
	 (n.pos::rdirt)) in
    if memo
    then
      let memo = (Htable.create Fn.identity (=)
		    ((Math.int_exp 2 max_dirt) *
		       (Array.length w.blocked) / 5)) in
	(fun n ->
	   let k = key n in
	     try
	       Htable.find memo k
	     with Not_found ->
	       let v = calculate n in
		 Htable.add memo k v;
		 v)
    else calculate


let make_reverse_spanning_tree_heuristic board =
  (** Generates a reversed version of the spanning tree heuristic, from
      the current location backwards towards the start. *)
  let all_dirts = Wrlist.remove_duplicates (board.start::board.dirt) in
    (fun n ->
       let verts = n.pos ::(List.filter
			      (fun a -> not (List.mem a n.remaining_dirt))
			      all_dirts) in
       spanning_tree verts)


let print n =
  let x, y = n.pos in
    Verb.pr Verb.always "x=%d, y=%d\n%s\n%!" x y
      (List.fold_left (fun s (x, y) -> Wrutils.str "%s, %d,%d" s x y) ""
	 n.remaining_dirt)



let rec displacement_heuristic ?(stop=false) n =
  (** [displacement_heuristic n] computes the sum of the max x and y
      displacements. *)
  let x, y = n.pos in
  let above = ref 0
  and below = ref 0
  and left = ref 0
  and right = ref 0
  and remaining = ref 0
  in
    List.iter (fun (i, j) ->
		 let y_disp = abs (y - j)
		 and x_disp = abs (x - i)
		 in
		   incr remaining;
		   if j < y
		   then begin		(* above *)
		     if !above < y_disp then above := y_disp;
		   end else begin	(* below *)
		     if !below < y_disp then below := y_disp;
		   end;
		   if i < x
		   then begin 		(* left *)
		     if !left < x_disp then left := x_disp;
		   end else begin	(* right *)
		     if !right < x_disp then right := x_disp;
		   end)
      n.remaining_dirt;
    float (!above + !below + !left + !right + !remaining)

let rec naive_calc dirts (x,y) =
  match dirts with
      [] -> 0
    | (dx,dy)::tl -> (abs (x - dx)) + (abs (y - dy)) + (naive_calc tl (dx,dy))


let naive_heuristic n =
  float_of_int (naive_calc n.remaining_dirt n.pos)

let reverse_naive board =
  let all_dirts = Wrlist.remove_duplicates (board.start::board.dirt) in
    (fun n ->
       let verts =(List.filter
		     (fun a -> not (List.mem a n.remaining_dirt))
		     all_dirts) in
       float_of_int (naive_calc verts n.pos))


let greedy_heuristic n =
  let nearest (x,y) xy_list =
    List.fold_left
      (fun (accum_d,accum_loc) (ex,ey) ->
	 let d = (abs (x - ex)) + (abs (y - ey)) in
	   if d < accum_d then (d,(ex,ey))
	   else (accum_d,accum_loc)) (max_int, (-1,-1)) xy_list in

  let rec f dirts (x,y) =
    match dirts with
      | [] -> 0
      | _ -> (let distance,(nx,ny) = nearest (x,y) dirts in
		distance + (f (List.filter (fun (px,py) ->
					      not ((px = nx) && (py = ny)))
				 dirts) (nx,ny))) in
    float_of_int (f n.remaining_dirt n.pos)


let make_chris_heuristic d =
  let dirts = List.sort compare d in
  let max_size = truncate (2. ** (float_of_int (List.length dirts))) in
  let cached_values = Array.create max_size ~-1 in
  let dirt_index n =
    let sum = ref 0 in
      Array.iteri (fun i a -> if a then sum := !sum +
		     int_of_float (2.**(float_of_int i)))
	(Array.of_list
	   (List.map (fun a -> List.mem a n.remaining_dirt) dirts));
      !sum in
    assert (max_size > 0); (* truncate will return 0 on overflow *)
    (fun n ->
       if n.remaining_dirt = [] then 0.
       else
	 (let ind = dirt_index n
	  and nx,ny = n.pos in
	    if cached_values.(ind) < 0
	    then cached_values.(ind) <- int_of_float
	      (spanning_tree n.remaining_dirt);
	    let to_pile =
	      List.fold_left min max_int
		(List.map (fun (dx,dy) -> (abs (dx - nx)) +
			     (abs (dy - ny))) n.remaining_dirt) in
	      float_of_int (to_pile + cached_values.(ind))))



let is_passable w x y =
  (** [is_passable x y] is true if the location [x],[y] is passable.
      If it is off the board or blocked then it is not passable. *)
  let height = height w
  and width = width w in
    x >= 0 && x < width && y >= 0 && y < height && (not w.blocked.(x).(y))


(* This expand function does not generate the parent,
   And does not generate vacuum moves when vacuuming is not appropriate*)

let make_expand_func ?(wparent = false) ?(pathmax=false)
    ?(costfn = (fun _ -> 0.)) w =
  let width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pos
       and px, py = n.parent.pos in
       let cost = 1. +. (costfn n) (* all actions cost unit here*)
       and children = ref [] in
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if ((((x != px) || (y != py)) || wparent) &&
			      (x >= 0) && (x < width) &&
			      (y >= 0) && (y < height) &&
			      (* only check after ensuring legality! *)
			      (not w.blocked.(x).(y))) then
			  let g' = g +. cost in
			  let n' = { pos = (x,y);
				     remaining_dirt = n.remaining_dirt;
				     parent = n;
				     h = ~-.1.0;
				   }
			  in
			    if pathmax
			    then begin
			      let h' = spanning_tree_heuristic n' in
				n'.h <- Math.fmax h' (n.h -. cost);
			    end;
			    Wrutils.push (n', g') children)
	   [(0,1);(0,-1);(1,0);(-1,0)];
	 if List.mem n.pos n.remaining_dirt
	 then begin
	   let g' = g +. cost in
	   let n' = {pos = n.pos;
		     remaining_dirt = (List.filter
					 (fun (x,y) -> x !=  nx || y != ny)
					 n.remaining_dirt);
		     parent = n;
		     h = ~-.1.0;}
	   in
	     if pathmax
	     then begin
	       let h' = spanning_tree_heuristic n' in
		 n'.h <- Math.fmax h' (n.h -. cost);
	     end;
	     (n', g') :: !children
	 end else !children)


let make_heavy_vac_costfn w =
  let max_dirts = List.length w.dirt in
    (fun n -> float (max_dirts - (List.length n.remaining_dirt)))


(************************************************************)
(* Possible node types.                                     *)
(************************************************************)

(*
let node_type w n = 0 and type_max w = 0
(* [node_type w n] all nodes have the same type. *)
*)

(*
let node_type w n =
  (** [node_type w n] gets a 'type' representation for a node which is
      the number of passable cells around it. *)
  let x, y = n.pos in
    (List.fold_left (fun sum (x, y) -> if is_passable w x y then 1 else 0)
       0 [(x, y + 1); (x, y - 1); (x + 1, y); (x - 1, y)])
    + (List.length n.remaining_dirt)
and type_max w = 4 + (ndirt w)
*)

let node_type w n =
  (** [node_type w n] two types, on dirt and not on dirt. *)
  let x, y = n.pos in
    if (List.exists (fun (i, j) -> i = x && j = y) n.remaining_dirt)
    then 1
    else 0
and type_max w = 1




(* EOF *)

