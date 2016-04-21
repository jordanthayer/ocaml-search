(* $Id: astar.ml,v 1.4 2006/06/18 21:31:38 ruml Exp $

   Generates and reads / processes TSP instances
*)


type problem = {
  (* a[i][j] is distance from i to j *)
  dists : float array array;
  symmetric : bool;
}


(********** read tsplib format **********)

(*
 * TSPlib tags:
 *
 * Specification Part:
 *
 * NAME - name of file
 * TYPE - {TSP, ATSP}
 * COMMENT
 * DIMENSION - number of nodes
 * CAPACITY - n/a
 * EDGE_WEIGHT_TYPE - {EXPLICIT, EUC_2D, EUC_3D}
 * EDGE_WEIGHT_FORMAT - {FULL_MATRIX, UPPER_ROW}
 * EDGE_DATA_FORMAT - n/a (unless graph is not complete)
 * NODE_COORD_TYPE - {TWOD_COORDS, THREED_COORDS, NO_COORDS}
 * DISPLAY_DATA_TYPE - n/a
 * EOF - optional
 *
 *
 * Data Section:
 *
 * NODE_COORD_SECTION - <int> <real> <real>
 * EDGE_WEIGHT_SECTION
 *
 *)



let distance point1 point2 =
  let sum = ref 0. in
    List.iter2 (fun p1 p2 ->
		  sum := !sum +. (p1 -. p2) ** 2.)
      point1 point2;
    sqrt !sum


let find_dists coords dim =
  (** Returns the distance between two points in Euclidean space with a
      dimension of 1 or higher *)
  let d = Array.make_matrix dim dim infinity in
    List.iter (fun p1 ->
		 List.iter (fun p2 ->
			      if (List.hd p1) <> List.hd p2 then
				(d.(-1 + int_of_float (List.hd p1)).
				   (-1 + int_of_float (List.hd p2))
				 <- distance (List.tl p1) (List.tl p2);

				 d.(-1 + int_of_float (List.hd p2)).
				   (-1 + int_of_float (List.hd p1))
				 <- distance (List.tl p1) (List.tl p2)))
		 coords)
      coords;
    d


let read_node_coords in_chnl dim c_type =
  (** Returns a float distance matrix  *)
  let coords = ref []
  and row = ref 0 in
    while !row < dim do
      let line = input_line in_chnl in
	Wrutils.push (Wrstr.parse_floats line) coords;
	incr row
    done;
    find_dists !coords dim





let lower_diag n row col d =
  d.(!row).(!col) <- n;
  d.(!col).(!row) <- n;
  if !col = !row + 1 then
    (incr row;
     col := 0)
  else
    incr col;
  !row, !col, d




let read_lower_diag in_chnl dim =
  (** Reads edge weights in lower-triangular form *)
  let d = Array.make_matrix dim dim infinity
  and list = ref []
  and row = ref 0
  and col = ref 0 in
    while !row < dim do
      let line = input_line in_chnl in
	(*Wrutils.pr "\n\nLine: %s\n\n%!" line;
	Wrutils.pr "Row: %d\tCol: %d%!" !row !col;*)
	list := Wrstr.parse_floats line;
	List.iter ( fun n ->
		      d.(!row).(!col) <- n;
		      d.(!col).(!row) <- n;
		      if !col = !row then
			(incr row;
			 col := 0)
		      else
			incr col)
	  !list;
    done;
    d



let read_edge_weights in_chnl dim =
  (** Reads (explicit) edge weight matrix *)
  let d = Array.make_matrix dim dim infinity
  and list = ref []
  and row = ref 0
  and col = ref 0 in
    while !row < dim do
      let line = input_line in_chnl in
	list := Wrstr.parse_floats line;
	List.iter (fun n ->
		     d.(!row).(!col) <- n;
		     if !col = (dim - 1) then
		       (incr row;
			col := 0)
		     else
		       incr col)
	  !list;
    done;
    d



let print_d_matrix d =
  for row = 0 to ((Array.length d) - 1) do
    for col = 0 to ((Array.length d) - 1) do
      (Wrutils.pr "%f\t%!" d.(row).(col);
      if col = ((Array.length d) -1) then
	Wrutils.pr "\n%!")
    done
  done;
  ()


type ew_format =
    Lower_diag_row
  | Full_matrix



let read_tsplib in_chnl =
  let d = ref (Array.make_matrix 0 0 infinity)
  and symm = ref false
  and num_nodes = ref 0
  and coord_type = ref 0
  and ewf = ref Full_matrix
  and stop = ref false in
    while not(!stop = true) do
      let line = input_line in_chnl in
      let (keyword, i) = Wrstr.next_token line 0 in
	(*Wrutils.pr "Keyword = %s\n" keyword;*)
	match keyword with
	  "TYPE:" -> (let (value, j) = Wrstr.next_token line i in
			match value with
			  "TSP" -> symm := true
			| "ATSP" -> ()
			| _ -> ())
	| "DIMENSION:" -> (  let (value, j) = Wrstr.next_token line i in
			       num_nodes := int_of_string value)
	| "EDGE_WEIGHT_TYPE:" -> (let (value, j) = Wrstr.next_token line i in
				    match value with
				      "EUC_2D" -> coord_type := 2
				    | "EUC_3D" -> coord_type := 3
				    | _ -> ())
	| "EDGE_WEIGHT_FORMAT:" -> (let (value, j) = Wrstr.next_token line i in
				      match value with
					"LOWER_DIAG_ROW" ->
					  ewf := Lower_diag_row;
				      | _ -> ())
	| "NODE_COORD_SECTION" -> (stop := true;
				   d := read_node_coords in_chnl
				     !num_nodes !coord_type)
	| "EDGE_WEIGHT_SECTION" -> ( stop := true;
				     match !ewf with
				       Full_matrix -> d := read_edge_weights
					 in_chnl !num_nodes
				     | Lower_diag_row -> d := read_lower_diag
					 in_chnl !num_nodes)
	| _ -> ()
    done;
    let p = { dists = !d;
	      symmetric = !symm;
	    } in
     (* Wrutils.pr "\n\nNumber of nodes: %d\n" !num_nodes;
      Wrutils.pr "Dimension of matrix: %d by %d\n"
	(Array.length !d) (Array.length !d.(0));
      (match !symm with
	 true -> Wrutils.pr "Symmetric: true\n\n";
       | false -> Wrutils.pr "Symmetric: false\n\n");*)
     (*print_d_matrix !d;*)
      p



let prob_path = "./tsp/data/instance/pkhard/true/12"

let read_p num =
  read_tsplib
    (Unix.in_channel_of_descr
       (Unix.openfile (Wrutils.str "%s/%s" prob_path num) [] 0))

(***** testing mst code *****)

(*let debug_mst p_name =
  let prob = read_tsplib (open_in p_name) in
    Tsp.old_mst prob.dists
    (*Tsp.new_mst prob.dists*)*)


(********** Uniform in unit square **********)


let make_rand_cities size =
  let coords = ref []
  and x = ref 0.
  and y = ref 0. in
    for i = 1 to size do
      x := Random.float 1.;
      y := Random.float 1.;
      Wrutils.push [float_of_int i ; !x ; !y]  coords
    done;
    !coords


let uniform_usquare size =
  let coords = make_rand_cities size in
  let d = find_dists coords size in
  let p = { dists = d;
	    symmetric = true
	  } in
    p



(********** Pearl & Kim "hard" problems **********)

let p_and_k ?(symm = true) size =
  (** Randomly generated "hard" TSP instances where the intercity
    distances are independently chosen from a uniform distriubtion
    over the interval (0.75, 1.25). Instances are symmetric by default. *)
  let d = Array.make_matrix size size infinity in
    for row = 0 to (size-1) do
      for col = (row+1) to (size-1) do
	d.(row).(col) <- 0.75 +. (Random.float 0.5);
	if symm = true then
	  d.(col).(row) <- d.(row).(col)
	else
	  d.(col).(row) <- 0.75 +. (Random.float 0.5);
      done
    done;
    let p = {dists = d;
	     symmetric = symm
	    } in
      (*print_d_matrix d;*)
      p



(********** I/O **********)


let write_edge_weights p ch =
  for row = 0 to ((Array.length p) - 1) do
    for col = 0 to ((Array.length p) - 1) do
      (Wrutils.pf ch "%f\t%!" p.(row).(col);
       if col = ((Array.length p) -1) then
	 Wrutils.pf ch "\n%!")
    done
  done;
  ()


let write prob symm ch =
  if (symm = true) then
    Wrutils.pf ch "TYPE: TSP\n"
  else
    Wrutils.pf ch "TYPE: ATSP\n";
  Wrutils.pf ch "DIMENSION: %d\n" (Tsp.prob_size prob.dists);
  Wrutils.pf ch "EDGE_WEIGHT_SECTION\n";
  write_edge_weights prob.dists ch;
  Wrutils.pf ch "EOF"




(************************** Interfaces ****************************)

let default_interface problem lim =
(*
  let initial = Tsp.make_initial problem.dists problem.symmetric in
*)
    (Search_interface.make
       ~h:(Tsp.make_h_mst problem.dists problem.symmetric)
       ~d:(Tsp.make_d problem.dists)
       ~t:(fun _ -> 0)
       ~hd:(Tsp.make_hd problem.dists problem.symmetric)
       ~domain_expand:(Tsp.make_expand problem.dists
			 problem.symmetric)
       ~predecessor:(fun n g -> [(n.Tsp.parent, n.Tsp.parent.Tsp.cost_so_far)])
       ~key:Tsp.key
       ~key_print:Tsp.key_to_string
       ~goal_p:Tsp.goal_p
       ~halt_on:lim
       ~equals:(fun a b -> a = b)
       ~p_update:Tsp.update_parent
       ~get_sol_length:(fun _ -> Array.length problem.dists)
       ~rev_h:(Tsp.make_rev_h problem.dists problem.symmetric)
       ~rev_d:(Tsp.make_rev_d problem)
       ~rev_hd:(let h = Tsp.make_rev_h problem.dists problem.symmetric
		and d = Tsp.make_rev_d problem in (fun n -> h n, d n))
       Search_interface.Salesman
       (Tsp.make_initial problem.dists problem.symmetric)
       (fun _ _ -> false) (fun _ -> ()))


let pathmax_interface problem lim =
  let interface = default_interface problem lim in
    interface.Search_interface.initial.Tsp.h <-
      Tsp.make_h_mst problem.dists problem.symmetric
      interface.Search_interface.initial;
    { interface with
	Search_interface.h = (Tsp.make_h_mst_pathmax
				problem.dists
				problem.symmetric);
	Search_interface.hd = (fun n ->
				 Tsp.make_h_mst_pathmax
				   problem.dists
				   problem.symmetric
				   n,
				 Tsp.make_d problem.dists n);
    }



let old_interface problem lim =
  (Search_interface.make
     ~h:(Tsp.old_mst problem.dists problem.symmetric)
     ~d:(Tsp.make_d problem.dists)
     ~hd:(Tsp.make_hd_old problem.dists problem.symmetric)
     ~domain_expand:(Tsp.make_expand problem.dists
		       problem.symmetric)
     ~key:Tsp.key
     ~key_print:Tsp.key_to_string
     ~goal_p:Tsp.goal_p
     ~halt_on:lim
     ~equals:(fun a b -> a = b)
     Search_interface.Salesman
     (Tsp.make_initial problem.dists problem.symmetric)
     (fun _ _ -> false) (fun _ -> ()))

(* EOF *)
