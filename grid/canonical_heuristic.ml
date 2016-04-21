(** Implements Canonical Heuristics as presented in
    Memory based heuristics for explicit state based search *)

(* The canonical heuristic selects a set of k points spread over the grid.
   It computes perfect pair wise distances between these k points.
   Additionally, we record the distance between each point and its nearest
   canonical point.  Then, the distance between two points becomes

   h(a,b) = d(C(a), C(b)) - [d(a,C(a)) + d(b,C(b))]

*)

open Grid


let p_data_equals (key1,key2) (key3,key4) =
 equals key1 key3 && equals key2 key4


let cexpand_func ind d w root_state c_states p_data sdata1 sdata2 =
  (** A specialized domain expand function to be used when doing
      the uniform cost searches which populate the canonical data tables *)
  assert (d = 1);
  let costs = cost_array w
  and moves = (match w.moves with
		   Fourway -> four_moves
		 | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pos
       and px, py = n.parent.pos in
       let cost = costs.(ny)
       and children = ref [] in
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if (((x != px) || (y != py)) &&
			      (x >= 0) && (x < width) &&
			      (y >= 0) && (y < height) &&
			      (* only check after ensuring legality! *)
			      (not w.blocked.(x).(y))) then
			  Wrutils.push
			    (let c,g = ({pos = (x,y);
					 parent = n; },
					if (not (dx = 0)) && (not (dy = 0))
					then g +. (cost *. rt2)
					else (g +. cost)) in
			       if List.mem c.pos c_states
			       then
				 (let c_ind = Wrlist.find_index
				    (fun a -> equals a c.pos) c_states in
				    if p_data.(ind).(c_ind) > g
				    then p_data.(ind).(c_ind) <- g)
			       else (* handle secondary data *)
				 (let x,y = key c in
				    if sdata1.(x).(y) < g
				    then (*new best*)
				      (sdata1.(x).(y) <- g;
				       sdata2.(x).(y) <- ind));
			       c,g)
			    children)
	   moves;
	 !children)


let random_canonical_states ?(seed = 314159) ?(current = []) grid k =
  (** Selects [k] random canonical states which are unblocked*)
  Verb.pe Verb.toplvl "Generating %i canonical states at random.\n" k;
  let max_x = (width grid) - 1
  and max_y = (height grid) - 1 in
  let rec fn k c =
    (match k with
       | 0 -> c
       | _ -> (let next_x = Random.int max_x
	       and next_y = Random.int max_y in
		 if (grid.blocked.(next_x).(next_y)) ||
		   (List.mem (next_x,next_y) c)
		 then fn k c
		 else fn (k - 1) ((next_x,next_y)::c))) in
    Random.init seed;
    fn k current


let regular_canonical_states ?(seed = 314159) ?(current = []) grid k =
  (** Divide the grid evenly into k-sectors, place one midpoint near the center
      of each sector *)
  Random.init seed;
  Verb.pe Verb.toplvl "Generating %i canonical states regularly.\n" k;
  let max_x = (width grid) - 1
  and max_y = (height grid) - 1 in
  let a_ratio = (float_of_int (width grid)) /. (float_of_int (height grid)) in
  let v_lines = truncate ((float_of_int k) /. (a_ratio *. 4.)) in
  let h_lines = k / v_lines in
    Verb.pe Verb.toplvl "a_ratio: %f v_lines: %i h_lines: %i\n" a_ratio v_lines h_lines;
    let v_step = (height grid) / h_lines
    and h_step = (width grid) / v_lines in
      Verb.pe Verb.toplvl "%i %i\n" h_step v_step;
      let point = ref current in
      let rec place dx dy =
	if dx < 0 then place 0 dy  (* A bunch of bounds checking *)
	else if dy < 0 then place dx 0
	else if dx > max_x then place max_x dy
	else if dy > max_y then place dx max_y
	else
	  if not grid.blocked.(dx).(dy)
	  then (dx,dy)
	  else place (dx - (Random.int 3)) (dy - (Random.int 3)) in
	for x = 0 to (v_lines - 1)
	do
	  for y = 0 to (h_lines - 1)
	  do
	    (let (desiredx,desiredy) = (x * h_step + (h_step / 2),
					y * v_step + (v_step / 2)) in
	       point := (place desiredx desiredy)::!point)
	  done
	done;
	Verb.pe Verb.toplvl "%i c. states\n" (List.length !point);
	!point


let make_iface ind d grid c_states p_data sdata1 sdata2 c_state =
  (** Generates a search interface for finding canonical state information.
      Fed to the uniform cost search which fills in the shortest path
      information *)
  let c_board = {blocked = grid.blocked;
		   costs = grid.costs;
		   moves = grid.moves;
		   goal = grid.goal;
		   start = c_state;
		   instance = grid.instance;} in
    (Search_interface.make
       ~h:(get_cheapest_h grid)
       ~d:(get_cheapest_d grid)
       ~hd:(get_cheapest_hd grid)
       ~rev_h:(get_cheapest_h c_board)
       ~rev_d:(get_cheapest_d c_board)
       ~rev_hd:(get_cheapest_hd c_board)
       ~halt_on:[]
       ~domain_expand:(cexpand_func ind d grid c_state c_states
			 p_data sdata1 sdata2)
       ~key:key
       ~key_print:key_to_string
       ~equals:equals
       ~goal_p:(fun _ -> false)
       ~get_sol_length:sol_length
       (get_type grid)
       (make_root c_board)
       (fun _ _ -> false)
       (fun _ -> ()))


let calculate_data ?(d = 1) grid c_states =
  (** calculates both the primary and secondary data for the canonical
      states *)
  Verb.pe Verb.toplvl "C States:";
  List.iter (fun (x,y) -> Verb.pe Verb.toplvl "\t%i,%i" x y) c_states;
  Verb.pe Verb.toplvl "\n";
  let n = List.length c_states in
  let primary_data = Array.create_matrix n n infinity
  and secondary_data1 = Array.create_matrix (width grid) (height grid) (infinity)
  and secondary_data2 = Array.create_matrix (width grid) (height grid) (-1) in
    Wrlist.iteri
      (fun i canonical_state ->
	 Verb.pe Verb.toplvl "Solving for canonical state %i\n" i;
	 ignore
	   (Uniform_cost_search.dups_silent
	      (make_iface i d grid c_states primary_data secondary_data1
		 secondary_data2 canonical_state) [||]))
      c_states;
    Verb.pe Verb.toplvl "Canonical data calculated\n";
    (*check_secondary_data grid secondary_data;*)
    primary_data, secondary_data1, secondary_data2, c_states


let make_h grid (p_data, sdata1, sdata2) =
  (* Creates the canonical cost-to-go heuristic. *)
  let (gx,gy) = List.hd grid.goal in
  let (cg,dg) = (if Math.finite_p sdata1.(gx).(gy)
		    then sdata2.(gx).(gy), sdata1.(gx).(gy)
		    else -1, 0.) in
    (fun n ->
       let x,y = key n in
       let (cn,dn) = (if Math.finite_p sdata1.(x).(y)
		      then sdata2.(x).(y), sdata1.(x).(y)
		      else -1, 0.) in
       if cg >= 0 && cn >= 0 && cg <> cn
		then Math.fmax (p_data.(cg).(cn) -. (dg +. dn)) 0.
		else abs_float (dg -. dn))


(* I/O *)

let print ?(ch = stdout) ?(print = Verb.pf Verb.always ch) c_states
    w p_data sdata1 sdata2 =
  (** Primary data is a hashtable containg distances between two states
      Secondary data is a list of the nearest canonical states to a node*)
  let to_int = key_to_int w in
    Datafile.write_header_pairs ch;
    (* set up the altcolts *)
    Datafile.write_alt_colnames ch "primary_data"
      ["c1_key"; "c2_key"; "p_cost"];
    Datafile.write_alt_colnames ch "secondary_data"
      ["node"; "c_key"; "s_cost"];
    (* Print primary data *)
    for i = 0 to ((Array.length p_data) - 1)
    do
      for j = 0 to ((Array.length p_data) - 1)
      do
	(Datafile.write_alt_row_prefix ch "primary_data";
	 print "%i\t%i\t%f\n" i j p_data.(i).(j))
      done
    done;
    (* Print secondary data *)
    for x = 0 to ((width w) - 1)
      do
	for y = 0 to ((height w) - 1)
	do
	  (let cind = sdata2.(x).(y) in
	     if cind >= 0
	     then (Datafile.write_alt_row_prefix ch "secondary_data";
		   print "%i\t%i\t%f\n" (to_int (x,y))
		     (to_int (List.nth c_states cind)) sdata1.(x).(y)))
	done
    done;
    (* Notice that this isn't sufficient for finding the matching instance.
       You still have to rely on rdb to do that part of things for you. *)
    Datafile.write_pairs ch
      ["movement", move_to_string w.moves;
       "cost_model", cost_to_string w.costs;
       "height", string_of_int (height w);
       "width", string_of_int (width w);
       "instance", string_of_int w.instance;];
    Datafile.write_trailer_pairs ch


let from_df g df =
  (** Given a grid instance [g] and a datafile representing the stored
      heuristic data, returns primary and secondary data that can then be
      used to create a heuristic *)
  let c1_key = Datafile.get_col df "c1_key"
  and c2_key = Datafile.get_col df "c2_key"
  and p_cost = Datafile.get_col df "p_cost"
  and node = Datafile.get_col df "node"
  and c_key = Datafile.get_col df "c_key"
  and s_cost = Datafile.get_col df "s_cost" in
  let p_states = Array.length p_cost in
  let primary_data = Array.create_matrix p_states p_states infinity
  and sdata1 = Array.create_matrix (width g) (height g) (infinity)
  and sdata2 = Array.create_matrix (width g) (height g) (-1)
  and from_int = int_to_key g
  and to_int = key_to_int g
  and find_key = (fun v -> Wrarray.find (fun a -> a = v) c1_key ) in
    (* Toss all of the primary data into the primary data set *)
    Array.iteri (fun i cost ->
		   let j = find_key c1_key.(i)
		   and k = find_key c2_key.(i) in
		     (*Verb.pe Verb.always "%f\t%f\t%f -> %i\t%i\t%f\n"
		       c1_key.(i) c2_key.(i) cost j k cost;*)
		     primary_data.(j).(k) <- cost;
		     primary_data.(k).(j) <- cost) p_cost;
    (* Toss all of the secondary data in*)
    Array.iteri (fun i cost ->
		   let (nx,ny) = from_int (int_of_float node.(i))
		   and (cx,cy) = from_int (int_of_float c_key.(i)) in
		     sdata1.(nx).(ny) <- cost;
		     sdata2.(nx).(ny) <- (find_key
					    (float_of_int (to_int (cx,cy)))))
      s_cost;
    (* Return primary and secondary data*)
    primary_data, sdata1, sdata2


let from_df_path g p =
  (** Allows you to do the same thing as from df, but with a string instead
      of a data file *)
  from_df g (Datafile.load p)


let from_ch g c =
(** Allows you to do the same thing as from df, but with a channel instead
      of a data file *)
  from_df g (Datafile.read "KaizerSoze" c)


let from_gridpath ?(c_fun = (fun () -> failwith "Heuristic not stored"))
    g gridpath =
  (** Attempts to load canonical heuristic data already stored
      on failure, it will call [c_fun].  By default, c_fun fails,
      but when replaced with a function for generating canonical data,
      will create and store canonical data, then return *)
  let f = Str.split (Str.regexp "/") gridpath in
  let path = List.fold_left
    (fun accum a ->
       if a = "instance"
       then accum ^ "/" ^ "canonical"
       else accum ^ "/" ^ a) "" f in
    Verb.pe Verb.debug "Path to canonical heuristic: %s\n" path;
    if Sys.file_exists path
    then from_df_path g path
    else (Verb.pe Verb.always "Canonical Data not stored, generating\n";
	  Verb.pe Verb.toplvl "Filling in unreachable states\n";
	  block_unreachable g;
	  let p_data, sdata1, sdata2, c_states = c_fun() in
	    Verb.pe Verb.always "Data generated, storing @ %s\n" path;
	    Wrfname.ensure_path path;
	    Wrio.with_outfile path
	      (fun ch ->
		 print ~ch:ch c_states g p_data sdata1 sdata2);
	    p_data, sdata1, sdata2)




(* Interfaces *)

let canonical_interface ?(g_path = "") k w lim =
  (** Standard canonical interface.  Does NOT take the max of the
      memory based heuristic and an admissible heuristic *)
  let h = make_h w (from_gridpath ~c_fun:(fun () ->
					    (calculate_data w
					       (random_canonical_states w k)))
		      w g_path)
  and d = (get_cheapest_d w) in
    (Search_interface.make
       ~h:h
       ~d:d
       ~hd:(fun n -> h n, d n)
       ~domain_expand:(make_expand_wp w)
       ~key:key
       ~key_print:key_to_string
       ~equals:equals
       ~goal_p:(make_goal_p w)
       ~halt_on:lim
       ~get_sol_length:sol_length
       (get_type w)
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let canonical_maxed_with_admiss_interface ?(g_path = "") k w lim =
  (** Modified canonical interface.  Takes the max of the
      memory based heuristic and an admissible heuristic *)
  let h = make_h w (from_gridpath ~c_fun:(fun () ->
					    (calculate_data w
					       (random_canonical_states w k)))
		      w g_path)
  and d = (get_cheapest_d w)
  and admiss_h = (get_cheapest_h w) in
  let fin_h = (fun n ->
		 let can_h = h n
		 and a_h = admiss_h n in
		   Verb.pe Verb.toplvl "ch: %f\tah: %f\n" can_h a_h;
		   Math.fmax can_h a_h) in
    (Search_interface.make
       ~h:fin_h
       ~d:d
       ~hd:(fun n -> fin_h n, d n)
       ~domain_expand:(make_expand_wp w)
       ~key:key
       ~key_print:key_to_string
       ~equals:equals
       ~goal_p:(make_goal_p w)
       ~halt_on:lim
       ~get_sol_length:sol_length
       (get_type w)
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))

(* EOF *)
