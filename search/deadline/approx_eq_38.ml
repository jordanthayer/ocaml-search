(**

    @author jtd7
    @since 2011-01-20

   Approximating the equation 38 table, for SCIENCE
*)
let base = Eq_38_tables.base
let tiles = base ^ "approx_tiles/"
let grids = base ^ "approx_grids/"
let robot = base ^ "approx_dyn_robots/"
let ugrid = grids ^ "approx_ugrids/"
let lgrid = grids ^ "approx_lgrid/"
let dock = base ^ "approx_dock_robot"

let (|/) a b = truncate (ceil ((float a) /. (float b)))

let get_path domain res =
  let dir = (match domain with
	       | Search_interface.Tiles -> tiles
	       | Search_interface.Robot -> robot
	       | Search_interface.UGrid -> ugrid
	       | Search_interface.LGrid -> lgrid
	       | Search_interface.Dock_robot -> dock
	       | _ -> failwith "No Tables For this Domain") in
   dir ^ (string_of_int res)


let get_kl res domain contract =
  let path_base = get_path domain res in
  let max_d = truncate (Eq_38_tables.depth_est domain *. 1.5) in
  let vals = Array.create (max_d + 1) 0 in
  let r = ref (contract |/ res)
  and last_n = ref (1 |/ res) in
    for depth = 0 to max_d do
      (let this_row = (Printf.sprintf "%s_%i" path_base depth) in
       let array = ((Wrio.with_infile this_row Marshal.from_channel) :
		      (int array array)) in
	 (* there is a weird edge case with computing c from c1
	    that causes this.  Assign minimum value to this bucket *)
	 let n = try array.(!last_n).(!r) with _ -> 1 in
	   last_n := n;
	   r := max (!r - n) 0;
	   vals.(depth) <- n * res)
    done;
    Verb.pe Verb.always "Table loaded\n%!";
    vals


let calc_size_gig ?(res = 1) depth_estimate bfact contract =
  let contract = contract |/ res in
  let entries_in_layer d = Math.int_exp (contract - (d |/ res)) 2 in
  let layer_as_ints d = (entries_in_layer d) * 8
  and layer_as_float d = (contract / bfact) * (contract - (d |/ res)) * 8 in
  let int_table = (List.fold_left (fun accum e -> accum + layer_as_ints e)
		     0 (Wrlist.range ~min:0 depth_estimate)) in
  let max_layer = layer_as_float 0
  and max_prev = layer_as_float 1 in
  let in_bytes = int_table + max_layer + max_prev in
    in_bytes / 1073741824


let calc_table ?(res = 100) domain success_prob goal_prob contract =
  let max_depth = truncate (Eq_38_tables.depth_est domain *. 1.5)
  and bfactor = Eq_38_tables.bfactor domain in
  let max_c_at_depth = Eq_38_tables.max_c_at_depth bfactor
  and max_r_at_depth = Eq_38_tables.max_r_at_depth contract
  and best_n = ref 0
  and best_v = ref 0.
  and n = ref 0
    (* store the tables totally in memory *)
  and this_layer = ref [||]
  and prev_layer = ref [||]
  and outfile = get_path domain res in
    for depth = max_depth downto 0 do
      (prev_layer := !this_layer;
       let max_c = max_c_at_depth depth
       and max_r = max_r_at_depth depth in
       let max_c = (if max_c <= 0 (* overflowed *)
		    then max_r
		    else Math.imin max_c max_r) in
       let max_c_index = truncate (ceil ((float max_c) /. bfactor)) in
       let goal_p = goal_prob depth in
       let cindex_in_res = max_c_index |/ res
       and maxr_in_res = max_r |/ res in
	 Verb.pe Verb.toplvl "depth @ %i, c %i r %i, goal_p %f %i %i\n%!"
	   depth max_c max_r goal_p cindex_in_res maxr_in_res;
	 let ns = Array.create_matrix (cindex_in_res + 1) (maxr_in_res + 1) 0 in
	   this_layer := (Array.create_matrix (cindex_in_res + 1)
			    (maxr_in_res + 1) 0.);
	   for ci = cindex_in_res downto 1 do
	     (let c = (truncate (ceil ((float (ci * res)) *. bfactor))) in
		for r = maxr_in_res downto 0 do
		  (best_n := 1;
		   best_v := -1.;
		   n := 1;
		   while !n < (min (c |/ res) r) && !best_v < 1. do
		     (let v =
			Math.fmin 1.
			  (if depth = max_depth
			   then (success_prob depth (!n * res)) *. goal_p
			   else
			     (success_prob depth (!n * res)) *.
			       (goal_p +.
				  (* I'm worried about this *)
 				  (try
				     !prev_layer.(!n).(r - !n)
				   with _ ->
				     (* next c exceeds next remaining,
					treat as if you kept c best *)
					 !prev_layer.((Array.length
					 !prev_layer) - 1).
					 (r - !n)))) in
			if v > !best_v then (best_n := !n; best_v := v);
			n := !n + 1)
		   done;
		   !this_layer.(ci).(r) <- !best_v;
		   ns.(ci).(r) <- !best_n)
		done)
	   done;
	   let out = Printf.sprintf "%s_%i" outfile depth in
	     Wrio.with_outfile out
	       (fun ch -> Marshal.to_channel ch ns []))
    done

(* EOF *)
