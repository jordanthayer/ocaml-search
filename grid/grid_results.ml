(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $

   reading results
*)


(*************** simple evaluation (time,quality) **********************)


let alg_str a =
  (** abbreviations for algorithms *)
  List.assoc a [ "a_star", "A*";
		 "greedy", "Gr";
		 "wted_a_star", "wA*";
		 "anytime_a_star", "AA*";
		 "ara_star", "ARA*";
		 "dyn_wted_a_star", "DWA*";
		 "speedy", "Sp";
		 "a_star_eps", "A*e";
		 "bugsy", "Bugsy";
		 "bugsy_coeff", "BugsyCo";
	       ]


let get_alg_dfs inst_attrs alg =
  Wrutils.pr "Loading %s on %s...%!" alg (Rdb.attrs_str [] inst_attrs);
  let dfs = Array.of_list (List.map Datafile.load
			     (Rdb.matching_paths Grid_runs.data_root
				(["type", "run";
				  "width", "2000";
				  "height", "1200";
				  "alg", alg] @
				 inst_attrs))) in
  let n = Array.length dfs in
    Wrutils.pr "got %d files...%!" n;
    if n != 20 then Wrutils.pr "######";
    dfs


(*************** evaluation on the basis of "utility" ***************)


let utility_tuples = Array.of_list Grid_runs.utility_tuples


let basic_util_str c t =
  Wrutils.str "%s, %s"
    (if c = 0. then "0"
     else if c = 1. then "1"
     else string_of_float c)
    (if (t < 1000.) && (Math.integral_p t) then
       (string_of_int (truncate t))
     else
       let e = log10 t in
	 if Math.integral_p e then
	   Wrutils.str "1e%d" (truncate e)
	 else string_of_float t)

let util_str c t =
  (** short string representation for utility coeffs *)
  if (c = 0.) && (t = 1.) then
    "time only"
  else if c = 1. then
    if t = 0. then
      "cost only"
    else
      let near x =
	Math.within_rel (1. /. t) x 1e-6
      in
	if near 0.1 then
	  "0.1 sec"
	else if near 0.05 then
	  "50 msec"
	else if near 0.01 then
	  "10 msec"
	else if near 0.005 then
	  "5 msec"
	else if near 0.001 then
	  "1 msec"
	else if near 0.0005 then
	  "500 microsec"
	else if near 0.0001 then
	  "100 microsec"
	else if near 0.00005 then
	  "50 microsec"
	else if near 0.00001 then
	  "10 microsec"
	else if near 0.000005 then
	  "5 microsec"
	else if near 0.000001 then
	  "1 microsec"
	else if near 0.0000005 then
	  "500 nanosec"
	else if near 0.0000001 then
	  "100 nanosec"
	else
	  (Wrutils.pr "WARNING: bad t %f in util\n" t;
	   basic_util_str c t)
  else
    (Wrutils.pr "WARNING: bad c in util\n";
     basic_util_str c t)


(*
let best_data time_key (cost_coeff, time_coeff) df =
  (** returns utility of best sol in [df] according to [util] *)
  Wrarray.fold_left2 (fun ((best_util,_,_) as prev) cost time ->
			let util = -. ((cost_coeff *. cost) +.
					 (time_coeff *. time)) in
			  if util > best_util then
			    (util, cost, time)
			  else prev)
    (neg_infinity, nan, nan)
    (Datafile.get_col df "sol cost")
    (Datafile.get_col df time_key)


let best_util time_key u df =
  (** returns utility of best sol in [df] according to [util] *)
  let u,_,_ = best_data time_key u df in
    u


let get_alg_data time_key inst_attrs alg =
  (** assumes that [alg] and the [inst_attrs] define a single batch of
    experiments - eg, many instances and one run per.  returns data for each
    utility function.  if multiple solutions, chooses best for each utility
    function. *)
  let dfs = get_alg_dfs inst_attrs alg in
  let data = Array.map (fun (cost,time) ->
			  let name = util_str cost time in
			  let data = Array.map (best_util time_key (cost,time))
				       dfs in
			    data, name)
	       utility_tuples in
    Wrutils.pr "done.\n%!";
    data, (alg_str alg)
*)


let all_times pairs =
  let a = Array.concat (Array.to_list (Array.map snd pairs)) in
    Array.sort compare a;
    a


let best_time (cost_coeff, time_coeff) pairs =
  let indices = Array.make (Array.length pairs) 0 in
    fst (Wrarray.max_by
	   (fun t ->
	      let sum = ref 0. in
		Array.iteri
		  (fun i (cost, time) ->
		     while (((indices.(i) + 1) < Array.length time) &&
			    (time.(indices.(i) + 1) <= t)) do
		       indices.(i) <- indices.(i) + 1
		     done;
		     (* either this is last result or next time is past t *)
		     sum := !sum -. ((cost_coeff *. cost.(indices.(i))) +.
				       (time_coeff *.
					  ( if ((indices.(i) + 1) =
						  Array.length time) then
					      (* also if n=1 *)
					      time.(indices.(i))
					    else if indices.(i) = 0 then
					      max time.(0) t
					    else t))))
		  pairs;
		!sum)
	   (all_times pairs))


let data_at_time (cost_coeff, time_coeff) t (cost, time) =
  let n = Array.length time
  and i = ref 0 in
    while (((!i+1) < n) &&
	   (time.(!i+1) <= t)) do
      incr i
    done;
    let c = cost.(!i)
    and t = (if (!i+1 = n) then
	       (* also if n=1 *)
	       time.(!i)
	     else if !i = 0 then
	       max time.(!i) t
	     else t) in
    let u = -. ((cost_coeff *. c) +. (time_coeff *. t)) in
      u, c, t


let best_data_for_static_time time_key u dfs =
  (** returns array of u,c,t triples, one per df *)
  let pairs = Array.map (fun df ->
			   (Datafile.get_col df "sol cost"),
			   (Datafile.get_col df time_key))
		dfs in
    if (Array.length pairs) = 0 then
      [|nan,nan,nan|]
    else
      let best_time = best_time u pairs in
	Array.map (data_at_time u best_time) pairs


let best_utils_for_static_time time_key u dfs =
  Array.map (fun (u,_,_) -> u)
    (best_data_for_static_time time_key u dfs)


let get_alg_data time_key inst_attrs alg =
  (** assumes that [alg] and the [inst_attrs] define a single batch of
    experiments - eg, many instances and one run per.  returns data for each
    utility function.  if multiple solutions, chooses best for each utility
    function. *)
  let dfs = get_alg_dfs inst_attrs alg in
  let data = Array.map (fun u ->
			  (best_utils_for_static_time time_key u dfs), u)
	       utility_tuples in
    Wrutils.pr "done.\n%!";
    data, (alg_str alg)


let get_bugsy_data_for time_key inst_attrs
  alg bugsy_attrs ((cost, time) as u) =
  (** for a specific utility function *)
  let attrs = inst_attrs @ bugsy_attrs @
	      ["cost_coeff", string_of_float cost;
	       "time_coeff", string_of_float time; ] in
  let dfs = get_alg_dfs attrs alg in
  let data = best_utils_for_static_time time_key u dfs in
    Wrutils.pr "done.\n%!";
    data, u


let get_bugsy_data time_key inst_attrs alg bugsy_attrs =
  (** need to get specific bugsy run for each utility function *)
  let data = Array.map (get_bugsy_data_for time_key inst_attrs alg bugsy_attrs)
	       utility_tuples in
    data, (alg_str alg)


let group_ranges data_sets =
  (** returns an array of (min,max) pairs, one for each corresponding
    utility function *)
  (* ((((float array) * string) array) * string) list *)
  match data_sets with
    [] -> failwith "group_ranges: no data"
  | (groups,_)::_ ->
      let num_groups = Array.length groups in
	Array.init num_groups
	  (fun i ->
	     (* computes min and max for particular group *)
	     Wrlist.min_and_max_across
	     (fun (groups, _) ->
		(* computes min and max for that group in a particular set *)
		let data,_ = groups.(i) in
		  Vector.min_and_max data)
	     data_sets)


let normalize a min max =
  (** transform the floats in [a] so that [min] goes to 0. and [max] goes
    to 1. *)
  let range = max -. min in
    Array.map (fun x ->
		 Math.div0 (x -. min) range)
      a


let normalize_utilities data_sets =
  (** normalize data to 0-1 *)
  let ranges = group_ranges data_sets in
    List.map (fun (groups, name) ->
		(Wrarray.map2 (fun (data, name) (min, max) ->
				 (normalize data min max), name)
		   groups ranges), name)
      data_sets


let add_condition_names sets =
  List.map (fun (groups, alg) ->
	      (Array.map (fun (data, (cost,time)) ->
			    data, (util_str cost time))
		 groups), alg)
    sets


(*************** box plots of utility ***************)


let plot_instance time_key inst_attrs bugsy_attrs other_algs =
  (** single set of instances, multiple algs and utility functions.
    returns path of postscript file *)
  let title = Rdb.attrs_str [] inst_attrs
  and sets = (get_bugsy_data time_key inst_attrs "bugsy_coeff" bugsy_attrs)::
	       (get_bugsy_data time_key inst_attrs "bugsy" bugsy_attrs)::
	       (List.map (get_alg_data time_key inst_attrs) other_algs) in
  let path = Filename.temp_file title ".ps"
  and sets = normalize_utilities sets in
  let sets = add_condition_names sets in
    (* for each alg, array across utility funcs.  each elt is (data, name)
       pair. *)
    Ps_plot.box_sets path sets title
      "Utility Function (time worth one cost unit)"
      "Normalized Solution Utility"
      ~divide_vals:true ~sort_sets:false;
    path


let test_plot_instance () =
  let key = "cpu time"
  and costs = Grid.Unit
  and ways = Grid.Fourway
  and p = 0.2 in
    (* ASSUMING ONLY ONE HEURISTIC/CONFIGURATION HAS BEEN USED WITH EACH
       ALG *)
    plot_instance key (Grid_runs.batch_attrs costs ways p) []
      ["speedy";
       "greedy";
       "wted_a_star";
       "anytime_a_star";
       "ara_star";
       (* "a_star_eps"; *)
       "a_star"; ]


let plot_paper_instances () =
  let key = "cpu time"
  and algs = ["speedy";
	      "greedy";
	      "anytime_a_star";
	      "ara_star";
	      "a_star"; ] in
  let do_plot costs ways p =
    plot_instance key (Grid_runs.batch_attrs costs ways p) [] algs
  in
    (* do_plot "unit" "8-way" 0.4;
       do_plot "unit" "4-way" 0.2; *)
    do_plot Grid.Life Grid.Fourway 0.2


let plot_batch () =
  (* ASSUMING ONLY ONE HEURISTIC/CONFIGURATION HAS BEEN USED WITH EACH ALG *)
  let plots = Grid_runs.batch_map (fun costs ways p ->
				let key = "cpu time" in
				  plot_instance key
				    (Grid_runs.batch_attrs costs ways p) []
				    ["speedy";
				     "greedy";
				     (* "wted_a_star"; *)
				     (* "a_star_eps"; *)
				     "anytime_a_star";
				     "ara_star";
				     "a_star"; ]) in
    Ps_plot.montage ~across:1 ~down:6 plots
      "/tilde/ruml/projects/path/grid/cost_time_montage.ps"


(********** table of utility ***************)


let sets_for_table sets =
  List.map (fun (rows, name) ->
	      (Array.map (fun (data, (cost,time)) ->
			    let str = util_str cost time in
			      (Vector.multiply_by data 100.), str)
		 rows), name)
    sets


let table key attrs other_algs =
  let sets = (get_bugsy_data key attrs "bugsy_coeff" [])::
	       (get_bugsy_data key attrs "bugsy" [])::
	       (List.map (get_alg_data key attrs) other_algs) in
  let sets = normalize_utilities sets in
  let sets = sets_for_table sets in
    Wrutils.pr "-- %s --\n" (Rdb.attrs_str [] attrs);
    Table.ordinal_sets sets "$U()$" 0


let old_utility_table () =
  let key = "cpu time"
  and other_algs = ["ara_star"; "anytime_a_star";
		    "speedy"; "greedy"; "a_star"] in
    List.iter (fun attrs -> table key attrs other_algs)
      [["moves","8-way"; "costs","unit"; "prob","0.4"];
       ["moves","4-way"; "costs","unit"; "prob","0.2"];
       ["moves","4-way"; "costs","life"; "prob","0.2"]]


let utility_table () =
  let key = "cpu time"
  and other_algs = ["ara_star"; "anytime_a_star";
		    "speedy"; "greedy"; "a_star"] in
    List.iter (fun (costs, ways, p) ->
		 table key (Grid_runs.batch_attrs costs ways p) other_algs)
      [ Grid.Unit, Grid.Eightway, 0.4;
	Grid.Unit, Grid.Fourway, 0.2;
	Grid.Life, Grid.Fourway, 0.2  ]


(**************** scatter plots of solutions ******************)


let best_sols_for_static_time time_key u dfs =
  Array.map (fun (_,c,t) -> c,t)
    (best_data_for_static_time time_key u dfs)


let get_alg_sols time_key inst_attrs alg =
  (** array of (float * float) arrays, one for each utility function. *)
  let dfs = get_alg_dfs inst_attrs alg in
  let data = Array.map (fun (cost,time) ->
			  best_sols_for_static_time time_key (cost,time) dfs)
	       utility_tuples in
    Wrutils.pr "done.\n%!";
    (alg_str alg), data


let get_bugsy_sols_for time_key inst_attrs bugsy_attrs (cost, time) =
  (** for a specific utility function *)
  let alg = "bugsy"
  and attrs = inst_attrs @ bugsy_attrs @
	      ["cost_coeff", string_of_float cost;
	       "time_coeff", string_of_float time; ] in
  let dfs = get_alg_dfs attrs alg in
  let data = best_sols_for_static_time time_key (cost,time) dfs in
    Wrutils.pr "done.\n%!";
    data


let get_bugsy_sols time_key inst_attrs bugsy_attrs =
  (** need to get specific bugsy run for each utility function *)
  let alg = "bugsy" in
  let data = Array.map (get_bugsy_sols_for time_key inst_attrs bugsy_attrs)
	       utility_tuples in
    (alg_str alg), data


let plot_utils_sols time_key inst_attrs bugsy_attrs other_algs =
  (** returns list of paths to plots for various utility functions on this
    one instance. *)
  let data = (get_bugsy_sols time_key inst_attrs bugsy_attrs)::
	       (List.map (get_alg_sols time_key inst_attrs) other_algs) in
    Array.to_list
      (Array.mapi
	 (fun i (cost, time) ->
	    let path = Filename.temp_file "plot-sols-" ".ps"
	    and data = List.map (fun (name, sets) ->
				   name, "", sets.(i))
			 data
	    and title = (Rdb.attrs_str [] inst_attrs) ^ ", " ^
			(util_str cost time) in
	      Ps_plot.scatter path data title
		~legend_pos:Ps_plot_interface.Upperright
		"Solution Cost" "Solving Time (seconds)";
	      path)
	 utility_tuples)


let plot_batch_sols () =
  let plots = Grid_runs.batch_fold (fun a costs ways p ->
				 let key = "cpu time" in
				 let basic = Grid_runs.batch_attrs costs ways p in
				 let plots = plot_utils_sols key basic []
					       ["speedy";
						"ara_star";
						"a_star"; ] in
				   List.append a plots) [] in
    Ps_plot.montage ~across:4 ~down:6 plots
      "/tilde/ruml/projects/path/grid/cost_time_montage.ps"




(* EOF *)
