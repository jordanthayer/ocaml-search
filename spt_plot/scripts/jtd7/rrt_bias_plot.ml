(**

    @author jtd7
    @since 2012-01-30
*)


let get_first_non_inf (times,costs) =
  let first_finite = ref (-1) in
    Array.iteri (fun i el ->
		   if !first_finite < 0 && Math.finite_p el
		   then first_finite := i) costs;
    if !first_finite < 0
    then (try times.((Array.length times) - 1)
	  with _ -> 90.)
    else times.(!first_finite)


let tr = ref (fun best v -> let vl = best /. v in
		if Math.finite_p vl
		then (if (vl < 0. || vl > 1.)
		      then (Verb.pe Verb.always "%f %f\n%!" best v); vl)
		else 1.)

let x_tr = ref Fn.identity

let get_first_sol_time maxes by_inst =
  let dset = List.hd by_inst in
  let times_costs =
    List.map
      (fun ds ->
	 let pc = (Dataset.get_values float_of_string
		     "Precomputation Time" ds).(0) in
	 let times = Dataset.get_values float_of_string "time val" ds
	 and costs = Dataset.get_values float_of_string "cost val" ds in
(*	   Array.map (fun v -> v -. pc)*) times,
	 costs) by_inst in
  let values = List.map2 (fun ds max ->
			   !tr max (get_first_non_inf ds))
    times_costs maxes in
    Dataset.get_name dset, Array.of_list values



let step_points points =
  let rec alter accum last_y = function
      | [] -> List.rev accum
      |	hd::tl -> (let pt = Geometry.point ~x:hd.Geometry.x ~y:last_y in
		   let accum' = hd::pt::accum in
		   alter accum' hd.Geometry.y tl) in
  let fst = List.hd points in
  let points' = alter [fst] fst.Geometry.y (List.tl points) in
  Array.of_list points'


let get_anytime_line ?(max_x = 20.) bests by_inst =
  let dset = List.hd by_inst in
  let lines = List.map2
    (fun best ds ->
       let times = Dataset.get_values float_of_string "time val" ds
       and costs = Dataset.get_values float_of_string "cost val" ds in
       let pts = Wrarray.map2 (fun x y -> Geometry.point
				 ~x:(!x_tr x)
				 ~y:(!tr best y)) times costs in
       let pts_list = Array.to_list pts in
       let pts' = (Geometry.point ~x:(!x_tr 0.)
		     ~y:(!tr best infinity))::pts_list in
       let pts'' = List.rev pts' in
       let last = List.hd pts'' in
       let pts''' = List.rev ((Geometry.point ~x:(!x_tr max_x)
			       ~y:(last.Geometry.y))::pts'') in
	 step_points pts'''
    ) bests by_inst in
    Some (Dataset.get_name dset),
  Array.of_list lines



let instance_solved ds =
  let costs = Dataset.get_values float_of_string "cost val" ds in
  let v = Array.fold_left min infinity costs in
    Math.finite_p v, v


let solved_instances ilist attrs =
    List.fold_left
      (fun accum (i,otime) ->
	 let attrs' = ("instance", string_of_int i)::attrs in
	   List.iter (fun (a,b) -> Verb.pe Verb.debug "%s,%s\n%!" a b) attrs';
	   try
	     let ds = Load_dataset.load_biased_sample
	       [attrs', (Printf.sprintf "instance %i" i)] in
	       assert((List.length ds) == 1);
	     let solved,time = instance_solved (List.hd ds) in
	       if solved
	       then (i, min time otime)::accum
	       else accum
	   with _ -> accum) [] ilist


let solved_by_all attr_list =
  let instances = Wrlist.range ~min:0 99 in
  let instances = List.map (fun a -> a,infinity) instances in
  let instances = List.fold_left
    (fun ilist attrs -> solved_instances ilist attrs)
    instances attr_list in
  let instances, maxes = List.split instances in
    instances, maxes


let time_to_first ?(cpfix = ["alg", "rrt";
			      "stepsize", "1000";
			      "iterations", "1000";])
    ?(ylabel = "Log10 Speedup")
    ?(title = "Speedup Over Worst First Solution")
    alg_list =
  let attrs_by_name_list = List.map (fun (attrs,nm) ->
				       attrs@cpfix, nm) alg_list in
  let instances,maxes = solved_by_all [("bias", "unbiased")::cpfix] in
    Verb.pe Verb.always "%i instances\n%!" (List.length instances);
  let dsets = List.map (fun (attrs,nm) ->
			  let all_vals = List.map
			    (fun i -> ("instance", string_of_int i)::attrs,nm)
			    instances in
			    Load_dataset.load_biased_sample all_vals)
    attrs_by_name_list in
  let name_by_val_array = List.map (get_first_sol_time maxes) dsets in
  let dsets' = Boxplot_dataset.boxplot_datasets ~interval:false ~outliers:false
    name_by_val_array in
  let plot = Num_by_nom.plot ~title ~ylabel (*~y_max:20.*)
    dsets' in
    plot#display



let mins_instances ilist attrs =
    List.fold_left
      (fun accum (i,otime) ->
	 let attrs' = ("instance", string_of_int i)::attrs in
	   List.iter (fun (a,b) -> Verb.pe Verb.debug "%s,%s\n%!" a b) attrs';
	   try
	     let ds = Load_dataset.load_biased_sample
	       [attrs', (Printf.sprintf "instance %i" i)] in
	       assert((List.length ds) == 1);
	     let solved,time = instance_solved (List.hd ds) in
	       (i, min time otime)::accum
	   with _ -> accum) [] ilist


let mins attr_list =
  let instances = Wrlist.range ~min:0 99 in
  let instances = [0;1;2;3;4;5;6;7;8;9] in
  let instances = List.map (fun a -> a,infinity) instances in
  let instances = List.fold_left
    (fun ilist attrs -> mins_instances ilist attrs)
    instances attr_list in
  let instances, maxes = List.split instances in
    instances, maxes


let anytime_profile ?(cpfix = ["alg", "rrtstar";
			      "stepsize", "1000";
			      "iterations", "20000";])
    ?(ylabel = "IPC Metric?")
    ?(xlabel = "Time")
    ?(title = "Foo")
    alg_list =
  let attrs_by_name_list = List.map (fun (attrs,nm) ->
				       attrs@cpfix, nm) alg_list in
  let instances,maxes = mins (List.map fst attrs_by_name_list) in
    List.iter (fun v -> Printf.printf "\t%i" v) instances;
    Printf.printf "\n%!";
    List.iter (fun v -> Printf.printf "\t%f" v) maxes;
    Printf.printf "\n%!";
  let dsets = List.map (fun (attrs,nm) ->
			  let all_vals = List.map
			    (fun i -> ("instance", string_of_int i)::attrs,nm)
			    instances in
			    Load_dataset.load_biased_sample all_vals)
    attrs_by_name_list in
  let name_by_val_array = List.map (get_anytime_line maxes) dsets in
  let dsets' = Line_errbar_dataset.line_errbar_datasets
    ~line_width:(Length.Pt 2.) ~color:true name_by_val_array in
  let plot = Num_by_num.plot ~title
    ~ylabel
    ~xlabel
(*  ~y_max:1000.
    ~y_min:0.*)
    ~legend_loc:Legend.Lower_right
    dsets' in
    plot#display


(* bias, hweight, iterations, stepsize, instance *)
