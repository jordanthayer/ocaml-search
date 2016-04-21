(**
    @author jtd7
    @since 2012-01-20

   Scripts to deal with anytime profiles in planning
*)

let root = "./research/data/planner"

let get_lines fname =
  if fname = ""
  then []
  else Wrio.with_infile fname Wrio.input_lines


let get_cost ln =
  Verb.pe Verb.toplvl "Getting cost from |%s|\n%!" ln;
  try
    Scanf.sscanf ln "Plan cost: %f" (fun i -> i)
  with _ -> -1.


let get_time ln =
  Verb.pe Verb.toplvl "Getting time from |%s|\n%!" ln;
  try
    Scanf.sscanf ln "Actual search time: %fs [t=%fs]" (fun lt at -> at)
  with _ -> -1.


let cheat_log x =
  if x = 0. then -2. else log10 x


let cheat_norm a b =
  if a == b then 1.
  else a /. b

let step_points points =
  let rec alter accum last_y = function
      | [] -> List.rev accum
      |	hd::tl -> (let pt = Geometry.point ~x:hd.Geometry.x ~y:last_y in
		   let accum' = hd::pt::accum in
		   alter accum' hd.Geometry.y tl) in
  let fst = List.hd points in
  let points' = alter [fst] fst.Geometry.y (List.tl points) in
  Array.of_list points'


let make_points ?(ny = 1.) ?(norm_x = cheat_log) costs times =
  let points = List.map2 (fun y x -> Geometry.point
			    ~x:(norm_x x) ~y:(cheat_norm ny y)) costs times in
    step_points points




let find_min time_cost_lists =
  List.fold_left (fun outer (_,clist) ->
		    min outer (List.fold_left
				 (fun inner e -> min e inner) infinity clist))
    infinity time_cost_lists


let rec adjust_lists ?(mtime = 1800.) cost time =
  let ltime = List.length time
  and lcost = List.length cost in
    if (ltime = 0)
    then [0.; 1800.;], [infinity; infinity;]
    else if ltime = lcost
    then (let last_val = List.hd cost in
	    0.::(List.rev (mtime::time)), infinity::(List.rev (last_val::cost)))
    else (assert(ltime > lcost);
	  adjust_lists ~mtime cost (List.tl time))


let rec get_time_cost_lines ?(t_acc = []) ?(c_acc = []) = function
  | [] -> adjust_lists c_acc t_acc
  | hd::tl -> (let tv = get_time hd in
		 if tv < 0.
		 then (let cv = get_cost hd in
			 if cv < 0.
			 then get_time_cost_lines ~t_acc ~c_acc tl
			 else get_time_cost_lines ~t_acc ~c_acc:(cv::c_acc) tl)
		 else get_time_cost_lines ~t_acc:(tv::t_acc) ~c_acc tl)


let single_instance ?(nms = None) domain problem algs =
  let nms = (match nms with None -> algs | Some l -> l) in
  let paths = List.map (fun alg ->
			  try
			  Rdb.matching_path root ["alg", alg;
						  "domain", domain;
						  "name", problem]
			  with _ ->
			    Verb.pe Verb.always "No Matching paths for";
			    List.iter (Verb.pe Verb.always "\t %s")
			      [domain; alg; problem;];
			    Verb.pe Verb.always "\n%!";
			    "") algs in
  let lines = List.map get_lines paths in
    Verb.pe Verb.toplvl "%i lines\n%!" (List.length (List.hd lines));
  let time_cost = List.map get_time_cost_lines lines in
  let min_y = find_min time_cost in
    List.map2 (fun (time,cost) name -> Some name,
		 make_points ~ny:min_y cost time) time_cost nms


let multiple_instances domain ?(nms = None) algs =
  let nms = (match nms with None -> algs | Some l -> l) in
  let all_attrs = Rdb.matching_attrs root ["domain", domain;
					   "alg", List.hd algs] in
  let all_attrs = List.flatten all_attrs in
  let problems = Rdb.filter_attrs ["domain"; "alg";] all_attrs in
  let name_norm = List.map (fun (_,nm) ->
				single_instance domain nm algs) problems in
  let data = List.fold_left (fun accum nm_data_list ->
			       List.map2 (fun (nm1,dla) (nm2,dl) ->
					    nm1, dl::dla) accum nm_data_list)
    (List.map (fun nm -> Some nm,[]) nms) name_norm in
    List.map (fun (nm_opt,dlist) -> nm_opt, Array.of_list dlist) data


let single_instance_count ?(nms = None) domain problem algs =
  let nms = (match nms with None -> algs | Some l -> l) in
  let paths = List.map (fun alg ->
			  try
			  Rdb.matching_path root ["alg", alg; "domain", domain;
						  "name", problem]
			  with _ -> "") algs in
  let lines = List.map get_lines paths in
  let time_cost = List.map get_time_cost_lines lines in
    List.map2 (fun (a,b) nm -> Some nm, List.length a) time_cost nms


let multiple_instance_counts domain ?(nms = None) algs =
  let nms = (match nms with None -> algs | Some l -> l) in
  let all_attrs = Rdb.matching_attrs root ["domain", domain;
					   "alg", List.hd algs] in
  let all_attrs = List.flatten all_attrs in
  let problems = Rdb.filter_attrs ["domain"; "alg";] all_attrs in
  let name_norm = List.map (fun (_,nm) ->
			      single_instance_count domain nm algs) problems in
  let data = List.fold_left (fun accum nm_data_list ->
			       List.map2 (fun (nm1,dla) (nm2,dl) ->
					    nm1, dl::dla) accum nm_data_list)
    (List.map (fun nm -> Some nm,[]) nms) name_norm in
    List.map (fun (nm, dlist) -> let total = List.fold_left (+) 0 dlist in
		(float total) /. (float (List.length dlist))) data




let single_instance_times ?(nms = None) domain problem algs =
  let nms = (match nms with None -> algs | Some l -> l) in
  let paths = List.map (fun alg ->
			  try
			  Rdb.matching_path root ["alg", alg; "domain", domain;
						  "name", problem]
			  with _ -> "") algs in
  let lines = List.map get_lines paths in
  let time_cost = List.map get_time_cost_lines lines in
    List.map2 (fun (times,_) nm ->
		 let times = Array.of_list times in
		 let values = ref [] in
		   for i = ((Array.length times) - 1) downto 1
		   do
		     values := (times.(i) -. times.(i-1))::!values
		   done;
		   Some nm, !values) time_cost nms


let multiple_instance_times domain ?(nms = None) algs =
  let nms = (match nms with None -> algs | Some l -> l) in
  let all_attrs = Rdb.matching_attrs root ["domain", domain;
					   "alg", List.hd algs] in
  let all_attrs = List.flatten all_attrs in
  let problems = Rdb.filter_attrs ["domain"; "alg";] all_attrs in
  let name_norm = List.map (fun (_,nm) ->
				single_instance_times domain nm algs) problems in
  let data = List.fold_left (fun accum nm_data_list ->
			       List.map2 (fun (nm1,dla) (nm2,dl) ->
					    nm1, dl@dla) accum nm_data_list)
    (List.map (fun nm -> Some nm,[]) nms) name_norm in
    List.map (fun (nm_opt,dlist) ->
		let total = List.fold_left (+.) 0. dlist in
		  nm_opt, total /. (float (List.length dlist))) data



let loc = ref Legend.Lower_right
let lwidth = ref (Length.Pt 2.)


let do_single_plot ?(ylabel = "Solution Quality")
    ?(xlabel = "log10 raw cpu time") title data =
  let dsets = Line_dataset.line_datasets ~line_width:!lwidth ~color:true
    data in
  let plot = Num_by_num.plot
    ~legend_loc:!loc
    ~title ~xlabel ~ylabel dsets in
    plot#display


let do_avg_plot ?(ylabel = "Solution Quality")
    ?(xlabel = "log10 raw cpu time") title data =
  let dsets = Line_errbar_dataset.line_errbar_datasets
    ~line_width:!lwidth ~color:true data in
  let plot = Num_by_num.plot
    ~legend_loc:!loc
    ~title ~xlabel ~ylabel dsets in
    plot#display
