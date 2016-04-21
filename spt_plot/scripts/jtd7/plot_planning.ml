(**

    @author jordan
    @since 2011-12-04
*)

type dat = {
  alg : string;
  inst : int;
  xval : float;
  yval : float;
}

let root = "./research/data/planner"

let lwidth = ref (Length.Pt 2.)


let strs_of_file path =
  Array.of_list (List.rev (Wrio.with_infile path Wrio.input_lines))

(****** Grab the values you care about ******)

let get_wall_time strs =
  let wall_time_str = strs.(1) in
    try
      Scanf.sscanf wall_time_str "Total time: %fs" (fun f -> f)
    with _ -> 1800.

let get_search_time strs =
  let search_time_str = strs.(2) in
    try
      Scanf.sscanf search_time_str "Search time: %fs" (fun f -> f)
    with _ -> 1800.

let get_cost strs =
  let cost_str = strs.(3) in
    Scanf.sscanf cost_str "Plan cost: %f" (fun f -> f)

let get_length strs =
  let len_str = strs.(4) in
    Scanf.sscanf len_str "Plan length: %i step(s)." (fun f -> f)


let get_solved strs =
  let sf = Str.regexp_string "Solver timed out." in
  let to_ret = ref false in
    for i = 0 to (Array.length strs) - 1 do
      (try
	 (ignore(Str.search_forward sf strs.(i) 0);
	  to_ret := true)
       with _ -> ())
    done;
    if !to_ret then 0. else 1.


let eps = 0.

let entry_of_file ?(gyv = get_wall_time) path =
  let attrs = Rdb.attrs_for path in
  let name = List.assoc "alg" attrs
  and num = List.assoc "num" attrs
  and xv = List.assoc "cost_bound" attrs in
  let num = int_of_string num
  and xv = float_of_string xv in
  let yv = gyv (strs_of_file path) in
  let yv = if yv = 0. then eps else yv in
    { alg = name; inst = num; xval = xv; yval = yv }

(***** Do the drawing ******)
let sq_log y =
  (* sq is for scare quotes *)
  if y = 0. then -5. else log10 y


let to_points ?(ytrans = sq_log) ?(norms = None) (_, entries) =
  let entries = List.sort (fun e1 e2 -> compare e1.xval e2.xval) entries in
    match norms with
      |	None -> Array.of_list (List.map
				 (fun e -> Geometry.point ~x:e.xval
				    ~y:(ytrans e.yval))
				 entries)
      | Some (_, nentries) ->
	  (let nentries = (List.sort (fun e1 e2 -> compare e1.xval e2.xval)
			     nentries) in
	   let allowed_xs = List.map (fun e -> e.xval) entries in
	   let allowed_xs' = List.map (fun e-> e.xval) nentries in
	   let entries = List.filter (fun e -> List.mem e.xval allowed_xs') entries in
	   let nentries = List.filter (fun e -> List.mem e.xval allowed_xs) nentries in
	   let vals = (List.map2 (fun a b ->
				   assert (a.xval = b.xval);
				   Geometry.point ~x:a.xval
				     ~y:(ytrans (a.yval /. b.yval)))
			 entries nentries) in
	     Array.of_list vals)

let norm = ref false


let plot ?(xlabel = "Cost Bound") ?(ylabel = "log10 CPU Time Relative to PTS")
    ?(domain = "Zeno Travel") algs =
  let norm_alg = List.hd algs in
  let algs' = List.map (fun alg_list ->
			  assert (alg_list <> []);
			  let vals = List.map2 (fun a b ->
						  if !norm then to_points ~norms:(Some b) a
						  else to_points a)
			    alg_list norm_alg in
			  let name = Some ((fun (a,_) -> a)
					     (List.hd alg_list)) in
			    name, Array.of_list vals) algs in
  let dset = Line_errbar_dataset.line_errbar_datasets
    ~color:true ~line_width:!lwidth algs' in
  let plot = Num_by_num.plot
    ~title:domain
    ~legend_loc:Legend.Upper_right
    ~xlabel ~ylabel dset in
  plot#display


let sum_plot ?(xlabel = "Cost Bound") ?(ylabel = "Number of Solved Instances")
    ?(domain = "Zeno Travel") algs =
  let algs' = List.map
    (fun (dname,alg_list) ->
       let alg_list = List.map (List.sort (fun a b -> compare a.xval b.xval)) alg_list in
	 List.fold_left (fun accl elist ->
			   List.map2 (fun e1 e2 ->
					{ alg = dname;
					  inst = -1;
					  xval = e1.xval;
					  yval = e1.yval +. e2.yval})
			     accl elist) (List.hd alg_list) (List.tl alg_list))
    algs in
  let algs'' = List.map (fun alg_list ->
			   let nm = (List.hd alg_list).alg in
			     Some nm,
			   Array.of_list (List.map
					    (fun e ->
					       Verb.pe Verb.always "%s %f %f\n%!"
						 nm e.xval e.yval;
					       Geometry.point ~x:e.xval ~y:e.yval)
					    alg_list)) algs' in
  let dset = Line_dataset.line_datasets
    ~color:true ~line_width:!lwidth algs'' in
  let plot = Num_by_num.plot
    ~title:domain
    ~legend_loc:Legend.Upper_right
    ~xlabel ~ylabel dset in
    plot#display


(********* Plot calls ******)

let zenotravel ?(yfun = get_search_time) alg_list =
  let instances = List.map string_of_int (Wrlist.range ~min:0 19) in
    plot ~domain:"Zeno Travel"
    (List.map (fun (alg_name,disp_name, attrs) ->
      let attrs' = ["alg", alg_name; "domain", "zenotravel"] @ attrs in
      List.map (fun i ->
	let paths = Rdb.matching_paths root (("num", i)::attrs') in
	let vals = List.map entry_of_file paths in
	  disp_name, vals) instances) alg_list)


let elevators ?(yfun = get_search_time) alg_list =
  let instances = List.map string_of_int (Wrlist.range ~min:0 19) in
    plot ~domain:"Elevators"
    (List.map (fun (alg_name,disp_name, attrs) ->
      let attrs' = ["alg", alg_name; "domain", "elevators"] @ attrs in
      List.map (fun i ->
	let paths = Rdb.matching_paths root (("num", i)::attrs') in
	let vals = List.map entry_of_file paths in
	  disp_name, vals) instances) alg_list)


let elevators_solved ?(yfun = get_search_time) alg_list =
  let instances = List.map string_of_int (Wrlist.range ~min:0 19) in
  let to_plot = List.map
    (fun (alg_name,disp_name, attrs) ->
       let attrs' = ["alg", alg_name; "domain", "elevators"] @ attrs in
	 disp_name, List.map (fun i ->
		     let paths = Rdb.matching_paths root (("num", i)::attrs') in
(*		     let not_100 = Str.regexp "100." in
		     let paths' = List.filter (fun p ->
						 try (ignore(Str.search_forward not_100 p 0);
						      false) with _ -> true) paths in*)
		     let vals = List.map (entry_of_file ~gyv:get_solved) paths in
		       vals) instances) alg_list in
    sum_plot ~domain:"Elevators" to_plot


(*
let zenotravel_by_num ?(yfun = get_search_time) num alg_list =
  let algs' = List.map (fun (alg_name,disp_name,attrs) ->
    alg_name,disp_name, ("num", string_of_int num)::attrs) alg_list in
  zenotravel ~yfun algs'


let elevators ?(yfun = get_search_time) alg_list =
  let cost_bounds = List.map string_of_float
    [(*10_000.; 9000.; 8000.; 7000.; 6000.; 5000.; 4000.;*) 3000.; 2000.;
     1000.; 900.; 800.; 700.; 600.; 500.;] in
  plot ~domain:"Elevators"
    (List.map (fun (alg_name,disp_name, attrs) ->
      let attrs' = ["alg", alg_name; "domain", "elevators"] @ attrs in
      List.map (fun cb ->
	let paths = Rdb.matching_paths root (("cost_bound", cb)::attrs') in
	let vals = List.map entry_of_file paths in
	disp_name, vals) cost_bounds) alg_list)


let elevators_by_num ?(yfun = get_search_time) num alg_list =
  let algs' = List.map (fun (alg_name,disp_name,attrs) ->
    alg_name,disp_name, ("num", string_of_int num)::attrs) alg_list in
  elevators ~yfun algs'
*)
(* EOF *)
