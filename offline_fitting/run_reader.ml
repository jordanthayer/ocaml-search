(* Reads runs in *)

(* Function for parsing grid keys *)
let grid_key_parse gk =
  Scanf.sscanf gk "(%d,%d)" (fun a b -> (a,b))

let grid_key_print (x,y) =
  Wrutils.str "(%i, %i)" x y


let tiles_key_parse ek =
  let str_list = Str.split (Str.regexp ";") ek in
    List.map (fun s -> int_of_string (Wrstr.trim_white s)) str_list

let rec tiles_key_print num_list =
  match num_list with
    | [] -> ""
    | hd::tl -> (Wrutils.str "%i %s" hd (tiles_key_print tl))


let pancake_key_parse = tiles_key_parse
and pancake_key_print = tiles_key_print

let vacuum_key_parse vk =
  Scanf.sscanf vk "%d,(%d,%d)"
    (fun a b c -> a,b,c)

let vacuum_key_print (a,b,c) =
  Wrutils.str "%i,(%i,%i)" a b c



(********************************************************************)
(* Reading in truth files *)

type 'a truth = {
  true_key : 'a;
  true_heuristics : float list;
  admi_heuristics : float list;
}


(** h, then d is the order on heuristics in the lists *)
let proc_truth_line key_parse line max_h max_d =
  match (Wrstr.split_white line) with
      [key;true_h;true_d;admiss_h; admiss_d; rh; rd] ->
	(let key = key_parse key in
	   max_h := max !max_h (float_of_string admiss_h);
	   max_d := max !max_h (float_of_string admiss_d);
	   let ah = float_of_string admiss_h
	   and th = float_of_string true_h in
(*	     if ah > th
	     then (Verb.pe Verb.always "%f > %f\n" ah th;
		   assert false);*)

	   { true_key = key;
	     true_heuristics =
	       List.map float_of_string [true_h;true_d];
	     admi_heuristics =
	       List.map float_of_string [admiss_h;admiss_d;rh;rd];
	   })
    | _ -> failwith (Wrutils.str "truth line fail |%s|" line)


let proc_truth_file key_parse fn =
  let max_h = ref neg_infinity
  and max_d = ref neg_infinity in
  let truth = Hashtbl.create 10000 in
    Wrio.with_file_lines fn
      (fun l ->
	 try
	   let n = proc_truth_line key_parse l max_h max_d in
	     Hashtbl.add truth n.true_key n
	 with Failure str -> Verb.pe Verb.debug "%s\n%!" str);
    truth, !max_h, !max_d

(********************************************************************)
(* Reading Struct files *)
let proc_struct_line key_parse line =
  let keylist = Str.split (Str.regexp " ") line in
    match keylist with
      | index::keys ->
	  (let to_ret = Array.create (List.length keys)
	     (key_parse (List.hd keys))
	   in
	     ignore(List.fold_left (fun i e ->
				      try
					to_ret.(i) <- (key_parse e);
					i+1
				      with Scanf.Scan_failure str ->
					Verb.pe Verb.debug "bad: %s\n" line;
					failwith "Couldn't parse str file.")
		      0 keys);

	     (int_of_string index),to_ret)
      | _ -> failwith "No index?"



let proc_struct_file key_parse fn =
  let structure = Structure.new_struct () in
  Wrio.with_file_lines fn
    (fun l ->
       try
	 let ind,keys = proc_struct_line key_parse l in
	   Structure.add_step_raw structure ind keys
       with Failure str -> Verb.pe Verb.debug "%s\n%!" str);
    Structure.reverse_sequence structure;
    structure
(********************************************************************)
(* Reading Search Files *)
type 'a expansion = {
  exp_key : 'a;
  par_key : 'a;
  g: float;
  depth : int;
  cost: float;
  exp_children : 'a list;
}

let cost_parse c_st =
  Scanf.sscanf c_st "Cost: %f" (fun f -> f)


let proc_search_line key_parse line =
  match Str.split (Str.regexp "|") line with
      [step;key;pkey; g; depth; cost; children;] ->
	let step = int_of_string step
	and k = key_parse key
	and pk = key_parse pkey in
	  { exp_key = k;
	    par_key = pk;
	    g = float_of_string g;
	    depth = int_of_string depth;
	    cost = float_of_string cost;
	    exp_children = (List.map key_parse
			      (Str.split (Str.regexp " ") children));},(step,k)
    | [step;key;pkey; g; depth; cost;] ->
	let step = int_of_string step
	and k = key_parse key
	and pk = key_parse pkey in
	  { exp_key = k;
	    par_key = pk;
	    g = float_of_string g;
	    depth = int_of_string depth;
	    cost = float_of_string cost;
	    exp_children = [];},(step,k)
    | [key; pkey; g; depth; cost;] ->
	let k = key_parse key
	and pk = key_parse pkey in
	{ exp_key = k;
	  par_key = pk;
	  g = float_of_string g;
	  depth = int_of_string depth;
	  cost = float_of_string cost;
	  exp_children = [];},(-1,k)
    | _ -> failwith (Wrutils.str "Search parse fail >%s<" line)


(* Returns the nodes, the final solution cost, and the sequence of
   expansions *)
let proc_search_file key_parse fn =
  let exps = Hashtbl.create 10000
  and seq = ref [] in

    Wrio.with_file_lines fn
      (fun l ->
	 try
	   let ex,(s,k) = proc_search_line key_parse l in
	     if s > -1 then seq := (s,k)::!seq;
	     try
	       let prev = Hashtbl.find exps ex.exp_key in
		 if prev.exp_children = []
		 then Hashtbl.replace exps ex.exp_key ex
	     with Not_found -> Hashtbl.add exps ex.exp_key ex
	 with Failure str -> Verb.pe Verb.debug "%s\n%!" str);
    let sar = Array.of_list !seq in
      (Array.sort (fun (s1,_) (s2,_) -> s1 - s2) sar);
      exps,Array.map (fun (_,key) -> key) sar



(********************************************************************)
(* Putting things together *)
let node_gen tru exps =
  let max_depth = ref neg_infinity
  and max_g = ref neg_infinity
  and max_h = ref neg_infinity
  and max_d = ref neg_infinity in
  match tru with
    | Some (truths, mh, md) ->
	(let nodes =
	   Hashtbl.fold
	     (fun ex_k ex_d accum ->  (* ex_k -> key, ex_d -> data *)
		try
		  let tr = Hashtbl.find truths ex_k in
		    max_depth := max !max_depth (float_of_int ex_d.depth);
		    max_g := max !max_depth ex_d.g;
		    {Recorded_run.key = ex_k;
		     Recorded_run.parent = ex_d.par_key;
		     Recorded_run.truth = tr.true_heuristics;
		     Recorded_run.admiss = tr.admi_heuristics;
		     Recorded_run.g = ex_d.g;
		     Recorded_run.depth = (float_of_int ex_d.depth);
		     Recorded_run.cost = ex_d.cost;
		     Recorded_run.children = ex_d.exp_children;}::accum
		with Not_found ->
		  Verb.pe Verb.debug "Missing truth values in hashtbl\n%!";
		  accum) exps [] in
	   max_h := mh;
	   max_d := md;
	   Array.of_list nodes, !max_depth, !max_g, !max_h, !max_d)
    | None ->
	(let nodes =
	   Hashtbl.fold
	     (fun ex_k ex_d accum ->  (* ex_k -> key, ex_d -> data *)
		    max_depth := max !max_depth (float_of_int ex_d.depth);
		    max_g := max !max_depth ex_d.g;
		    {Recorded_run.key = ex_k;
		     Recorded_run.parent = ex_d.par_key;
		     Recorded_run.truth = [];
		     Recorded_run.admiss = [];
		     Recorded_run.g = ex_d.g;
		     Recorded_run.depth = (float_of_int ex_d.depth);
		     Recorded_run.cost = ex_d.cost;
		     Recorded_run.children = ex_d.exp_children;}::accum)
	     exps [] in
	   Array.of_list nodes, !max_depth, !max_g, neg_infinity, neg_infinity)


let make_run key_print sequence solution (nodes,mdepth,mg,mh,md) structs =
  let run = Hashtbl.create (Array.length nodes) in
    Array.iter
      (fun ele ->
	 try
	   let prev = Hashtbl.find run ele.Recorded_run.key in
	     (if prev.Recorded_run.children = []
		(* it wasn't the expanded node*)
	      then (if ele.Recorded_run.g < prev.Recorded_run.g
		    then Hashtbl.replace run ele.Recorded_run.key ele))
	 with Not_found ->
	   Hashtbl.add run ele.Recorded_run.key ele) nodes;
    { Recorded_run.sequence = sequence;
      Recorded_run.run = run;
      Recorded_run.sol_cost = 0.; (* not yet implemented *)
      Recorded_run.solution = solution; (* not yet implemented *)
      Recorded_run.structs = structs;
      Recorded_run.key_print = key_print;
      Recorded_run.max_g = mg;
      Recorded_run.max_depth = mdepth;
      Recorded_run.max_h = mh;
      Recorded_run.max_d = md;}

(* Need to rewrite data loaders *)

let grid_run ?(tr_file = "") ex_file str_file_list =
  Verb.pe Verb.toplvl "starting\n%!";
  let truths =
    (match tr_file with
	 "" -> None
       | _ -> Some (proc_truth_file grid_key_parse tr_file)) in
    Verb.pe Verb.toplvl "truth loaded\n%!";
    let exps,seq = proc_search_file grid_key_parse ex_file in
      Verb.pe Verb.toplvl "exps loaded\n%!";
      let str = List.map (proc_struct_file grid_key_parse) str_file_list in
	Verb.pe Verb.toplvl "Struct loaded\n%!";
	let nodes = node_gen truths exps in
	  make_run grid_key_print seq [] nodes str


let ep_run ?(tr_file = "data/eight_puzzle.truth") ?(tr_val = None)
    ex_file str_file_list =
  Verb.pe Verb.toplvl "starting\n%!";
  let truths =
    (match tr_file with
	 "" -> tr_val
       | _ -> Some (proc_truth_file tiles_key_parse tr_file)) in
    Verb.pe Verb.toplvl "truth loaded\n%!";
    let exps,seq = proc_search_file tiles_key_parse ex_file in
      Verb.pe Verb.toplvl "exps loaded\n%!";
      let str = List.map (proc_struct_file tiles_key_parse)
	str_file_list in
	Verb.pe Verb.toplvl "Struct loaded\n%!";
	let nodes = node_gen truths exps in
	  make_run tiles_key_print seq [] nodes str


let fp_run ?(tr_file = "")
    ex_file str_file_list =
  Verb.pe Verb.toplvl "starting\n%!";
  let truths =
    (match tr_file with
	 "" -> None
       | _ -> Some (proc_truth_file tiles_key_parse tr_file)) in
    Verb.pe Verb.toplvl "truth loaded\n%!";
    let exps,seq = proc_search_file tiles_key_parse ex_file in
      Verb.pe Verb.toplvl "exps loaded\n%!";
      let str = List.map (proc_struct_file tiles_key_parse)
	str_file_list in
	Verb.pe Verb.toplvl "Struct loaded\n%!";
	let nodes = node_gen truths exps in
	  make_run tiles_key_print seq [] nodes str


let vac_run ?(tr_file = "")
    ex_file str_file_list =
  Verb.pe Verb.toplvl "starting\n%!";
  let truths =
    (match tr_file with
	 "" -> None
       | _ -> Some (proc_truth_file vacuum_key_parse tr_file)) in
    Verb.pe Verb.toplvl "truth loaded\n%!";
    let exps,seq = proc_search_file vacuum_key_parse ex_file in
      Verb.pe Verb.toplvl "exps loaded\n%!";
      let str = List.map (proc_struct_file vacuum_key_parse)
	str_file_list in
	Verb.pe Verb.toplvl "Struct loaded\n%!";
	let nodes = node_gen truths exps in
	  make_run vacuum_key_print seq [] nodes str


let grid_run_thunk (truth, exp) () = grid_run ~tr_file:truth exp []
and vac_run_thunk (truth, exp) () = vac_run ~tr_file:truth exp []
and tile_run_thunk (truth, exp) () = ep_run ~tr_file:truth exp []


let make_tile_run_thunk tr_file =
  let truth = (proc_truth_file tiles_key_parse tr_file) in
    (fun  (_ , exp) () -> ep_run ~tr_file:"" ~tr_val:(Some truth) exp [])


let tr =
  [
    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/1.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/2.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/3.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/4.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/5.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/6.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/7.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/8.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/9.exhaust";

    "/var/tmp/offline_fitting/data/eight/truth",
    "/var/tmp/offline_fitting/data/eight/10.exhaust";

]

and gru =
  [
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_1.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_1.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_2.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_2.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_3.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_3.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_4.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_4.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_5.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_5.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_6.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_6.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_7.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_7.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_8.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_8.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_9.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_9.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_10.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Unit_10.exhaust";
  ]

and grl =
  [
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_1.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_1.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_2.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_2.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_3.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_3.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_4.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_4.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_5.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_5.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_6.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_6.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_7.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_7.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_8.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_8.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_9.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_9.exhaust";

    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_10.truth",
    "/var/tmp/offline_fitting/data/grid/200x200_0.35_Life_10.exhaust";
  ]


and vr =
  [
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_1.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_1.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_2.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_2.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_3.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_3.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_4.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_4.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_5.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_5.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_6.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_6.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_7.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_7.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_8.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_8.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_9.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_9.exhaust";

    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_10.truth",
    "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_10.exhaust";
  ]
(* EOF *)
