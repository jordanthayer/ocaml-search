(**

   @author jtd7
   @since 2011-03-08

   Bootstrapping iteratively runs through a batch of instances with a
   time cutoff
*)


let bootstrap_iteration make_sface solver instances timeout =
  let solutions = (List.map
		    (fun (path,instance) ->
		       solver (make_sface path instance
				 [Limit.Time timeout; Limit.MachineMemory]))
		    instances) in
  let remain,train = (List.fold_left2
			(fun (pac,sac) sol path ->
			   match sol with
			     | None -> (path::pac),sac
			     | Some i -> pac,  i::sac)
			([],[]) solutions instances) in
    remain, train


let bootstrap ?(ins_min = 1) ?(tmax = infinity) make_sface do_train solver
    make_instance load_instance instance_root base_attrs start_timeout =
  Verb.pe Verb.always "Fetching paths\n%!";
  let paths = Rdb.matching_paths instance_root base_attrs in
  Verb.pe Verb.always "Fetching instances\n%!";
  let all_instances = List.map (fun p -> p, load_instance p) paths in

  let rec do_it it train_acc timeout instances =
    let timeout = Math.fmin timeout tmax in
      Datafile.write_alt_row_prefix stdout "bootstrap_iteration";
      Verb.pr Verb.always "%i\t%f\t%i\n%!" it timeout (List.length instances);
      Verb.pe Verb.always "%i\t%f\t%i\n%!" it timeout (List.length instances);
      if instances = [] then ()
      else
	let remain, train = (bootstrap_iteration make_sface solver
			       instances timeout) in
	let solved_this_it = List.length train
	and solved_last_it = List.length train_acc in
	let solved = solved_this_it + solved_last_it in
	  if solved >= ins_min (* ts sufficiently large*)
	  then (Verb.pe Verb.always "Doing Training\n%!";
		List.iter do_train (train_acc @ train);
		do_it (it + 1) [] timeout remain)
	  else (if timeout < tmax
		then do_it (it + 1) (train @ train_acc) (timeout *. 2.) remain
		else (let to_make = Wrlist.range (ins_min - solved) in
		      let to_make = List.map (fun _ -> -1) to_make in
		      let new_instances = List.map make_instance to_make in
			(* we assume the bootstrapping instances are solvable
			   with the current heuristics, so the timeout is
			   infinite *)
		      let remain', train' = (bootstrap_iteration make_sface
					       solver new_instances infinity) in
			assert (to_make <> []);
			Verb.pe Verb.toplvl
			  "%i bootstrap instances solved, %i attempted\n%!"
			  (List.length train')
			  (List.length new_instances);
			List.iter do_train (train @ train_acc @ train');
			do_it (it + 1) [] tmax remain)) in

    Datafile.write_alt_colnames stdout "bootstrap_iteration"
      ["iteration"; "timeout"; "remaining"];
  Verb.pe Verb.always "Starting runs\n%!";
    do_it 0 [] start_timeout all_instances



let domain_to_iroot_attrs = function
  | Search_interface.UGrid -> "./group/data/grid_instances",
      ["obstacles", "uniform";
       "type", "instance";
       "costs", "Unit";
       "moves", "Four-way";
       "prob", "0.35";
       "width", "2000";
       "height", "1200"; ]
  | Search_interface.LGrid -> "./group/data/grid_instances",
      ["obstacles", "uniform";
       "type", "instance";
       "costs", "Life";
       "moves", "Four-way";
       "prob", "0.35";
       "width", "2000";
       "height", "1200"; ]
  | Search_interface.Tiles -> "./group/data/tiles_instances",
      ["model", "boot_aij";
       "rows", "5";
       "cols", "5";]
  | _ -> failwith "Haven't handled that domain yet"


let make_logger ?(time = (Sys.time())) get_cost get_length =
  Datafile.write_colnames stdout
    ["instance"; "sol cost"; "sol length" ;"nodes expanded";
     "nodes generated"; "duplicates encountered"; "raw cpu time"];
  let start = time in
    (fun inum i ->
       if inum >= 0 then
	 (let t = (Sys.time ()) -. start in
	    match i.Limit.incumbent with
		Limit.Nothing -> failwith "Logging without an incumbent?"
	      | Limit.Incumbent (q,inc) ->
		  Verb.pr Verb.always "%i\t%f\t%d\t%d\t%d\t%d\t%f\n" inum
		    (get_cost inc) (get_length inc) i.Limit.expanded
		    i.Limit.generated i.Limit.duplicates t))

(* EOF *)
