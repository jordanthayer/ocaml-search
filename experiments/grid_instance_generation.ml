(** Instance generation for grid problems *)

open Grid_runs

let make_high_obst_instances ?(w = 2000) ?(h = 1200) ?(n = 20) costs ways p =
  Random.self_init ();
  for i = 1 to n do
    (let attrs = [ "obstacles", "uniform";] @
       (batch_attrs ~tipe:"training" costs ways p) @
       [ "width", string_of_int w;
	 "height", string_of_int h;
	 "num", string_of_int i;] in
     let path = Rdb.path_for instance_root attrs in
       Verb.pe Verb.always "%s..." path;
       (** TODO: test to see if file is complete **)
       if Sys.file_exists path (*&& Datafile.run_finished path*)
       then Wrutils.pr "%s exists, skipping...\n%!" path
       else (let b = High_obstacles.gen i costs ways w h p in
	       Grid_instance.save path b;
	       Wrutils.pr "Wrote board %d.\n%!" i))
  done

let make_uniform_instances ?(w = 2000) ?(h = 1200) ?(n = 20) costs ways p =
  Random.self_init ();
  for i = 1 to n do
    (let attrs = [ "obstacles", "uniform";] @
       (batch_attrs (*~tipe:"training"*) costs ways p) @
       [ "width", string_of_int w;
	 "height", string_of_int h;
	 "num", string_of_int i;] in
     let path = Rdb.path_for instance_root attrs in
       Verb.pe Verb.always "%s..." path;
       (** TODO: test to see if file is complete **)
       if Sys.file_exists path
       then Wrutils.pr "%s exists, skipping...\n%!" path
       else (let b = (match ways with
			| Grid.Fourway -> Grid_instance.unit_four_board
			| Grid.Eightway -> Grid_instance.unit_eight_board)
	       (Experiments.feasible_p_dups Grid_algs.with_path6
		  Grid_interfaces.default_interface) w h p in
	     let b = { b with Grid.costs = costs} in
	       Grid_instance.save path b;
	       Wrutils.pr "Wrote board %d.\n%!" i))
  done

let mkseedinsts ?(w = 5000) ?(h = 5000) ?(n = 20) cost move pr =
  Random.self_init ();
  let attrs =
    ("obstacles", "uniform") :: (batch_attrs ~tipe:"seedinst" cost move pr) @
      [ "width", string_of_int w; "height", string_of_int h ] in
  for i = 1 to n do
    let attrs = attrs @ ["num", string_of_int i] in
    let path = Rdb.path_for instance_root attrs in
    Printf.printf "%s..." path;
    if Sys.file_exists path then
      Printf.printf "%s exists, skipping...\n%!" path
    else begin
      let sol brd =
	let x, _, _, _, _, _ =
	  Experiments.feasible_p_dups Grid_algs.with_path6
	    Grid_interfaces.default_interface brd () in
	x in
      let brd, seed = Grid_seedinst.feasible move cost sol w h pr in
      Grid_seedinst.save seed pr brd path;
      Wrutils.pr "Wrote board %d.\n%!" i
    end
  done

let make_ladder_instances ?(w = 2000) ?(h = 1200) () =
  let base_attrs = [ "obstacles", "ladder";
		     "width", string_of_int w;
		     "height", string_of_int h;] in
    for i = 1 to 20 do
      (let lw = h - (i * 50) in
       let path = (Rdb.path_for instance_root
		     (base_attrs @ ["ladder_width", string_of_int lw])) in
	 Verb.pe Verb.always "%s..." path;
	 (** TODO: test to see if file is complete **)
	 if Sys.file_exists path
	 then Wrutils.pr "%s exists, skipping...\n%!" path
	 else (Ladder_instance.save path (Ladder_instance.make h w lw) ;
	       Wrutils.pr "Wrote board %d.\n%!" i))
    done


let make_cup_instances ?(w = 2000) ?(h = 1200) () =
  let base_attrs = [ "obstacles", "cup";
		     "width", string_of_int w;
		     "height", string_of_int h;] in
  let path = Rdb.path_for instance_root (base_attrs @ ["num", "1"]) in
  Verb.pe Verb.always "%s..." path;
   (** TODO: test to see if file is complete **)
  if Sys.file_exists path
  then Wrutils.pr "%s exists, skipping...\n%!" path
  else (Ladder_instance.save path (Single_depression.make 1 h w) ;
	Wrutils.pr "Wrote board %d.\n%!" 1)


let make_mazes ?(w = 1000) ?(h = 1000) ?(p = 0.05) n =
  Random.self_init ();
  for i = 1 to n do
    let attrs = maze_attrs w h p @ ["num", string_of_int i] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.run_finished path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Mazes.make_maze_dfs p w h in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let scaled_mazes ?(w = 1000) ?(h = 1000) ?(p = 0.05) ?(hw = 2) n =
  Random.self_init ();
  for i = 1 to n do
    let attrs = scaled_maze_attrs w h p hw @ ["num", string_of_int i] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.run_finished path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Mazes.make_maze_dfs p w h in
	  Verb.pe Verb.always "Scaling Instance\n";
	let sb = Mazes.scale_maze b hw in
	  Grid_instance.save path sb;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let make_line_instances ?(w = 2000) ?(h = 1200) ?(n = 20) costs ways lines =
  Random.self_init ();
  for i = 1 to n do
    let attrs = [ "obstacles", "lines"; "type", "instance";] @
      (line_attrs costs ways lines) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path
      then Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = LineSegGrid.feasible_board validator w h costs ways lines in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let make_game_instances path brd_name =
  let rec write_bucket i b list =
    match list with
	[] -> ()
      | h::tl ->
	  (let attrs = (game_attrs brd_name (string_of_int (100 + b * 10))) @
			 ["num", string_of_int i;] in
	   let path = Rdb.path_for instance_root attrs in
	     if Sys.file_exists path && Datafile.seems_complete path then
	       Wrutils.pr "%s exists, skipping...\n%!" path
	     else
	       (Grid_instance.save path h.GameGrid.b;
		Wrutils.pr "Wrote board\n%!";
		write_bucket (i+1) b tl)) in
    Random.self_init ();
    let bar = GameGrid.load_bucket validator path 100 200 10 40 in
      Verb.pe Verb.toplvl "bar loaded\n%!";
      for i = 0 to ((Array.length bar.GameGrid.bar) - 1)
      do
	write_bucket 0 i bar.GameGrid.bar.(i)
      done


let make_fshad_instances ?(w = 2000) ?(h = 1200) ?(n = 20) costs ways prob =
  Random.self_init ();
  for i = 1 to n do
    let attrs = [ "obstacles", "uniform_region"; "type", "instance";] @
      (batch_attrs costs ways prob) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Grid_demo_fshadow.feasible_board validator w h prob costs ways
	in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let make_many_instances ?(w = 1000) ?(h = 1000) ?(n = 20) costs ways
    start_p end_p goals =
  Random.self_init ();
  for i = 1 to n do
    let attrs = (mg_attrs costs ways start_p end_p goals) @
      [ "width", string_of_int w;
	"height", string_of_int h;
	"num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Many_goal.feasible_board validator w h costs ways
	  start_p end_p goals in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let make_bench_instance1 ?(w = 8000) ?(h = 4800) ?(n = 1) costs ways p =
  Random.self_init ();
  for i = 1 to n do
    let attrs = ["obstacles", "uniform"; "type", "instance";] @
      (batch_attrs costs ways p) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Grid_instance.feasible_board validator w h p costs ways in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done
(* EOF *)
