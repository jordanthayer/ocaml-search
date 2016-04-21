(**

    @author jtd7
    @since 2010-10-25

   Script for gathering truth data for various problems

*)

let data_root = "/var/tmp/offline_fitting/data"
and vbinary = ".research/experiments/bins/vacuum_solver"
and gbinary = ".research/experiments/bins/grid_path_solver"
and tbinary = ".research/experiments/bins/tiles_solver"


let get_vacuum_truth ?(w = 200) ?(h = 200) ?(p = 0.35) ?(d = 5) () =
  let get_attrs num = [ "obstacles", "uniform";
			"width", string_of_int w;
			"height", string_of_int h;
			"obst_prob", string_of_float p;
			"dirt", string_of_int d;
		        "num", string_of_int num ] in
  let path num = Rdb.path_for Vacuum_runs.instance_root (get_attrs num)
  and out_str n = (Wrutils.str "%s/vacuum/%ix%i_%.2f_%i_%i"
		     data_root w h p d n) in
    for i = 1 to 20 do
      (let ipath = path i
       and out_base = out_str i in
	 if not (Sys.file_exists (Wrutils.str "%s.truth" out_base))
	 then
	   (ignore
	      (Sys.command
		 (Wrutils.str
		    "%s --iface reverse_spanning truth < %s 2> %s.truth"
		    vbinary ipath out_base));
	    ignore
	      (Sys.command
		 (Wrutils.str
		    "%s --iface exhaust_spanning record_astar < %s 2> %s.exhaust"
		    vbinary ipath out_base))))
    done


let get_grid_truth  ?(w = 200) ?(h = 200) ?(p = 0.35) ?(c = "Unit") () =
  let get_attrs num = [ "obstacles", "uniform";
			"type", "instance";
			"width", string_of_int w;
			"height", string_of_int h;
			"costs", c;
			"prob", string_of_float p;
			"moves", "Four-way";
		        "num", string_of_int num ] in
  let path num = Rdb.path_for Grid_runs.instance_root (get_attrs num)
  and out_str n = (Wrutils.str "%s/grid/%ix%i_%.2f_%s_%i"
		     data_root w h p c n) in
    for i = 1 to 20 do
      (let ipath = path i
       and out_base = out_str i in
	 if not (Sys.file_exists (Wrutils.str "%s.truth" out_base))
	 then
	   (ignore
	      (Sys.command
		 (Wrutils.str
		    "%s truth < %s 2> %s.truth"
		    gbinary ipath out_base));
	    ignore
	      (Sys.command
		 (Wrutils.str
		    "%s record_astar < %s 2> %s.exhaust"
		    gbinary ipath out_base))))
    done


let get_tiles_truth () =
  let get_attrs () = [ "rows", "3";
		       "cols", "3";
		       "bucket", string_of_int (Random.int 499); ] in
  let path () = (Wrlist.random_elt
		   (Rdb.matching_paths Tiles_runs.instance_root (get_attrs ())))
  and out_str n = (Wrutils.str "%s/tiles/3x3_%i" data_root n) in
    for i = 1 to 20 do
      (let ipath = path ()
       and out_base = out_str i in
	 if not (Sys.file_exists (Wrutils.str "%s.truth" out_base))
	 then
	   (ignore
	      (Sys.command
		 (Wrutils.str
		    "%s -v 4 --iface record_truth truth < %s 2> %s.truth"
		    tbinary ipath out_base));
	    ignore
	      (Sys.command
		 (Wrutils.str
		    "%s -v 4 --iface exhaustive record_astar < %s 2> %s.exhaust"
		    tbinary ipath out_base))))
    done
(* EOF *)
