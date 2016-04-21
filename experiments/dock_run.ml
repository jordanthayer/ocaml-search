(**

    @author jtd7
    @since 2011-01-21
*)

open Printf

let instance_root =
  Filename.concat Experiments.instance_root "dock_robot"

let solver_binary =
  ref (Filename.concat Experiments.bin_root "dock_robot_solver")

let call_solver limits args prob_path attrs outch =
  (** [call_solver limits args prob_path attrs outch] calls the
      external solver binary with [args] and the given limits as the
      arguments and [prob_path] is piped into its input.  The output
      is redirected to [outch]. *)
  let limit_args = Limit.to_string limits in
  let cmd = sprintf "%s --memory max %s %s < %s"
    !solver_binary limit_args args prob_path in
    Verb.pe Verb.debug "Command: %s\n%!" cmd;
  let run cmd () =
    Wrsys.with_subprocess_pipes cmd
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch [ "attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res);
  in
    Notify.try_and_email_on_fail (run cmd) "Sliding_tiles_runs"


let do_run ?(lazy_run=true) overwrite limits args attrs prob_path =
  (** [do_run overwrite limits macro args attrs prob_path] performs a
      single run on the given problem.  The results are stored in the
      RDB specified by data_root.

      the lazy_run optional parameter makes it so that if the file
      already exists and looks done, it just skips it no questions
      asked.

  *)
  let data_root = Filename.concat Experiments.data_root "dock_robot" in
  let res_file = Rdb.path_for data_root attrs in
    if Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && not (Limit.has_new_limits res_file limits)
      && not overwrite
    then printf "%s exists -- skipping!\n%!" res_file
    else if
      lazy_run
      && Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && (let df = Datafile.load res_file in
	  let found_solution = Datafile.get_val df "found solution" in
	    found_solution = "yes")
    then printf "%s exists -- skipping (lazy mode)!\n%!" res_file
    else begin
      Wrio.with_outfile res_file
	(fun outch ->
	   let rel_path = Wrfname.make_relative instance_root prob_path in
	     printf "Running %s on %s...%!" args rel_path;
	     let c, t = call_solver limits args prob_path attrs outch in
	       printf "%.1f in %.3f.\n%!" c t)
    end


let do_batch ?(lazy_run = false) ?(send_mail=true) ?(overwrite=false)
    ?(limits=[Limit.Time 600.]) ?(ncranes = 3) ?(containers = 15) ?(npiles = 3)
    ?(nrobots = 1) ?(nlocs = 3) ?(model = "usquare") args alg_attrs =
  let batch_attrs = [ "ncranes", string_of_int ncranes;
		      "npiles", string_of_int npiles;
		      "nrobots", string_of_int nrobots;
		      "nlocations", string_of_int nlocs;
		      "ncontainers", string_of_int containers;
		      "model", model]  in

  let attr_sets = Rdb.matching_attrs instance_root batch_attrs in
    Verb.pe Verb.always "%i instances\n%!" (List.length attr_sets);
    if(send_mail) then Notify.start_batchtime ();

    List.iter (fun inst_attrs ->
		 let attrs = alg_attrs @ inst_attrs in
		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   do_run ~lazy_run:lazy_run
		     overwrite limits args attrs prob_path) attr_sets;
    if(send_mail)
    then Notify.send_batch_completed_mail "Dock Robot Runs"
      (snd (List.hd alg_attrs)) args


let basic_batch ?(append = "") alg_name =
 do_batch (sprintf "--iface sum_deep %s%s" alg_name append)
   ["alg", alg_name;
    "heuristic", "sum_deep";]


let do_wted_batch alg_name weight =
 do_batch (sprintf "--iface sum_deep %s %f" alg_name weight)
   ["alg", alg_name;
    "wt", string_of_float weight;
    "heuristic", "sum_deep";]


let do_beam_batch ?(append = "") alg_name width =
 do_batch (sprintf "--iface sum_deep %s %i%s" alg_name width append)
   ["alg", alg_name;
    "beam_width", string_of_int width;
    "heuristic", "sum_deep";]


let do_wted_batches ?(wts = Experiments.low_res_weights) alg_name =
  List.iter (do_wted_batch alg_name) (List.rev (List.filter (fun w -> w >= 1.2)
						  wts))


let do_beam_batches ?(beams = Experiments.reduced_beams) ?(append = "")
    alg_name =
  List.iter (do_beam_batch ~append alg_name) beams


let do_aggressive_batches ?(wts = Experiments.low_res_weights) opt alg_name =
  List.iter (fun wt ->
	       do_batch (sprintf "--iface sum_deep %s %f %f" alg_name
			   wt opt)
		 ["alg", alg_name;
		  "wt", string_of_float wt;
		  "opt", string_of_float opt;
		  "heuristic", "sum_deep"])
    (List.filter (fun w -> w >= 1.2) wts)


let do_deadline_contract_batch alg_name =
  let deadlines =
    [60.; 30.; 15.; 7.5; 3.75; 1.875; 0.9375; 0.46875; 0.234375;
     0.1171875; 0.05859375; 0.029296875; 0.0146484375;
     0.00732421875;] in
  let w_res = List.map (fun a -> a,1500) deadlines in
    List.iter (fun (dl,res) ->
	       do_batch ~limits:[Limit.Time dl]
		 (sprintf "--iface sum_deep %s %f %i" alg_name dl res)
		 ["alg", alg_name;
		  "deadline", string_of_float dl;
		  "res", string_of_int res;]) w_res


let model_on_instance alg =
  let base_instance_attrs = [
			     "model", "usquare";
			     "heuristic", "sum_deep";
			     "ncranes", "3";
			     "ncontainer", "10";
			     "npiles", "3";
			     "nrobots", "1";
			     "nlocations", "3";
			     "alg", "greedier_adapt";] in
    for i = 1 to 250 do
      (let iat = ("num", string_of_int i)::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"dock_robot" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false [Limit.Time 600.]
	   (Wrutils.str " %s %f %f" alg herr derr)
	   ["model", "usquare";
	    "heuristic", "sum_deep";
	    "ncranes", "3";
	    "ncontainer", "10";
	    "npiles", "3";
	    "nrobots", "1";
	    "nlocations", "3";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "this_instance")]
	   (Wrutils.str
	      "./group/data/dock_robot/usquare/3/10/3/1/3/%i" i)
      )
    done


let model_on_next_instance alg =
  let base_instance_attrs = ["model", "usquare";
			     "heuristic", "sum_deep";
			     "ncranes", "3";
			     "ncontainer", "10";
			     "npiles", "3";
			     "nrobots", "1";
			     "nlocations", "3";
			     "alg", "greedier_adapt";] in
    for i = 1 to 250 do
      (let iat = ("num", string_of_int (i+1 mod 250))::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"dock_robot" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false [Limit.Time 600.]
	   (Wrutils.str " %s %f %f"
	      alg herr derr)
	   ["model", "usquare";
	    "heuristic", "sum_deep";
	    "ncranes", "3";
	    "ncontainer", "10";
	    "npiles", "3";
	    "nrobots", "1";
	    "nlocations", "3";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "this_instance")]
	   (Wrutils.str
	      "./group/data/dock_robot/usquare/3/10/3/1/3/%i" i)
      )
    done

(* EOF *)
