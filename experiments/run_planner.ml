(**

    @author jtd7
    @since 2011-11-20
*)

let data_root = Experiments.data_root ^ "planner"
and instance_root = "planning3"
and solver_binary = ref (Experiments.bin_root ^ "downward")
and dry_run = ref true
and instance_attributes' = ["type", "instance"]
and instance_attributes = ["file", "output"]


let get_instance_attrs domain =
  let attrs = match domain with
    | None -> instance_attributes
    | Some s -> ["domain", s] @ instance_attributes in
    attrs


let make_alg_string alg_name parm_list timeout =
  (* the leading , isn't a problem apparently *)
  let string =
    List.fold_left (fun accum el -> Printf.sprintf "%s, %s" accum el)
      (Printf.sprintf "--search \"%s(" alg_name) parm_list in
    match timeout with
	None -> string ^ ")\""
      | Some t -> Printf.sprintf "%s, time=%f)\"" string t


let make_call_solver_string search_string p_path =
  Printf.sprintf "%s %s < %s" !solver_binary search_string p_path


let make_rwa_string time =
  (* at some point maybe you want to parameterize the weight list *)
    Printf.sprintf "%s %s[%s,%s,%s,%s,%s], %s, time=%f)\""
      "--heuristic \"lm=lmcut()\""
      "--search \"iterated("
      "lazy_greedy(lm)"
      "wted_astar(lm,5)"
      "wted_astar(lm,3)"
      "wted_astar(lm,2)"
      "astar(lm)"
      "repeat_last=true,continue_on_fail=true"
      time

let do_run problem_attrs search_attrs search_string =
  let problem_paths = Rdb.matching_paths instance_root problem_attrs in
    List.iter (fun path ->
		 Verb.pe Verb.debug "Working on path %s\n%!" path;
		 let p_attrs = Rdb.attrs_for path in
		 let p_attrs = List.rev p_attrs in
		 let filt = Rdb.filter_attrs ["file"] p_attrs in
		 let search_attrs' = search_attrs @ filt in
		 let output_file = Rdb.path_for data_root search_attrs' in
		 let solver = make_call_solver_string search_string path in
		 let cmd = Printf.sprintf "%s > %s" solver output_file in
		 let dom = List.assoc "domain" p_attrs
		 and alg = List.assoc "alg" search_attrs' in
		 let dir = Filename.dirname path in
		   Verb.pr Verb.debug "Changing to directory |%s|\n%!" dir;
		   Sys.chdir dir;
		   Verb.pr Verb.debug "Running %s\n%!" cmd;
		   Verb.pr Verb.always "Running %s on %s\n%!" alg dom;
		   if not !dry_run
		   then ignore (Wrsys.shell_output cmd);
		   ignore (Wrsys.shell_output "rm sas_plan*");
		   ignore (Wrsys.shell_output "rm plan_numbers_and_cost"))
      problem_paths


let strip_prens =
  let pren_regexp = Str.regexp "[()]" in
    Str.global_replace pren_regexp "_"


(****************** Some bounded suboptimal search runs **********************)

let do_ees_run ?(domain = None) ?(h = "lmcut") ?(hhat = "ff")
    ?(dhat = "ff(cost_type=1)") ?(timeout = Some 300.) bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "ees";
		      "h", strip_prens h;
		      "hhat", strip_prens hhat;
		      "dhat", strip_prens dhat;
		      "suboptimality_bound", bound;] in
  let search_string = make_alg_string "ees" [h; hhat; dhat; bound] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: EES h: %s hhat: %s dhat:%s" h hhat dhat))


let do_wted_astar ?(domain = None) ?(h = "lmcut") ?(timeout = Some 300.) bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "wted_astar";
		      "h", strip_prens h;
		      "suboptimality_bound", bound;] in
  let search_string = make_alg_string "wted_astar" [h;bound] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: wA* h: %s" h))


let do_aseps ?(domain = None) ?(h = "lmcut") ?(d = "ff(cost_type=1)")
    ?(timeout = Some 300.) bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "aseps";
		      "h", strip_prens h;
		      "d", strip_prens d;
		      "suboptimality_bound", bound;] in
  let search_string = make_alg_string "aseps" [h;d;bound] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: A*eps h: %s d: %s" h d))


let do_skeptical ?(domain = None) ?(h = "lmcut") ?(hhat = "ff")
    ?(timeout = Some 300.) bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "skeptical";
		      "h", strip_prens h;
		      "hhat", strip_prens hhat;
		      "suboptimality_bound", bound;] in
  let search_string = make_alg_string "skeptical" [h; hhat; bound] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: skeptical h: %s hhat: %s" h hhat))


let do_optimistic ?(domain = None) ?(h = "lmcut")
    ?(timeout = Some 300.) optimism bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "optimistic";
		      "h", strip_prens h;
		      "optimism", optimism;
		      "suboptimality_bound", bound;] in
  let h_string = Printf.sprintf "--heuristic \"h=%s()\"" h
  and alg_string = (Printf.sprintf "--search \"skeptical(h,weight(h,%s),%s)\""
		      optimism bound) in
  let search_string = Printf.sprintf "%s %s" h_string alg_string in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: optimistic h: %s optimism: %s" h optimism))


let do_wted_batch ?(domain = None) alg_fun alg_name =
  (* this should probably be replaced by a more generic do batch caller *)
  Notify.start_metatime();
  List.iter alg_fun (List.map string_of_float Experiments.full_weights);
  if not !dry_run then Notify.send_metarun_completed_mail "Planning" alg_name


let do_optimistic_batch ?(domain = None) alg_fun alg_name =
  (* this should probably be replaced by a more generic do batch caller *)
  Notify.start_metatime();
  List.iter (fun opt ->
	       List.iter (alg_fun opt)
		 (List.map string_of_float Experiments.full_weights))
    (List.map string_of_float Experiments.optimisms);
  if not !dry_run then Notify.send_metarun_completed_mail "Planning" alg_name


(************************* Some bounded cost runs **************************)


let do_potential_run ?(domain = None) ?(h = "lmcut") ?(hhat = None)
    ?(timeout = Some 1800.) bound =
  (* consider separating out the potential inadmiss from potential in here at
     some point, as it is, this is a bit confusing *)
  let problem_attrs = get_instance_attrs domain in
  let alg_name =  (match hhat with
		     | None -> "potential"
		     | Some _ -> "potential_inadmiss" ) in
  let search_attrs = ["alg", alg_name;
		      "h", strip_prens h;
		      "cost_bound", bound;] in
  let search_attrs = (match hhat with None -> search_attrs
			| Some heur -> search_attrs @ ["hhat", heur]) in
  let search_string = (match hhat with
			 | None -> make_alg_string alg_name [h; bound] timeout
			 | Some hh -> make_alg_string alg_name [h; hh; bound]
			     timeout) in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: %s" alg_name))


let do_speedy_bc ?(domain = None) ?(h = "lmcut") ?(d = "ff(cost_type=1)")
    ?(timeout = Some 1800.) bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "speedy_bc";
		      "h", strip_prens h;
		      "d", strip_prens d;
		      "cost_bound", bound;] in
  let search_string = make_alg_string "bc_baseline" [h; d; bound] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound) "Alg: bc_baseline (speedy_bc)")


let do_bees ?(domain = None) ?(h = "lmcut") ?(hhat = "ff")
    ?(dhat = "ff(cost_type=1)") ?(timeout = Some 1800.) bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "bees";
		      "h", strip_prens h;
		      "hhat", strip_prens hhat;
		      "dhat", strip_prens dhat;
		      "cost_bound", bound;] in
  let search_string = make_alg_string "bees" [h; hhat; dhat; bound] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: BEES h: %s hhat: %s dhat:%s" h hhat dhat))


let do_beeps ?(domain = None) ?(h = "lmcut") ?(hhat = "ff")
    ?(dhat = "ff(cost_type=1)") ?(timeout = Some 1800.) bound =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "beeps";
		      "h", strip_prens h;
		      "hhat", strip_prens hhat;
		      "dhat", strip_prens dhat;
		      "cost_bound", bound;] in
  let search_string = make_alg_string "bees" [h; hhat; dhat; bound] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %s" bound)
	    (Printf.sprintf "Alg: BEEPS h: %s hhat: %s dhat:%s" h hhat dhat))



let do_rwa_batch ?(domain = None) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "restarts_wA*";
		     "h", "lmcut";
		     "wts", "[5,3,2,1]";] in
    (* consider adding timout here later?*)
  let search_string = make_rwa_string timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "RwA*" ""


let do_awa_batch ?(domain = None) ?(timeout = (Some 1800.)) wt =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "anytime_astar";
		      "h", "lmcut";] in
    (* consider adding timout here later?*)
  let search_string = make_alg_string "wted_astar"
    ["lmcut"; wt; "keep_going=true";] timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "AwA*" ""


let lama11_uc timeout =
  "--heuristic \"hlm,hff=lm_ff_syn(lm_rhw" ^
    "(reasonable_orders=true,lm_cost_type=2,cost_type=2))\" " ^
    "--search \"iterated(["^
    "lazy_greedy([hff,hlm],preferred=[hff,hlm]),"^
    "lazy_wastar([hff,hlm],preferred=[hff,hlm],w=5),"^
    "lazy_wastar([hff,hlm],preferred=[hff,hlm],w=3),"^
    "lazy_wastar([hff,hlm],preferred=[hff,hlm],w=2),"^
    "lazy_wastar([hff,hlm],preferred=[hff,hlm],w=1)],"^
    (Printf.sprintf "repeat_last=true,continue_on_fail=true, time=%f)\""
       timeout)

let lama11_nuc timeout =
  "--heuristic \"hlm1,hff1=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=1,cost_type=1))\" " ^
    "--heuristic \"hlm2,hff2=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=2,cost_type=2))\" " ^
    "--search \"iterated([" ^
    "lazy_greedy([hff1,hlm1],preferred=[hff1,hlm1]," ^
    "cost_type=1,reopen_closed=false)," ^
    "lazy_greedy([hff2,hlm2],preferred=[hff2,hlm2]," ^
    "reopen_closed=false)," ^
    "lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=5)," ^
    "lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=3)," ^
    "lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=2)," ^
    "lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=1)]," ^
    (Printf.sprintf "repeat_last=true,continue_on_fail=true, time=%f)\""
       timeout)


let eager_lama11_nuc timeout =
  "--heuristic \"hlm1,hff1=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=1,cost_type=1))\" " ^
    "--heuristic \"hlm2,hff2=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=2,cost_type=2))\" " ^
    "--search \"iterated([" ^
    "eager_greedy([hff1,hlm1],preferred=[hff1,hlm1]," ^
    "cost_type=1,reopen_closed=false)," ^
    "eager_greedy([hff2,hlm2],preferred=[hff2,hlm2]," ^
    "reopen_closed=false)," ^
    "wted_astar([hff2,hlm2],preferred=[hff2,hlm2],w=5)," ^
    "wted_astar([hff2,hlm2],preferred=[hff2,hlm2],w=3)," ^
    "wted_astar([hff2,hlm2],preferred=[hff2,hlm2],w=2)," ^
    "wted_astar([hff2,hlm2],preferred=[hff2,hlm2],w=1)]," ^
    (Printf.sprintf "repeat_last=true,continue_on_fail=true, time=%f)\""
       timeout)



let do_lama11_batch ?(domain = None) ?(uc = false) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "lama11";] in
    (* consider adding timout here later?*)
  let search_string = (if uc then lama11_uc else lama11_nuc) timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "Lama11" ""


let do_eager_lama11_batch ?(domain = None) ?(uc = false) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "eager_lama11";] in
    (* consider adding timout here later?*)
  let search_string = (if uc then eager_lama11_nuc else eager_lama11_nuc) timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "Lama11" ""


let eager_lama11_nopref timeout =
  "--heuristic \"hlm1,hff1=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=1,cost_type=1))\" " ^
    "--heuristic \"hlm2,hff2=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=2,cost_type=2))\" " ^
    "--search \"iterated([" ^
    "eager_greedy([hff1,hlm1]," ^
    "cost_type=1,reopen_closed=false)," ^
    "eager_greedy([hff2,hlm2]" ^
    "reopen_closed=false)," ^
    "wted_astar([hff2,hlm2],w=5)," ^
    "wted_astar([hff2,hlm2],w=3)," ^
    "wted_astar([hff2,hlm2],w=2)," ^
    "wted_astar([hff2,hlm2],w=1)]," ^
    (Printf.sprintf "repeat_last=true,continue_on_fail=true, time=%f)\""
       timeout)


let eager_speedy timeout =
  "--heuristic \"hlm1,hff1=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=1,cost_type=1))\" " ^
    (Printf.sprintf "--search eager_greedy(hff1, cost_type=1, time=%f)"
       timeout)

let do_eager_speedy_batch
    ?(domain = None) ?(uc = false) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "speedy";] in
    (* consider adding timout here later?*)
  let search_string = eager_speedy timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "Eager Lama11" "Nopref"


let eager_speediest timeout =
  "--heuristic \"hlm1,hff1=lm_ff_syn(lm_rhw(" ^
    "reasonable_orders=true,lm_cost_type=1,cost_type=1))\" " ^
    (Printf.sprintf "--search greediest(hff1, cost_type=1, time=%f)"
       timeout)

let do_eager_speedy_batch
    ?(domain = None) ?(uc = false) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "speediest";] in
    (* consider adding timout here later?*)
  let search_string = eager_speediest timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "Eager Lama11" "Nopref"


let do_eager_lama11_np_batch
    ?(domain = None) ?(uc = false) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "eager_lama11_nopref";] in
    (* consider adding timout here later?*)
  let search_string = eager_lama11_nopref timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "Eager Lama11" "Nopref"


let aees_string ?(s = "aees") timeout =
  (Printf.sprintf "--search \"%s(lmcut(), lmcount(lm_rhw(reasonable_orders=true))," s) ^ (Printf.sprintf "ff(cost_type=ONE), time=%f)\"" timeout)


let do_aees_batch ?(s = "aees") ?(domain = None) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", s;] in
    (* consider adding timout here later?*)
  let search_string = aees_string ~s timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" s "AEES"


let ana_string timeout =
  Printf.sprintf "--search \"nonparametric_astar(lmcut(),time=%f)\"" timeout


let do_ana_batch ?(domain = None) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "anastar";] in
    (* consider adding timout here later?*)
  let search_string = ana_string timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "ANA*" "ANA*"


let ara_string timeout =
  (Printf.sprintf
     "--search \"arastar(lmcut(), schedule=[1,1.5,2,3,5],time=%f)\"" timeout)


let do_ara_batch ?(domain = None) ?(timeout = 1800.) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "arastar";] in
    (* consider adding timout here later?*)
  let search_string = ara_string timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then Notify.send_batch_completed_mail "Planning" "ARA*" "ARA*"


let do_cbees ?(domain = None) ?(h = "lmcut") ?(hhat = "ff")
    ?(dhat = "ff(cost_type=1)") ?(timeout = Some 1800.) ?(bound = infinity) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "cbees";
		      "h", strip_prens h;
		      "hhat", strip_prens hhat;
		      "dhat", strip_prens dhat;] in
  let search_string = make_alg_string "bees" [h; hhat; dhat;
					      "keep_going=true";]
    timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %f" bound)
	    (Printf.sprintf "Alg: BEES h: %s hhat: %s dhat:%s" h hhat dhat))



let do_cbeeps ?(domain = None) ?(h = "lmcut") ?(hhat = "ff")
    ?(dhat = "ff(cost_type=1)") ?(timeout = Some 1800.) ?(bound = infinity) () =
  let problem_attrs = get_instance_attrs domain in
  let search_attrs = ["alg", "cbeeps";
		      "h", strip_prens h;
		      "hhat", strip_prens hhat;
		      "dhat", strip_prens dhat;] in
  let search_string = make_alg_string "beeps" [h; hhat; dhat;
					      "keep_going=true";]
    timeout in
    Notify.start_batchtime();
    do_run problem_attrs search_attrs search_string;
    if not !dry_run
    then (Notify.send_batch_completed_mail "Planning"
	    (Printf.sprintf "Bound: %f" bound)
	    (Printf.sprintf "Alg: BEES h: %s hhat: %s dhat:%s" h hhat dhat))

(* EOF *)
