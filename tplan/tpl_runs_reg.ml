(* $Id: runs.ml,v 1.1 2005/04/06 00:39:05 ruml Exp ruml $
   search algorithms for tplan
*)


let run_alg alg domain problem limit =
  Datafile.write_colnames stdout
    ["sol cost"; "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (sol, e, g, p, q),t = Wrsys.with_time (fun () ->
					       alg domain problem limit) in
    flush_all ();
    (match sol with
	 None ->
	   Datafile.write_pairs stdout ["found solution", "no";
					"final sol cost", "0."]
       | Some (s, ms) ->
	   Verb.force 2 (lazy (Tpl_regression.print_plan stderr s.Tpl_regression.s));
	   Wrutils.pr "%f\t%d\t%d\t%f\n" ms e g t;
	   Datafile.write_pairs stdout ["found solution", "yes";
					"final sol cost", string_of_float ms]);

    let trail_pairs = ["total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g;
		       "total nodes pruned", string_of_int p;
		       "max search queue", string_of_int q;
		       "time per node", string_of_float (t /. (float) g)]
    in
      Datafile.write_pairs stdout trail_pairs

let prob_path = "./group/data/tplan"
and dom_path  = "./group/data/tplan"

let read ch =
  let buf = Lexing.from_channel ch in
  let domain = Wrlexing.parse_verb 5 stderr "planning domain"
		 (Parse_pddl.domain Lex.lexer) buf in
  let problem = Wrlexing.parse_verb 5 stderr "problem instance"
		  (Parse_pddl.problem Lex.lexer) buf in
  let problem = Tpl_domain.correct_problem domain problem in
    domain, problem

let read_p dom num =
  let pin = Lexing.from_channel (Unix.in_channel_of_descr
    (Unix.openfile (Wrutils.str "%s/%s/%s" prob_path dom num) [] 0))
  and din = Lexing.from_channel
    (Unix.in_channel_of_descr
      (Unix.openfile (Wrutils.str "%s/%s" dom_path dom) [] 0)) in
  let domain = Wrlexing.parse_verb 5 stderr "planning domain"
    (Parse_pddl.domain Lex.lexer) din in
  let problem = Wrlexing.parse_verb 5 stderr "problem instance"
    (Parse_pddl.problem Lex.lexer) pin in
  let problem = Tpl_domain.correct_problem domain problem in
    domain, problem

(** called by main **)

let run_alg_ch alg limit ch =
  let domain, problem = read ch in
    run_alg alg domain problem limit


(************** running experiments **************)


let data_root = "./research/data/tplan"
(*
let data_root = Experiments.data_root ^ "tplan"
*)
let instance_root = "./group/data/tplan"
let solver_binary = "./research/tplan/tplan_reg.linux"

let time_limit = 300

let call_planner args domain_path prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
    output to [outch].  all logging is done within here.  returns cost, time
    pair *)
  let limit_args = Wrutils.str "-limit %i %s" time_limit args in
  Wrsys.with_subprocess_pipes
    (Wrutils.str "cat %s %s | %s %s" domain_path prob_path solver_binary limit_args)
    (fun to_solver from_solver ->
       Datafile.write_header_pairs outch;
       Datafile.write_pairs outch attrs;
       Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
       let res = Datafile.pipe_data from_solver outch in
	 Datafile.write_trailer_pairs outch;
	 res
    )

let do_run args domain_path prob_path attrs =
  (** sets up run file and calls solver on problem *)
  let run_file = Rdb.path_for data_root
		   (Rdb.filter_attrs["type"] attrs) in
    if ((Sys.file_exists run_file)
	 && (Datafile.seems_complete run_file))
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running on %s...%!" (Wrfname.make_relative data_root
					    prob_path);
	   let c,t = call_planner args domain_path prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)


let do_batches alg_attrs args =
  (** runs an alg on all problems *)
  List.iter (fun domain_attrs ->
	       let domain_path = Rdb.path_for dom_path domain_attrs in
		 List.iter (fun prob_attrs ->
			      let prob_path = Rdb.path_for prob_path prob_attrs
			      and attrs = alg_attrs @ prob_attrs in
				do_run args domain_path prob_path attrs)
		   (Rdb.matching_attrs prob_path
		      (Rdb.override_attrs ["type", "instance"] domain_attrs)))
    (Rdb.matching_attrs dom_path ["type", "domain"])


let do_domain alg_attrs args domainname =
  (** runs an alg on all problems *)
  List.iter (fun domain_attrs ->
	       let domain_path = Rdb.path_for dom_path domain_attrs in
		 List.iter (fun prob_attrs ->
			      let prob_path = Rdb.path_for data_root prob_attrs
			      and attrs = alg_attrs @ prob_attrs in
				do_run args domain_path prob_path attrs)
		   (Rdb.matching_attrs prob_path
		      (Rdb.override_attrs ["type", "instance"] domain_attrs)))
    (Rdb.matching_attrs dom_path ["type", "domain";
				  "domainname", domainname;])


(***
  a_star, greedy, speedy,
  wted_a_star, a_star_eps
  anytime_a_star, ara_star
  bugsy
***)

let do_basic_batch alg =
  (* a_star, greedy *)
  do_batches ["alg", alg] alg

let weights =
  [(*100000.;
   100.;
   50.;
   20.;
   15.;*)
   10.;
   7.;
   5.;
   4.;
   3.;
   2.5; 2.; 1.75;1.5; 1.3; 1.2; 1.1;1.15; 1.05; 1.01; 1.001; 1.0005; 1.;]

(*let weights = List.rev weights*)


let do_wted_batch alg =
  (* wted_a_star, a_star_eps, anytime_a_star *)
  List.iter (fun wt ->
	       do_batches ["alg", alg;
			   "wt", string_of_float wt;]
	       (Wrutils.str "%s %f" alg wt))
    (List.rev weights)


(* let do_ara_star_batch () =
  List.iter (fun wt ->
	       do_batches ["alg", "ara_star";
			   "wt", string_of_float wt;
			   "decr", string_of_float 0.1;]
	       (Wrutils.str "ara_star %f 0.1 1"  wt))
    weights


let do_ara_star_3_02_batch () =
      (*  *)
  do_batches ["alg", "ara_star";
	      "wt", string_of_float 3.;
	      "decr", string_of_float 0.2;]
    (Wrutils.str "ara_star 3. 0.2 1")
*)




let utility_tuples =
  (* cost_coeff, time_coeff *)
  let r secs_per_cost =
    1., (1. /. secs_per_cost)
  in
    [ (* 0., 1.0; *)
      (* r 0.0000001;
      r 0.000001; *)
      r 0.000005;
      r 0.00001;
      r 0.00005;
      r 0.0001;
      r 0.0005;
      r 0.001;
      r 0.005;
      r 0.01;
      r 0.05;
      r 0.1;
      r 0.5;
      r 1.;
      r 5.;
      r 10.;
      r 50.;
      1., 0.0; ]


let do_bugsy_batch alg domain =
  (* bugsy, bugsy_coeff.  PLEASE ROUGHLY NORMALIZE COSTS IN DOMAINS *)
  List.iter (fun (cost, time) ->
	       let attrs = ["alg", alg;
			    "cost_coeff", string_of_float cost;
			    "time_coeff", string_of_float time; ]
	       and args = Wrutils.str "%s %f %f" alg cost time
	       in
		 do_domain attrs args domain)
    utility_tuples

(* EOF *)
