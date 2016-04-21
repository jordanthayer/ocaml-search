(* $Id: testgrid.ml,v 1.1 2005/01/17 18:55:56 ruml Exp ruml $
   code helpful for testing (mostly informal)
*)


open Grid
open Grid_algs
open Grid_instance

(********************** informal testing *****************************)
let ipath = "./jtd7/grid/data/instance/uniform"

let load_board ways cost prob num =
  let s = Wrutils.str "%s/%s/%s/%s/2000/1200/%s" ipath cost ways prob num
  in load s

let test a name w =
  clear_cache ();
  Wrutils.pr "\n--- %s ---\n\n" name;
  let ((s, e, g, p, m, d), t) = Wrsys.with_time (fun () -> a w [(Limit.Time 300.)]) in
    print_results w s e g p m d t;
    match s with
	None -> failwith "ack! no solution"
      | Some (p, f) ->
	  ignore(check_path w p f);
	  name, f, e


let compare_algs_on algs b =
  let r = List.map (fun (a, n) -> test a n b) algs in
    List.iter (fun (n, f, e) -> Wrutils.pr "%20s %F %4d\n%!" n f e) r


let u8compare_algs ?(w = 70) ?(h = 20) ?(p = 0.3) ?(seed = 31415) algs =
  let validator = Experiments.feasible_p_dups  with_path6
    Grid_interfaces.default_interface in
  let b = Math.with_random_seed
    (fun () -> unit_eight_board validator w h p) seed
  in compare_algs_on algs b

let compare_algs ?(w = 70) ?(h = 20) ?(p = 0.3) ?(seed = 31415) algs =
  let validator = Experiments.feasible_p_dups with_path6
    Grid_interfaces.default_interface in
  let b = Math.with_random_seed
    (fun () -> life_four_board validator w h p) seed in
    compare_algs_on algs b

let compare_algs_unit ?(w = 70) ?(h = 20) ?(p = 0.3) ?(seed = 31415) algs =
  let validator = Experiments.feasible_p_dups with_path6
    Grid_interfaces.default_interface in
  let b = Math.with_random_seed
    (fun () -> unit_four_board validator w h p) seed in
    compare_algs_on algs b


let all_algs = [
(*
  speedy, "speedy";
  greedy, "greedy";
  (*		 George_algs.george, "george, global error";
		 George_algs.george_path_err, "george, path error";
		 George_algs.george_level_err,"george, level error";
		 George_algs.george_window_err, "george, sliding window";
		 George_algs.george_classic_w, "george, classic w";*)
  (wted_a_star 3.), "wted A* (3)";
  (dyn_wted_a_star 3.), "dyn wted A* (3)";
  (*(Algs.a_star_eps 3.), "A*eps (3)";*)
  (*(Algs.a_star_epslow 3.), "A*epslow (3)";*)
  a_star, "A*";
  uniform_cost, "uniform cost";
  (*  Algs.ara_star_debug, "ARA* (3 and 0.2)" *) *)
]

let compare_all_algs () =
  compare_algs ~seed:(Random.int 100000) all_algs

(*
let compare_weights () =
  compare_algs [ (Algs.wted_a_star 3.), "wted A* (3)";
		 (Algs.a_star_eps 3.), "A*eps (3)";
		 (Algs.wted_a_star 2.), "wted A* (2)";
		 (Algs.a_star_eps 2.), "A*eps (2)";
		 (Algs.wted_a_star 1.5), "wted A* (1.5)";
		 (Algs.a_star_eps 1.5), "A*eps (1.5)";
		 (Algs.wted_a_star 1.25), "wted A* (1.25)";
		 (Algs.a_star_eps 1.25), "A*eps (1.25)";
		 (Algs.wted_a_star 1.2), "wted A* (1.2)";
		 (Algs.a_star_eps 1.2), "A*eps (1.2)";
		 (Algs.wted_a_star 1.1), "wted A* (1.1)";
		 (Algs.a_star_eps 1.1), "A*eps (1.1)";
		 (Algs.wted_a_star 1.05), "wted A* (1.05)";
		 (Algs.a_star_eps 1.05), "A*eps (1.05)";
		 Algs.a_star, "A*";]
*)

(********** testing with utility function ***************)


let test_u cost_c time_c a w =
  clear_cache ();
  let ((s, e, g, p, m, d), t) = Wrsys.with_time (fun () -> a w [Limit.Never]) in
    print_results w s e g p m d t;
    match s with
      None -> neg_infinity, 0
    | Some (_, c) ->
	let u = -. ((cost_c *. c) +. (t *. time_c)) in
	  Wrutils.pr "Cost of %.1f in %.3f secs = utility of %f\n%!" c t u;
	  u, e


let compare_alg_u_on b cost_c time_c =
  Wrutils.pr "\n------------ utility: cost %f, time %f --------------------\n"
    cost_c time_c;
  let res = (List.map (fun (name, alg) ->
			 Wrutils.pr "\n--- %s ---\n\n" name;
			 let u, _ = test_u cost_c time_c alg b in
			   u, name)
	       [
		 (*"speedy", speedy;
		 "greedy", greedy;
		 "A*", a_star;*)
		 (*"A* epsilon", (Algs.a_star_eps 3.0);*)])
		 (* "ARA*", Algs.ara_star_debug; *)
		 (*"bugsy", (Algs.bugsy cost_c time_c 10000) ])*) in
    Wrutils.pr "\nResults (cost %f, time %f\n----------------------------\n"
      cost_c time_c;
    List.iter (fun (u,n) ->
		 Wrutils.pr "%14s:%12.1f\n" n u)
      res


let compare_algs_u b =
  let do_balance secs_per =
    compare_alg_u_on b 1.0 (1. /. secs_per)
  in
    compare_alg_u_on b 0. 1.0;
    do_balance 0.00000001;
    do_balance 0.0000001;
    do_balance 0.000001;
    do_balance 0.00001;
    do_balance 0.0001;
    do_balance 0.001;
    do_balance 0.01;
    do_balance 0.1;
    do_balance 1.0;
    compare_alg_u_on b 1. 0.0


let compare_algs_u_random ?(w = 70) ?(h = 20) ?(p = 0.3) () =
  let validator = Experiments.feasible_p_dups  with_path6
    Grid_interfaces.default_interface in
    compare_algs_u (life_four_board validator w h p)


(*
/tilde/ruml/projects/path/grid/data/instance/uniform/life/4-way/500/300/0.25/1
*)
let compare_algs_u_file path =
  compare_algs_u (load path)


(******** verifying accuracy of life cost heuristics ************)


let cost_via_search w =
  failwith "Temporarily broken"
  (** returns cost of optimal path from start to goal in [w] and the
    path itself.  useful for verifying heuristics *)
(*  let s, _, _, _, _, _ = uniform_cost w [Limit.Never] in
    match s with
      None -> failwith "no sol w/o obstacles?"
    | Some (p, c) -> c, p*)


let random_position w =
  (Random.int (width w)), (Random.int (height w))


let test_heuristic ?(w = 70) ?(h = 20) n =
  let validator = Experiments.feasible_p_dups  with_path6
    Grid_interfaces.default_interface in
    List.iter
      (fun costs ->
	 List.iter
	   (fun ways ->
	      Wrutils.pr "Making board for %s %s...%!"
		(tag_of_moves ways) (tag_of_costs costs);
	      let b = (feasible_board validator w h 0. costs ways) in
		Wrutils.pr "done.\n%!";
		Wrutils.ntimes
		  (fun () ->
		     let b = { b with
				 start = random_position b;
				 goal = [random_position b]; } in
		     let h = (Grid.get_cheapest_h b) (make_root b)
		     and truth, path = cost_via_search b in
		       if h <> truth then
			 let s = draw_board b in
			   draw_path b s path;
			   Wrutils.pr "heuristic said %f but truth is %f:\n"
			     h truth;
			   print_board stdout s)
		  n)
	   [ Grid.Fourway; Grid.Eightway ])
      [ Grid.Unit; Grid.Life ]


(********************** debugging retrograde a-star *****************)
(*
open Grid_retro_algs

let do_retro ?(log_filename="/dev/null") ?(w=70) ?(h=20) () =
  let log_ch =
    match log_filename with
      "stdout" -> stdout
    | _ -> open_out log_filename
  in
    compare_algs ~w:w ~h:h ~seed:(Random.int 100000)
      [ (* (a_star, "a_star"); *)
        (retro_a_star ~log_ch:log_ch, "retro_a_star") ];
    if log_ch != stdout then
      close_out log_ch;
    Gc.print_stat stdout
*)
(********************** debugging bugsy *****************************)

(*
let bugsy_on b cost_c time_c =
  Wrutils.pr "\n------------ utility: cost %f, time %f --------------------\n"
    cost_c time_c;
  test_u cost_c time_c (Algs.bugsy_v cost_c time_c 10000) b


(*
  75 50 0.4 oh yes, even speedy has problems
  75 50 0.3 sometimes, and sometimes even speedy
  50 30 0.4 usually
  50 30 0.3 quite seriously
            seeds: 8234, 4514, 5328, 1874
  50 30 0.2 very rarely
  25 20 0.3 very rarely
*)
let debugsy ?(seed = Random.int 10000) w h p =
  let res = Math.with_random_state
	      (fun () ->
		 let b = life_four_board w h p in
		   List.map (fun ways ->
			       List.map (fun (cost,time) ->
					   let u, g = bugsy_on b cost time in
					     g)
			       [ 0., 1.;
				 1., 1e7;
				 1., 1e5;
				 1., 1e3;
				 1., 0.; ])
		     [Grid.Fourway
			(* ; Grid.Eightway *)
		     ])
	      (Math.random_state_from seed) in
    Wrutils.pr "Seed was %d.\n" seed;
    res


(************** A star epsilon **************)


let test_eps1 () =
  Verb.with_level 5
    (fun () ->
       let i = life_four_board 10 10 0.1 in
	 Algs.a_star_eps 1.1 i)


let test_as f =
  let i = load f in
    Grid.clear_cache ();
    let (s, e, g, p, m, d), t = Wrsys.with_time (fun () -> Algs.a_star i) in
      Algs.print_results i s e g p m d t


let small = "/tilde/ruml/projects/path/grid/src/test.in"
let large = "/tilde/ruml/projects/path/grid/data/instance/uniform/life/4-way/500/300/0.2/1"


let test_eps a f =
  let i = load f in
    a 1.1 i

let test1 () =
  let i = load small in
    Algs.a_star_eps 1.1 i
*)
(*
  Grid.a_star_eps
  Grid.a_star_epslow
*)


(* EOF *)
