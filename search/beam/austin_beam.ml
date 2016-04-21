(**

    @author jtd7
    @since 2011-03-11

   Beam search code that relies on Austin's learning technique
*)

open Path_based_adaptive


let call_dups ?(search = Generic_beam.breadth_first_dups) f_calc sface args =
  let info = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
		(Limit.make_default_logger (fun n -> n.g) (fun n -> n.depth)))
  and beam_width = Search_args.get_int "Austin_Beam" args 0
  and goal_p = wrap sface.Search_interface.goal_p
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.hd f_calc)
  and key = wrap sface.Search_interface.key
  and h,d = sface.Search_interface.hd sface.Search_interface.initial in
  let root = {est_f = h;
	      h_err = 0.;
	      d_err = 0.;
	      d = d;
	      f = h;
	      g = 0.;
	      depth = 0;
	      q_pos = Dpq.no_position;
	      data = sface.Search_interface.initial;} in
    Generic_beam.breadth_first_dups info beam_width root goal_p expand
      ordered_p better_p sface.Search_interface.hash
      sface.Search_interface.equals key setpos getpos;
    Limit.unwrap_sol6 unwrap_sol
      (Limit.results6 info)


let greedy_path_dups sface args = call_dups greedy sface args
and austin_dups sface args = call_dups unclamped_austin sface args

let bfs_greedy_path_dups sface args =
  call_dups ~search:Generic_beam.best_first_dups greedy sface args

and bfs_austin_dups sface args =
  call_dups ~search:Generic_beam.best_first_dups unclamped_austin sface args


let seeded_greedy_path_dups sface args =
  let herr_seed = Search_args.get_float "Seeded_greedy" args 1
  and derr_seed = Search_args.get_float "Seeded_greedy" args 2
  and seed_ct = Search_args.get_float "Seeded_greedy" args 3 in
  call_dups ~search:Generic_beam.breadth_first_dups
    (seeded_greedy ~herr_seed ~derr_seed ~seed_ct) sface args


and seeded_austin_dups sface args =
  let herr_seed = Search_args.get_float "Seeded_greedy" args 1
  and derr_seed = Search_args.get_float "Seeded_greedy" args 2
  and seed_ct = Search_args.get_float "Seeded_greedy" args 3 in
    call_dups ~search:Generic_beam.breadth_first_dups
      (seeded_austin ~herr_seed ~derr_seed ~seed_ct) sface args
(* eof *)
