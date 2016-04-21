(**

    @author jtd7
    @since 2011-12-09
*)

open Tqs_rewrite

let make_dummy_incumbent init cost = { data = init;
				  fp = { h = 0.; d = 0.; h_err = 0.;
					 d_err = 0.; g = cost;
					 f = cost; est_f = cost;
					 est_d = 0.; };
				  ints = { clean_pos = -1337;
					  open_pos = -1337;
					  depth = -1;};
				  geqe = Geq.make_dummy_entry();}


let dups sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand sface.SI.domain_expand hd in
  let cost = Search_args.get_float "Tqs_rewrite.dups" args 0 in
  let bound = Search_args.get_float "Tqs_rewrite.dups" args 1 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
    reset();
    Limit.new_incumbent i (Limit.Incumbent (0., (make_dummy_incumbent initial.data cost)));
    let i = search i key hash equals goal expand initial bound in
      Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)

