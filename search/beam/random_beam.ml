(**

    @author jtd7
    @since 2011-04-24
*)

type 'a node = {
  data : 'a;
  g : float;
  rand : float;
  mutable pos : int;
}


let make_expand exp =
  (fun n -> List.map (fun (d,g) ->
			{ data = d;
			  g = g;
			  rand = Random.float 1.;
			  pos = Dpq.no_position;}) (exp n.data n.g))

let ordered_p a b = a.rand < b.rand || (a.rand = b.rand && a.g < b.g)
and better_p a b = a.g < b.g
and setpos n i = n.pos <- i
and getpos n = n.pos
and wrap fn n = fn n.data
and unwrap_sol s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let random_dups sface args =
  let beam_width = Search_args.get_int "Generic_beam" args 0
  and key = wrap sface.Search_interface.key
  and goalp = wrap sface.Search_interface.goal_p
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on
		better_p (Limit.make_default_logger (fun n -> n.g)
			    (fun n -> -1)))
  and expand = make_expand sface.Search_interface.domain_expand
  and root = { data = sface.Search_interface.initial;
	       g = 0.; rand = 0.; pos = Dpq.no_position; } in
    Generic_beam.breadth_first_dups
      info beam_width root goalp expand ordered_p better_p
      sface.Search_interface.hash sface.Search_interface.equals
      key setpos getpos;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)

