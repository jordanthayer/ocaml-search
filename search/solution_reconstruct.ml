(** Solution reconstruction for searches that do not use a closed
    list.

    @author eaburns
    @since 2010-03-08
*)

let divide_and_conquer search sface =
  (** [recursive search make_cost_between sface]

      search : sface -> (half_way_node * cost) *)
  let rec recur sface =
    let is_goal = sface.Search_interface.goal_p in
    let new_subproblem = sface.Search_interface.new_subproblem in
    let init = sface.Search_interface.initial in
    let pr_key = sface.Search_interface.key_printer in
    let key = sface.Search_interface.key in
    let info = sface.Search_interface.info in
      if is_goal init
      then Some ([init], 0.)
      else match search sface with
	| None ->
	    Verb.pr Verb.optional "No goal at %d\n%!" info.Limit.expanded;
	    None
	| Some (mid, cost) when is_goal mid ->
	    Verb.pr Verb.optional "Got goal at %d\n%!" info.Limit.expanded;
	    Some ([mid], cost)
	| Some (mid, cost) ->
	    Verb.pr Verb.optional "Got middle goal at %d, cost=%g: %s -> %s\n%!"
	      info.Limit.expanded cost (pr_key (key init)) (pr_key (key mid));
	    let new_sface = new_subproblem info ~init ~target:mid in
	      begin match recur new_sface with
		| None -> None
		| Some (first_path, _) ->
		    Verb.pr Verb.optional "Split at %d\n%!" info.Limit.expanded;
		    let new_sface =  { sface with
					 Search_interface.initial = mid }
		    in
		      begin match recur new_sface with

			| None -> None
			| Some (second_path, _) ->
			    Some ((first_path @ second_path), cost)
		      end
	      end
  in recur sface
