(** Recursive best first search

    From "Anytime Heuristic Search", E. Hansen and R. Zhou,
    Algorithm 2

    @author eaburns
    @since 2009-10-12
*)

type 'a t = {
  data : 'a;
  g : float;
  h : float;
  f : float;
  mutable big_f : float;
}


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let is_better a b =
  (* compares two node tuples *)
 a.big_f < b.big_f


let has_solution i = match i.Limit.incumbent with
    | Limit.Nothing -> false
    | _ -> true


let make_expand sface i =
  (fun node ->
     let children = sface.Search_interface.domain_expand node.data node.g in
       Limit.incr_exp i;
       List.map (fun (ch, g) ->
		   let h = sface.Search_interface.h ch in
		   let f = g +. h in
		     Limit.incr_gen i;
		     { data = ch;
		       g = g;
		       h = h;
		       f = f;
		       big_f = (if node.f < node.big_f
				then Math.fmax node.big_f f
				else f);
		     })
	 children)


let do_search initial i expand is_goal =
  let rec rbfs node threshold =
    if is_goal node.data
    then begin
      Limit.new_incumbent i (Limit.Incumbent (1.0, node));
      infinity
    end else
      let children = Dpq.create_with is_better initial in
	List.iter (fun ch -> Dpq.insert children ch) (expand node);
	if Dpq.empty_p children then
	  infinity
	else begin
	  let n1 = ref (Dpq.extract_first children) in
	    while Math.finite_p !n1.big_f
	      && !n1.big_f <= threshold
	      && not (has_solution i)
	      && not (Limit.halt_p i)
	    do
	      let big_f_n2 =
		if Dpq.empty_p children then infinity
		else (Dpq.peek_first children).big_f
	      in
		!n1.big_f <- rbfs !n1 (Math.fmin threshold big_f_n2);
		Dpq.insert children !n1;
		n1 := Dpq.extract_first children
	    done;
	    !n1.big_f
	end
  in
    ignore (rbfs initial infinity)


let no_dups sface args=
  Search_args.is_empty "Recursive_best_first.no_dups" args;
  let i = Limit.make Limit.Nothing  sface.Search_interface.halt_on
      is_better (Limit.make_default_logger
		   (fun n -> n.g)
		   (fun n -> sface.Search_interface.get_sol_length n.data)) in
  let expand = make_expand sface i in
    ignore (do_search { data = sface.Search_interface.initial;
		   g = 0.;
		   h = 0.;
		   f = 0.;
		   big_f = 0.; } i expand sface.Search_interface.goal_p);
    Limit.unwrap_sol5 unwrap_sol (Limit.results5 i)


let dups sface args=
  Search_args.is_empty "Recursive_best_first.no_dups" args;
  let i = Limit.make Limit.Nothing  sface.Search_interface.halt_on
      is_better (Limit.make_default_logger
		   (fun n -> n.g)
		   (fun n -> sface.Search_interface.get_sol_length n.data)) in
  let expand = make_expand sface i in
    ignore (do_search { data = sface.Search_interface.initial;
		   g = 0.;
		   h = 0.;
		   f = 0.;
		   big_f = 0.; } i expand sface.Search_interface.goal_p);
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)

(* EOF *)
