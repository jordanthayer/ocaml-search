(**
  anytime windowed a* as described by Aine, Chakrabarti and Kumar 2007.
  initial implementation by cm wilt november 2009
*)


type 'a node = {
  data : 'a;
  f: float;
  g: float;
  mutable heap_index: int;
  h: float; (*for efficiency*)
}


let awastar ?(queue_record = Recorders.none) ?(get_win = (fun a -> a.g))
    i initial is_goal expand ordered_p better_p hash_compare hash
    key =
  Iterated_window_search.do_search
    ~queue_record:queue_record i initial is_goal expand ordered_p better_p
    hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i


let f_ordered n1 n2 =
  n1.f < n2.f


let better_p n1 n2 =
  n1.g < n2.g


let wrap_expand expand h =
  (fun n -> List.map (fun (d, g) ->
			let hv = h d in
			  {data = d;
			   f = hv +. g;
			   g = g;
			   heap_index = -1;
			   h = hv;})
     (expand n.data n.g))

let wrap f =
  (fun n -> f n.data)


let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let call_awastar rh unwrap sif =
  let v_i = sif.Search_interface.h sif.Search_interface.initial in
    unwrap unwrap_sol_node
      (rh
	 (awastar
	    (Limit.make Limit.Nothing sif.Search_interface.halt_on f_ordered
	       (Limit.make_default_logger (fun n -> n.g)
		  (wrap sif.Search_interface.get_sol_length)))
	    {data=sif.Search_interface.initial;
	     f = v_i;
	     g = 0.0;
	     heap_index = -1;
	     h = v_i;}
	    (wrap sif.Search_interface.goal_p)
	    (wrap_expand sif.Search_interface.domain_expand
	       sif.Search_interface.h)
	    f_ordered
	    better_p
	    sif.Search_interface.equals
	    sif.Search_interface.hash
	    (wrap sif.Search_interface.key)))


let no_dups sif args =
  Search_args.is_empty "Awasta_g.no_dups" args;
  call_awastar Limit.results5 Limit.unwrap_sol5 sif


let dups sif args =
  Search_args.is_empty "Awasta_g.dups" args;
  call_awastar Limit.results6 Limit.unwrap_sol6 sif

(* eof *)
