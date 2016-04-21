(**
  anytime windowed a* as described by Aine, Chakrabarti and Kumar 2007.
  initial implementation by cm wilt november 2009
*)


type 'a node = {
  data : 'a;
  f: float;
  g: float;
  mutable heap_index: int;
  d: float; (*for efficiency*)
}


let in_window node_value current_window size =
  node_value >= current_window +. size

(*
let in_window node_value current_window size =
  let b = node_value >= current_window +. size in
    if b then Verb.pe Verb.always "%f >= %f + %f\n" node_value
      current_window size;
    b
*)

let awastar ?(queue_record = Recorders.none) ?(get_win = (fun a -> a.d))
    i initial is_goal expand ordered_p better_p hash_compare hash
    key =
  Iterated_window_search.do_search ~win_fn:in_window ~get_next_window:Math.fmin
    (*~init_window:infinity*) (*uncomment to test sanity *)
    ~queue_record:queue_record i initial is_goal expand ordered_p
    better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i

let awastar_dd ?(queue_record = Recorders.none) ?(get_win = (fun a -> a.d))
    i initial is_goal expand ordered_p better_p hash_compare hash
    key =
  Iterated_window_search.do_search_dd ~win_fn:in_window
    ~get_next_window:Math.fmin
    ~queue_record:queue_record i initial is_goal expand ordered_p
    better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i



let awastar_scaling ?(queue_record = Recorders.none) ?(get_win = (fun a -> a.d))
    i initial is_goal expand ordered_p better_p hash_compare hash
    key =
  Iterated_window_search_wscaling.do_search ~win_fn:in_window ~get_next_window:Math.fmin
    ~queue_record:queue_record i initial is_goal expand ordered_p
    better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i

let awastar_scaling_dd ?(queue_record = Recorders.none) ?(get_win = (fun a -> a.d))
    i initial is_goal expand ordered_p better_p hash_compare hash
    key =
  Iterated_window_search_wscaling.do_search_dd ~win_fn:in_window
    ~get_next_window:Math.fmin
    ~queue_record:queue_record i initial is_goal expand ordered_p
    better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i

let f_ordered n1 n2 =
  n1.f < n2.f


let better_p n1 n2 =
  n1.g < n2.g


let wrap_expand expand hd =
  (fun n -> List.map (fun (d, g) ->
			let hv,dv = hd d in
			  {data = d;
			   f = hv +. g;
			   g = g;
			   heap_index = -1;
			   d = dv;})
     (expand n.data n.g))

let wrap f =
  (fun n -> f n.data)


let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let call_awastar rh unwrap sif =
  let ih,id = sif.Search_interface.hd sif.Search_interface.initial in
    unwrap unwrap_sol_node
      (rh
	 (awastar
	    (Limit.make Limit.Nothing sif.Search_interface.halt_on f_ordered
	       (Limit.make_default_logger (fun n -> n.g)
		  (wrap sif.Search_interface.get_sol_length)))
	    {data=sif.Search_interface.initial;
	     f = ih;
	     g = 0.0;
	     heap_index = -1;
	     d = id;}
	    (wrap sif.Search_interface.goal_p)
	    (wrap_expand sif.Search_interface.domain_expand
	       sif.Search_interface.hd)
	    f_ordered
	    better_p
	    sif.Search_interface.equals
	    sif.Search_interface.hash
	    (wrap sif.Search_interface.key)))

let call_awastar_dd rh unwrap sif =
  let ih,id = sif.Search_interface.hd sif.Search_interface.initial in
    unwrap unwrap_sol_node
      (rh
	 (awastar_dd
	    (Limit.make Limit.Nothing sif.Search_interface.halt_on f_ordered
	       (Limit.make_default_logger (fun n -> n.g)
		  (wrap sif.Search_interface.get_sol_length)))
	    {data=sif.Search_interface.initial;
	     f = ih;
	     g = 0.0;
	     heap_index = -1;
	     d = id;}
	    (wrap sif.Search_interface.goal_p)
	    (wrap_expand sif.Search_interface.domain_expand
	       sif.Search_interface.hd)
	    f_ordered
	    better_p
	    sif.Search_interface.equals
	    sif.Search_interface.hash
	    (wrap sif.Search_interface.key)))


let call_awastar_scaling rh unwrap sif =
  let ih,id = sif.Search_interface.hd sif.Search_interface.initial in
    unwrap unwrap_sol_node
      (rh
	 (awastar_scaling
	    (Limit.make Limit.Nothing sif.Search_interface.halt_on f_ordered
	       (Limit.make_default_logger (fun n -> n.g)
		  (wrap sif.Search_interface.get_sol_length)))
	    {data=sif.Search_interface.initial;
	     f = ih;
	     g = 0.0;
	     heap_index = -1;
	     d = id;}
	    (wrap sif.Search_interface.goal_p)
	    (wrap_expand sif.Search_interface.domain_expand
	       sif.Search_interface.hd)
	    f_ordered
	    better_p
	    sif.Search_interface.equals
	    sif.Search_interface.hash
	    (wrap sif.Search_interface.key)))

let call_awastar_scaling_dd rh unwrap sif =
  let ih,id = sif.Search_interface.hd sif.Search_interface.initial in
    unwrap unwrap_sol_node
      (rh
	 (awastar_scaling_dd
	    (Limit.make Limit.Nothing sif.Search_interface.halt_on f_ordered
	       (Limit.make_default_logger (fun n -> n.g)
		  (wrap sif.Search_interface.get_sol_length)))
	    {data=sif.Search_interface.initial;
	     f = ih;
	     g = 0.0;
	     heap_index = -1;
	     d = id;}
	    (wrap sif.Search_interface.goal_p)
	    (wrap_expand sif.Search_interface.domain_expand
	       sif.Search_interface.hd)
	    f_ordered
	    better_p
	    sif.Search_interface.equals
	    sif.Search_interface.hash
	    (wrap sif.Search_interface.key)))


let no_dups sif args =
  Search_args.is_empty "Awasta_d.no_dups" args;
  call_awastar Limit.results5 Limit.unwrap_sol5 sif

and dups sif args =
  Search_args.is_empty "Awasta_d.dups" args;
  call_awastar Limit.results6 Limit.unwrap_sol6 sif

and dd sif args =
  Search_args.is_empty "Awasta_d.dd" args;
  call_awastar_dd Limit.results6 Limit.unwrap_sol6 sif


let no_dups_scaling sif args =
  Search_args.is_empty "Awasta_d.no_dups_scaling" args;
  call_awastar_scaling Limit.results5 Limit.unwrap_sol5 sif

and dups_scaling sif args =
  Search_args.is_empty "Awasta_d.dups_scaling" args;
  call_awastar_scaling Limit.results6 Limit.unwrap_sol6 sif

and dd_scaling sif args =
  Search_args.is_empty "Awasta_d.dd_scaling" args;
  call_awastar_scaling_dd Limit.results6 Limit.unwrap_sol6 sif

(* eof *)
