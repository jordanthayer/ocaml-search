(** anytime windowed weighted A* as imagined by me, Jordan
    March 2010 *)

type 'a node = {
  data : 'a;
  fp: float;
  f : float;
  g: float;
  mutable heap_index: int;
  depth: float; (*for efficiency*)
}


let awastar ?(queue_record = Recorders.none) ?(get_win = (fun a -> a.depth))
    i initial is_goal expand ordered_p better_p hash_compare hash
    key =
  Iterated_window_search.do_search ~queue_record:queue_record i initial
    is_goal expand ordered_p better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i

let awastar_dd ?(queue_record = Recorders.none) ?(get_win = (fun a -> a.depth))
    i initial is_goal expand ordered_p better_p hash_compare hash
    key =
  Iterated_window_search.do_search_dd ~queue_record:queue_record i initial
    is_goal expand ordered_p better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i

let awastar_scaling ?(queue_record = Recorders.none)
    ?(get_win = (fun a -> a.depth)) i initial is_goal expand ordered_p
    better_p hash_compare hash key =
  Iterated_window_search_wscaling.do_search ~queue_record:queue_record i
    initial is_goal expand ordered_p better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i

let awastar_scaling_dd ?(queue_record = Recorders.none)
    ?(get_win = (fun a -> a.depth)) i initial is_goal expand ordered_p
    better_p hash_compare hash key =
  Iterated_window_search_wscaling.do_search_dd ~queue_record:queue_record i
    initial is_goal expand ordered_p better_p hash_compare hash key
    (fun a b -> a.heap_index <- b) (fun a -> a.heap_index)
    (fun a -> a.g) get_win;
  i


let f_ordered n1 n2 =
  n1.fp < n2.fp

let better_p n1 n2 =
  n1.f < n2.f

let wrap_expand wt expand h =
  (fun n -> List.map (fun (d, g) ->
			let h = h d in
			  {data = d;
			   fp = wt *. h +. g;
			   f = g +. h;
			   g = g;
			   heap_index = -1;
			   depth = n.depth +. 1.;})
     (expand n.data n.g))

let wrap f =
  (fun n -> f n.data)


let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)




let call_awastar wt rh unwrap sif =
  unwrap unwrap_sol_node
    (rh
       (awastar
	  (Limit.make Limit.Nothing sif.Search_interface.halt_on f_ordered
	     (Limit.make_default_logger (fun n -> n.g)
		(wrap sif.Search_interface.get_sol_length)))
	  {data=sif.Search_interface.initial;
	   fp = neg_infinity;
	   f = neg_infinity;
	   g = 0.0;
	   heap_index = -1;
	   depth = 0.}
	  (wrap sif.Search_interface.goal_p)
	  (wrap_expand wt sif.Search_interface.domain_expand
	     sif.Search_interface.h)
	  f_ordered
	  better_p
	  sif.Search_interface.equals
	  sif.Search_interface.hash
	  (wrap sif.Search_interface.key)))


let call_awastar_scaling wt rh unwrap sif =
  unwrap unwrap_sol_node
    (rh
       (awastar_scaling
	  (Limit.make Limit.Nothing sif.Search_interface.halt_on f_ordered
	     (Limit.make_default_logger (fun n -> n.g)
		(wrap sif.Search_interface.get_sol_length)))
	  {data=sif.Search_interface.initial;
	   fp = neg_infinity;
	   f = neg_infinity;
	   g = 0.0;
	   heap_index = -1;
	   depth = 0.}
	  (wrap sif.Search_interface.goal_p)
	  (wrap_expand wt sif.Search_interface.domain_expand
	     sif.Search_interface.h)
	  f_ordered
	  better_p
	  sif.Search_interface.equals
	  sif.Search_interface.hash
	  (wrap sif.Search_interface.key)))


let no_dups sif args =
  let wt = Search_args.get_float "Awwastar.no_dups" args 0 in
  call_awastar wt Limit.results5 Limit.unwrap_sol5 sif

and dups sif args =
  let wt = Search_args.get_float "Awwastar.dups" args 0 in
  call_awastar wt Limit.results6 Limit.unwrap_sol6 sif


let no_dups_scaling sif args =
  let wt = Search_args.get_float "Awwastar.no_dups_scaling" args 0 in
  call_awastar_scaling wt Limit.results5 Limit.unwrap_sol5 sif

and dups_scaling sif args =
  let wt = Search_args.get_float "Awwastar.dups_scaling" args 0 in
  call_awastar_scaling wt Limit.results6 Limit.unwrap_sol6 sif

(* EOF *)
