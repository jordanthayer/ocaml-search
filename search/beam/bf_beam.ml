


let fp_delta = 0.000001

type 'a node = {
  data : 'a; (**)
  f: float;(**)
  g: float;(**)
  mutable heap_index: int;(*where this node resides in the heap.*)
}

let make_initial initial_state =
  { data= initial_state;
    f=0.0;
    g = 0.0;
    heap_index = -1;
  }


let set_index n v = 
  n.heap_index <- v
let get_index n = 
  n.heap_index


let get_f n = n.f
let get_g n = n.g
let get_h n = n.f -.n.g


let h_ordered n1 n2 =
  if ((get_h n1) < (get_h n2)) then true
  else false

let f_ordered n1 n2 =
  if (n1.f < n2.f) then true
  else false


and setpos n i = n.heap_index <- i
and getpos n = n.heap_index
and wrap fn n = fn n.data
and unwrap_sol s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let make_expand exp h wt =
  (fun n -> List.map (fun (d,g) ->
			{ data = d;
			  g = g;
			  f =g +. (wt *. (h d));
			  heap_index = Dpq.no_position;}) (exp n.data n.g))



let wted_bf_dups sface args = 
  let beam_width = Search_args.get_int "Generic_beam" args 0 in
  let wt = Search_args.get_float "wted_bf_beam" args 2 in
  let order = 
    match (Search_args.get_string "wted beam order" args 1) with
	"f" -> f_ordered
      | "h" -> h_ordered
      | s -> failwith (Printf.sprintf "Invalid order choice: %s" s)
  and key = wrap sface.Search_interface.key
  and goalp = wrap sface.Search_interface.goal_p
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on
		f_ordered (Limit.make_default_logger (fun n -> n.g)
			    (fun n -> -1)))
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h wt)
  and root = make_initial sface.Search_interface.initial in
    
    Generic_beam.best_first_dups info beam_width root goalp expand 
      order
      f_ordered
      sface.Search_interface.hash sface.Search_interface.equals
      key setpos getpos;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)
