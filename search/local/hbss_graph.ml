(**

   Heuristic Biased Stochastic Sampling, as presented by John L. Bresina

*)


type bias = 
    Log
  | Linear
  | Polynomial of int
  | Exponental


let bf_log r = 1.0 /. (log (r +. 1.0))
let bf_linear r = 1.0 /. r
let bf_poly deg r = r ** (deg *. (-1.0))
let bf_exp r = 2.71828183 ** (r *. (-1.0))

type 'a hbss_node = {
  data : 'a;
  h : float;
  g : float;
  depth: int;
}

let unwrap s = 
  s.data

let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let wrap f =
  (fun n -> f n.data)


let wrap2 f =
  (fun a b -> f a.data b.data)


let wrap_expand 
    expand 
    h 
    =
  (fun n ->
     let children = (List.map (fun (d, g) ->
                                 { data = d;
                                   h = h d;
                                   g = g;
				   depth = n.depth + 1;})
                       (expand n.data n.g)) in children)


let h_ordered n1 n2 =
  (**checks to see if 2 nodes are h ordered tie breaking on g.*)
  let diff = n1.h -. n2.h in
    if(diff < 0.0) then (-1)
    else if (diff > 0.0) then (1)
    else 0


let f_ordered n1 n2 =
  (**checks to see if 2 nodes are f ordered tie breaking on g*)
  let f1 = n1.g +. n1.h in 
  let f2 = n2.g +. n2.h in
  (f1 < f2) ||
    ((f1 = f2) &&
       (n1.g > n2.g))


let make_initial initial_state =
  { data = initial_state;
    h = 0.0;
    g = 0.0;
    depth = 0;}

 
let hbss sif args = 
  let backtracks = ref (-1) in 
  let bias_function_string = Search_args.get_string "Hbss.hbss"
    args 0 in
  let bias_function = 
    match bias_function_string with
	"log" -> bf_log
      | "exp" -> bf_exp
      | "lin" -> bf_linear
      | "p_2" -> bf_poly 2.0
      | "p_3" -> bf_poly 3.0
      | bad_bf -> failwith 
	  (Printf.sprintf "%s is an invalid bias fuction" bad_bf) in
  let bound = Search_args.get_int "Hbss.hbss" args 1 in
  let is_goal n = wrap sif.Search_interface.goal_p n in
  let limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
                   f_ordered
                   (Limit.make_default_logger 
		      (fun n -> n.g)
		      (wrap sif.Search_interface.get_sol_length))) in
  let initial = make_initial sif.Search_interface.initial in
  let (expand_node:'a -> 'a list) = wrap_expand 
    sif.Search_interface.domain_expand sif.Search_interface.h in

  let closed_list = ref (Htable.create sif.Search_interface.hash
			   sif.Search_interface.equals (bound + 10)) in

  let key n = (wrap sif.Search_interface.key) n in
  let ht_add n = 
    Htable.replace !closed_list (key n) n in
  let ht_check n = Htable.mem !closed_list (key n) in


  let rec hbss_search state = 
    if(is_goal state) then (Limit.new_incumbent limit_t
			      (Limit.Incumbent (0., state)))
    else if (state.depth > bound) then ()
    else 
      (
	ht_add state;
	let dup_counter = ref 0 in
	let children = Array.of_list (
	  List.filter (fun n -> 
			 if(ht_check n) then
			   (
			     dup_counter := !dup_counter + 1;
			     Limit.incr_dups limit_t;
			     false
			   )
			 else 
			   true
		      )
	    (expand_node state)) in
	  Limit.incr_exp limit_t;
	  Limit.incr_gen_n limit_t (Array.length children);
	  Array.fast_sort h_ordered children;
	  let children_wt = Array.make (Array.length children) 0.0 in
	    if(Array.length children_wt > 0) then (
	      (*
		for i = 0 to (Array.length children) - 2
		do
		if(children.(i).h > children.(i+1).h) then
		failwith (Printf.sprintf
		"%f %f in index %d %d"
		children.(i).h    
		children.(i+1).h
		i 
		(i+1));
		done;
	      *)
	      children_wt.(0) <- bias_function 1.0;
	      let total_wt = ref children_wt.(0) in
		for i = 1 to (Array.length children) - 1
		do
		  children_wt.(i) <- (bias_function 
					(1.0 +. (float_of_int i))) +. 
		    children_wt.(i-1);
		  total_wt := !total_wt +. children_wt.(i);
		done;
		let random_selection = Random.float !total_wt in
		let selected_index = try (Wrarray.find 
					    (fun v -> v < random_selection) children_wt)
		with Not_found -> (Array.length children) - 1 
		in
		  hbss_search children.(selected_index))
	    else ()
      ) in 
    while ((not (Limit.halt_p limit_t)) &&
	     limit_t.Limit.incumbent = Limit.Nothing)
    do
      hbss_search initial;
      closed_list := 
	(Htable.create sif.Search_interface.hash
	   sif.Search_interface.equals (bound + 10));

      backtracks := !backtracks + 1;
    done;

    Datafile.write_pairs stdout 
      ["backtracks",(string_of_int !backtracks)];


    Limit.unwrap_sol6 unwrap_sol_node (Limit.results6 limit_t)

