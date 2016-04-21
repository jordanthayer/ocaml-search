(**

    @author jtd7
    @since 2010-10-08

   Greedy search on a single corrected heuristic
*)


type 'a node = {
  mutable est_h : float;
  h : float;
  d : float;
  g : float;
  depth : int;
  mutable q_pos : int;   (* for Dpq *)
  data : 'a;
}


let setpos n i =
  (** Updates the [q_pos] of node [n], setting it to [i] *)
  n.q_pos <- i


let getpos n =
  (** returns the current [q_pos] of node [n] *)
  n.q_pos


let wrap f =
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> f n.data)


let est_f_then_d_then_g a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      low d values, and then in favor of high g *)
  (a.est_h < b.est_h) ||  (* sort by fhat *)
    ((a.est_h = b.est_h) &&
       a.g >= b.g)

let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  a.g <= b.g


let unwrap_sol s =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


let wrap_incumbent i =
  (** Wraps an incumbent solution [i] and returns a Limit.info based on it *)
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)


let make_expand expand hd timer calc_h_data f_calc =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [timer] returns true every so often, to tell the openlist to resort
      [calc_h_data] takes the parent, the best child, and all children
      in order to make a better h estimator
      [f_calc] uses the estimated h values to calculate the bounded f
      estimates *)
  (fun n ->
     let best_f = ref infinity
     and best_child = ref n
     and reorder = timer() in
     let children = (List.map (fun (s, g) ->
				 let h, d = hd s in
				 let f = g +. h in
				 let c =
				   {est_h = h;
				    h = h;
				    d = d;
				    g = g;
				    depth = n.depth + 1;
				    q_pos = Dpq.no_position;
				    data = s;} in
				   if  f <= !best_f then
				     (best_child := c;
				      best_f := f);
				   c)
		       (expand n.data n.g))
     in
       if not ((List.length children) = 0)
       then
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> c.est_h <- f_calc c) children);
       reorder, children)


let make_updater f_calc =
  (** Updates the estimated f values of all nodes in a given dpq *)
  (fun dpq ->
     Dpq.iter (fun n -> n.est_h <- f_calc n) dpq)



(************************ Search Callers, internal only **********************)
let make_interface sface h_calc f_calc timer =
  let hd = sface.Search_interface.hd in
  let ih,id = hd sface.Search_interface.initial in
  Search_interface.make
    ~resort_expand:(make_expand sface.Search_interface.domain_expand
		      hd timer h_calc f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    {est_h = ih;
     h = ih;
     d = id;
     g = 0.;
     depth = 0;
     q_pos = Dpq.no_position;
     data = sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length))

let call_no_dups sface timer h_calc f_calc =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = make_interface sface h_calc f_calc timer in
    Limit.unwrap_sol5 unwrap_sol
      (Reorderable_best_first.search
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 (make_updater f_calc))


let call_dups sface timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = make_interface sface h_calc f_calc timer in
  (*let queue_rec = Recorders.dpq_recorder
    sface.Search_interface.key_printer (wrap sface.Search_interface.key) in*)
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_best_first.search_dups
	 (*~record:queue_rec*)
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 setpos
	 getpos
	 (make_updater f_calc))

let call_dups_int sface timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = make_interface sface h_calc f_calc timer in
  (*let queue_rec = Recorders.dpq_recorder
    sface.Search_interface.key_printer (wrap sface.Search_interface.key) in*)
      (Reorderable_best_first.search_dups
	 (*~record:queue_rec*)
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 setpos
	 getpos
	 (make_updater f_calc))



let call_drop sface timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates, they are ignored
      [sface] is the domain's search interface
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  (*let queue_rec = Recorders.dpq_recorder
    sface.Search_interface.key_printer (wrap sface.Search_interface.key) in*)
  let search_interface = make_interface sface h_calc f_calc timer in
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_best_first.search_drop_dups
	 (*~record:queue_rec*)
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 setpos
	 getpos
	 (make_updater f_calc))


let call_drop_int sface timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates, they are ignored
      [sface] is the domain's search interface
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  (*let queue_rec = Recorders.dpq_recorder
    sface.Search_interface.key_printer (wrap sface.Search_interface.key) in*)
  let search_interface = make_interface sface h_calc f_calc timer in
      (Reorderable_best_first.search_drop_dups
	 (*~record:queue_rec*)
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 setpos
	 getpos
	 (make_updater f_calc))



(* call the searches with the function *)

let no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_greedy_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    call_no_dups sface Timers.reckless h_calc f_calc


let dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_greedy_correction (fun n -> n.g)
       (fun n -> n.h) (fun n -> n.d) in
       call_dups sface (Timers.reckless) h_calc f_calc

let dups_with_args sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_err = float_of_string args.(0)
  and d_err = float_of_string args.(1)
  and h_calc _ _ _ = () in
  let f_calc n =
    let nd = n.d /. (1. -. d_err) in
      if Math.finite_p nd then n.h +. (h_err *. nd) else infinity in
    assert(d_err <= 1.);
    call_dups sface (Timers.reckless) h_calc f_calc

let seeded_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_err = float_of_string args.(0)
  and d_err = float_of_string args.(1) in
let h_calc,f_calc = Global_hd_ss.make_seeded_correction 100 h_err d_err
  (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
  call_dups sface (Timers.reckless) h_calc f_calc



let drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)

  let h_calc,f_calc = Global_hd_ss.make_greedy_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    call_drop sface Timers.reckless h_calc f_calc


let drop_with_args sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_err = float_of_string args.(0)
  and d_err = float_of_string args.(1)
  and h_calc _ _ _ = () in
  let f_calc n =
    let nd = n.d /. (1. -. d_err) in
      if Math.finite_p nd then n.h +. (h_err *. nd) else infinity in
    assert(d_err <= 1.);
    call_drop sface (Timers.reckless) h_calc f_calc


let seeded_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_err = float_of_string args.(0)
  and d_err = float_of_string args.(1) in
  let h_calc,f_calc = Global_hd_ss.make_seeded_correction 100 h_err d_err
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
    call_drop sface Timers.reckless h_calc f_calc


let speedy_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_speedy_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    call_no_dups sface Timers.reckless h_calc f_calc


let speedy_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_speedy_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    call_dups sface (Timers.reckless) h_calc f_calc


let speedy_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_speedy_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    call_drop sface Timers.reckless h_calc f_calc


let speedy_drop_int sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_speedy_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    call_drop_int sface Timers.reckless h_calc f_calc


(************************* Now we use LMS ***********************************)

let print_weights_on_goal sface get_weights =
  {sface with Search_interface.goal_p =
      (fun n -> let v = sface.Search_interface.goal_p n in
	 if v
	 then
	   (let warray = get_weights () in
	      Datafile.write_pairs
		stdout
		["h_weight", string_of_float warray.(0);
		 "d_weight", string_of_float warray.(1);
		 "g_weight", string_of_float warray.(2);
		 "c_weight", string_of_float warray.(3)]);
	 v)}

let lms_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_correction i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
  let sface = print_weights_on_goal sface gw in
    call_no_dups sface Timers.reckless h_calc f_calc


let lms_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_correction i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
  let sface = print_weights_on_goal sface gw in
    call_dups sface (Timers.reckless) h_calc f_calc


let lms_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_correction i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
  let sface = print_weights_on_goal sface gw in
    call_drop sface Timers.reckless h_calc f_calc


let dlms_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;
			  Search_args.get_float "Greedy Lms Dups" args 4;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_wdepth i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    maxes.(Norm_values.depth) (fun n -> n.g) (fun n -> n.h)
    (fun n -> n.d) (fun n -> float n.depth) in
  let sface = print_weights_on_goal sface gw in
    call_no_dups sface Timers.reckless h_calc f_calc


let dlms_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.; 0.; |]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;
			  Search_args.get_float "Greedy Lms Dups" args 4;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_wdepth i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    maxes.(Norm_values.depth) (fun n -> n.g) (fun n -> n.h)
    (fun n -> n.d) (fun n -> float n.depth) in
  let sface = print_weights_on_goal sface gw in
    call_dups sface (Timers.reckless) h_calc f_calc


let dlms_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;
			  Search_args.get_float "Greedy Lms Dups" args 4;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_wdepth i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    maxes.(Norm_values.depth) (fun n -> n.g) (fun n -> n.h)
    (fun n -> n.d) (fun n -> float n.depth) in
  let sface = print_weights_on_goal sface gw in
    call_drop sface Timers.reckless h_calc f_calc


let hlms_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let ih = sface.Search_interface.h sface.Search_interface.initial in
  let maxes = [| 2.5 *. ih; 2.5 *. ih; 1.|] in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.;|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_justh i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) (fun n -> n.g) (fun n -> n.h) in
  let sface = print_weights_on_goal sface gw in
    call_no_dups sface Timers.reckless h_calc f_calc


let hlms_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let ih = sface.Search_interface.h sface.Search_interface.initial in
  let maxes = [| 2.5 *. ih; 2.5 *. ih; 1.|] in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; |]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_justh i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) (fun n -> n.g) (fun n -> n.h) in
  let sface = print_weights_on_goal sface gw in
    call_dups sface (Timers.reckless) h_calc f_calc


let hlms_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let ih = sface.Search_interface.h sface.Search_interface.initial in
  let maxes = [| 2.5 *. ih; 2.5 *. ih; 1.|] in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; |]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;|]) in
  let h_calc,f_calc,gw = Lms_h.make_greedy_justh i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) (fun n -> n.g) (fun n -> n.h) in
  let sface = print_weights_on_goal sface gw in
    call_drop sface Timers.reckless h_calc f_calc


(************************* Now we use rev_h ***********************************)

let revh_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let rh = wrap sface.Search_interface.rev_h in
  let h_calc,f_calc = Rev_h_nomemory.make_greedy_correction (fun n -> n.g)
    rh (fun n -> n.h) in
    call_no_dups sface Timers.reckless h_calc f_calc


let revh_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let rh = wrap sface.Search_interface.rev_h in
  let h_calc,f_calc = Rev_h_nomemory.make_greedy_correction (fun n -> n.g)
    rh (fun n -> n.h) in
    call_dups sface (Timers.reckless) h_calc f_calc


let revh_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let rh = wrap sface.Search_interface.rev_h in
  let h_calc,f_calc = Rev_h_nomemory.make_greedy_correction (fun n -> n.g)
    rh (fun n -> n.h) in
    call_drop sface Timers.reckless h_calc f_calc

(************************* Now we use rev_h lms *******************************)

let r_feat rhd n =
  let h,d = rhd n in
  [|h; d; n.h; 1.|]

let f_feat n =
  [|n.h; n.d; n.g; 1.|]

and i_feat = [|1.; 0.; 0.; 0.;|]

let print_rweights_on_goal sface get_weights =
  {sface with Search_interface.goal_p =
      (fun n -> let v = sface.Search_interface.goal_p n in
	 if v
	 then
	   (let warray = get_weights () in
	      Datafile.write_pairs
		stdout
		["h_weight", string_of_float warray.(0);
		 "d_weight", string_of_float warray.(1);
		 "g_weight", string_of_float warray.(2);
		 "c_weight", string_of_float warray.(3)]);
	 v)}


let lms_revh_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5
  and rh = wrap sface.Search_interface.rev_hd in
  let feat = r_feat rh in
  let h_calc,f_calc,gw = Lms_rev_h.make_greedy_correction
    maxes feat f_feat i_feat (fun n -> n.g) (fun n -> n.h) in
  let sface = print_rweights_on_goal sface gw in
    call_no_dups sface Timers.reckless h_calc f_calc


let lms_revh_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5
  and rh = wrap sface.Search_interface.rev_hd in
  let feat = r_feat rh in
  let h_calc,f_calc,gw = Lms_rev_h.make_greedy_correction
    maxes feat f_feat i_feat (fun n -> n.g) (fun n -> n.h) in
  let sface = print_rweights_on_goal sface gw in
    call_dups sface (Timers.reckless) h_calc f_calc


let lms_revh_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5
  and rh = wrap sface.Search_interface.rev_hd in
  let feat = r_feat rh in
  let h_calc,f_calc,gw = Lms_rev_h.make_greedy_correction
    maxes feat f_feat i_feat (fun n -> n.g) (fun n -> n.h) in
  let sface = print_rweights_on_goal sface gw in
    call_drop sface Timers.reckless h_calc f_calc


(************************* Now we use ANNs ***********************************)

let feat n = [| n.h; n.d; n.g; 1.|]

let ann_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let h_calc,f_calc = (Ann_hd.make_greedy_correction maxes
			 maxes.(Norm_values.h) feat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_no_dups sface Timers.reckless h_calc f_calc


let ann_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let h_calc,f_calc = (Ann_hd.make_batch_delay_greedy_correction 100
			 maxes
			 maxes.(Norm_values.h) feat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_dups sface (Timers.reckless) h_calc f_calc

let rev_ann_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5
  and rh = wrap sface.Search_interface.rev_hd in
  let h_calc,f_calc = (Ann_hd.make_batch_delay_greedy_correction 100
			 maxes
			 maxes.(Norm_values.h) (r_feat rh)
			 (fun n -> n.g) (fun n -> n.h)) in
    call_dups sface (Timers.reckless) h_calc f_calc

let rev_ann_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5
  and rh = wrap sface.Search_interface.rev_hd in
  let h_calc,f_calc = (Ann_hd.make_batch_delay_greedy_correction 100
			 maxes
			 maxes.(Norm_values.h) (r_feat rh)
			 (fun n -> n.g) (fun n -> n.h)) in
    call_drop sface (Timers.reckless) h_calc f_calc

let ann_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let h_calc,f_calc = (Ann_hd.make_greedy_correction maxes
			 maxes.(Norm_values.h) feat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_drop sface Timers.reckless h_calc f_calc

(************************* Now we use ANNs with depth ************************)

let dfeat n = [| n.h; n.d; n.g; float n.depth; 1.|]

let dann_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let h_calc,f_calc = (Ann_hd.make_greedy_correction maxes
			 maxes.(Norm_values.h) dfeat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_no_dups sface Timers.reckless h_calc f_calc


let dann_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let h_calc,f_calc = (Ann_hd.make_batch_delay_greedy_correction 100
			 maxes
			 maxes.(Norm_values.h) dfeat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_dups sface (Timers.reckless) h_calc f_calc


let dann_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let h_calc,f_calc = (Ann_hd.make_greedy_correction maxes
			 maxes.(Norm_values.h) dfeat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_drop sface Timers.reckless h_calc f_calc


(************************* Now we use ANNs with depth ************************)

let hfeat n = [| n.h; n.g; 1.|]

let hann_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let ih = sface.Search_interface.h sface.Search_interface.initial in
  let maxes = [| 2.5 *. ih; 2.5 *. ih; 1.|] in
  let h_calc,f_calc = (Ann_hd.make_greedy_correction maxes
			 maxes.(Norm_values.h) hfeat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_no_dups sface Timers.reckless h_calc f_calc


let hann_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let ih = sface.Search_interface.h sface.Search_interface.initial in
  let maxes = [| 2.5 *. ih; 2.5 *. ih; 1.|] in
  let h_calc,f_calc = (Ann_hd.make_batch_delay_greedy_correction 100
			 maxes
			 maxes.(Norm_values.h) hfeat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_dups sface (Timers.reckless) h_calc f_calc


let hann_drop sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let ih = sface.Search_interface.h sface.Search_interface.initial in
  let maxes = [| 2.5 *. ih; 2.5 *. ih; 1.|] in
  let h_calc,f_calc = (Ann_hd.make_greedy_correction maxes
			 maxes.(Norm_values.h) hfeat
			 (fun n -> n.g) (fun n -> n.h)) in
    call_drop sface Timers.reckless h_calc f_calc


(******************************************************************************)
let lms_no_dups_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;|]) in
  let _,f_calc,gw = Lms_h.make_greedy_correction i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
  let sface = print_weights_on_goal sface gw in
    call_no_dups sface Timers.reckless (fun _ _ _ -> ()) f_calc


let lms_dups_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;|]) in
  let _,f_calc,gw = Lms_h.make_greedy_correction i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
  let sface = print_weights_on_goal sface gw in
    call_dups sface (Timers.reckless) (fun _ _ _ -> ()) f_calc


let lms_drop_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  (*let maxes = Norm_values.get_maxes sface.Search_interface.domain in*)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Greedy Lms Dups" args 0;
			  Search_args.get_float "Greedy Lms Dups" args 1;
			  Search_args.get_float "Greedy Lms Dups" args 2;
			  Search_args.get_float "Greedy Lms Dups" args 3;|]) in
  let _,f_calc,gw = Lms_h.make_greedy_correction i_weights
    maxes.(Norm_values.h) maxes.(Norm_values.g) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
  let sface = print_weights_on_goal sface gw in
    call_drop sface Timers.reckless (fun _ _ _ -> ()) f_calc

(*************************** ANN No Dups No Learn ****************************)

let ann_no_dups_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let ann_string = Search_args.get_string "Greedy Ann NoLearn" args 0 in
  let h_calc,f_calc = (Ann_hd.make_fixed_greedy_correction maxes
			 maxes.(Norm_values.h) feat
			 (fun n -> n.g) (fun n -> n.h) ann_string) in
    call_no_dups sface Timers.reckless h_calc f_calc


let ann_dups_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let ann_string = Search_args.get_string "Greedy Ann NoLearn" args 0 in
  let h_calc,f_calc = (Ann_hd.make_fixed_greedy_correction maxes
			 maxes.(Norm_values.h) feat
			 (fun n -> n.g) (fun n -> n.h) ann_string) in
    call_dups sface (Timers.reckless) h_calc f_calc


let ann_drop_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let ann_string = Search_args.get_string "Greedy Ann NoLearn" args 0 in
  let h_calc,f_calc = (Ann_hd.make_fixed_greedy_correction maxes
			 maxes.(Norm_values.h) feat
			 (fun n -> n.g) (fun n -> n.h) ann_string) in
    call_drop sface Timers.reckless h_calc f_calc


(************************* LMSHD2: THE REVENGENING *************************)
let lms_dups_2 sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  let h_calc, f_calc, gw = Lms_hd_m2.make_unbounded_lms_correction
    ((fun n -> [|n.h; n.g; n.d; 1.|]), 4) (fun n -> n.g) maxes in
  let sface = print_weights_on_goal sface gw in
    call_dups sface (Timers.reckless) h_calc f_calc

let lms_dd_2 sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let maxes = Norm_values.wd_norm_from_sface sface 2.5 in
  let h_calc, f_calc, gw = Lms_hd_m2.make_unbounded_lms_correction
    ((fun n -> [|n.h; n.g; n.d; 1.|]), 4) (fun n -> n.g) maxes in
  let sface = print_weights_on_goal sface gw in
    call_drop sface (Timers.reckless) h_calc f_calc

(* EOF *)
