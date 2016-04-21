(**

   An implementation of Will and J's hybrid algorithm.

*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  h : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  d : float;          (* Estimated depth to solution *)
  quality : float;
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let just_q a b = 
  (a.quality  : float) <= b.quality

let q_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.quality : float) < b.quality) ||
    ((a.quality = b.quality) && (a.g >= b.g))


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand ratio expand hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.filter (fun n -> n.h < infinity)
     (List.map (fun (d, g) -> 
		  let h_val, d_val = hd d in
(*
		    (*sanity check  for  debigging*)
		    Printf.fprintf stderr "%f %f %f %f\n"
		      g h_val d_val (h_val +. g +. (d_val *. ratio))
		    ;
*)
		  { data = d;
		    h = h_val;
		    g = g;
		    d = d_val;
		    quality = h_val +. g +. (d_val *. ratio);
		    depth = n.depth + 1;
		    pos = Dpq.no_position; }) (expand n.data n.g)))
    

let make_sface sface =
  let initial_h = sface.Search_interface.h
    sface.Search_interface.initial in
  let initial_d = sface.Search_interface.d
    sface.Search_interface.initial in
  let hd_ratio = initial_h /. initial_d in
  let def_log = Limit.make_default_logger (fun n -> n.quality)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand hd_ratio sface.Search_interface.domain_expand
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       h = initial_h;
       g = 0.;
       d = initial_d;
       depth = 0;
       quality = initial_h +. (initial_d *. hd_ratio);
       pos = Dpq.no_position;}
      just_q
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let make_sface_wt sface wt =
  let initial_h = sface.Search_interface.h
    sface.Search_interface.initial in
  let initial_d = sface.Search_interface.d
    sface.Search_interface.initial in

  let hd_ratio = float_of_string wt in

  let def_log = Limit.make_default_logger (fun n -> n.quality)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand hd_ratio sface.Search_interface.domain_expand
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       h = initial_h;
       g = 0.;
       d = initial_d;
       depth = 0;
       quality = initial_h +. (initial_d *. hd_ratio);
       pos = Dpq.no_position;}
      just_q
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let wted_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)

  let search_interface = 
    make_sface_wt sface args.(0) in
    
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 q_then_g
	 just_q
	 setpos
	 getpos)



let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)

  let search_interface = make_sface sface in 
    
    Limit.unwrap_sol6 unwrap_sol
    (Best_first.search_dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       q_then_g
       just_q
       setpos
       getpos)
