(**

    @author jtd7
    @since 2010-10-09
*)


type 'a node = {
  estimates : float array;
  h : float;
  d : float;
  g : float;
  mutable q_pos : int array;   (* for Dpqs *)
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


let h_then_g i a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  a.estimates.(i) < b.estimates.(i) ||
    ((a.estimates.(i) = b.estimates.(i)) && (a.g >= b.g))


let just_g a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g

let setpos i n pos =
  (** Sets the location of a node, used by dpq's *)
  n.q_pos.(i) <- pos


let getpos i n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.q_pos.(i)


let make_expand ?(num_h = 2) expand hd timer calc_h_data f_calc =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let best_f = ref infinity
     and best_child = ref n
     and reorder = timer() in
     let children =
       List.map (fun (d, g) ->
		   let h, d = hd d in
		   let f = g +. h in
		   let c =
		     {estimates = [|h;d|];
		      data = d;
		      h = h;
		      d = d;
		      g = g;
		      q_pos = Array.create num_h Dpq.no_position; } in
		     if  f < !best_f then
		       (best_child := c;
			best_f := f);
		     c)
	 (expand n.data n.g) in
       if not ((List.length children) = 0)
       then
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> let h,d = f_calc c in
		       c.estimates.(0) <- h;
		       c.estimates.(1) <- d) children);
       reorder, children)


let make_updater f_calc =
  (** Updates the estimated f values of all nodes in a given dpq *)
  (fun dpq ->
     Dpq.iter (fun n ->
		 let h,d = f_calc n in
		   n.estimates.(0) <- h;
		   n.estimates.(1) <- d) dpq)



(* EOF *)
