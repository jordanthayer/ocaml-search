(**

    @author jtd7
    @since 2011-05-04
   Based on Roni Stern et al's 2011 ICAPS paper
*)

type 'a node = {
  data : 'a;
  f : float;
  g : float;
  d : float;
  potential : float;
  mutable pos : int;
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
    | Limit.Incumbent (q,n) -> (if n.pos = -1337 then None
				else Some (n.data, n.f))

let potential_then_g a b =
  let dif = b.potential -. a.potential in
  Math.is_zero dif || Math.is_positive dif


let d_then_potential a b =
  a.d < b.d ||
    (a.d = b.d && a.potential <= b.potential)


let better_p a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f

let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos

let make_close_enough bnd =
  let close_enough a b =
    let ap = a.potential
    and bp = b.potential in
    (bp *. bnd) > ap ||
      Math.within ap bp 0.0001 ||
      (not (Math.finite_p ap)) && (not (Math.finite_p bp)) in
  close_enough


let make_expand expand c hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     (List.map (fun (data, g) ->
		  let h,d = hd data in
		  let p = h /. (1. -. (g /. c)) in
		  let f = g +. h in
		  Verb.pe Verb.always "%f %f %f\n %!" g h p;
		    { data = data;
		      f = f;
		      g = g;
		      d = d;
		      potential = p;
		      pos = Dpq.no_position;})
	(expand n.data n.g)))


let make_root init hd =
  let h,d = hd init in
  { data = init;
    f = h;
    g = 0.;
    d = d;
    potential = h;
    pos = Dpq.no_position;}


let make_sface sface cost_bound =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
  let dummy_incumbent = { data = sface.Search_interface.initial;
			  potential = nan;
			  f = cost_bound;
			  g = cost_bound;
			  d = 0.;
			  pos = -1337;} in
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand cost_bound
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      ~incumbent:(Limit.Incumbent (0., dummy_incumbent))
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       f = h;
       g = 0.;
       potential = h;
       d = d;
       pos = Dpq.no_position;}
      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)

let unwrap_sol = function
  | Limit.Incumbent (q, n) -> Some (n.data, n.g)
  | _ -> None


let dups sface args =
  let pot_bound = Search_args.get_float "bounded potential" args 0 in
  let cost_bound = Search_args.get_float "bounded potential" args 1 in
  let sface' = make_sface sface cost_bound in
  Limit.unwrap_sol6 unwrap_sol
    (Focal_search.search_dups
       sface'
       potential_then_g
       (make_close_enough pot_bound)
       potential_then_g
       better_p
       setpos
       getpos)
