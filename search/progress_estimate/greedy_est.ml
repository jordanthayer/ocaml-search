(**

    @author jtd7
    @since 2012-03-22
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  h : float;          (* Heuristic at node*)
  d : float;
  g : float;          (* Cost of reaching a node *)
  depth : float;
  generated : int;
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


let h_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  (a.h < b.h) ||
  ((a.h = b.h) && (a.g >= b.g))

let d_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  (a.d < b.d) ||
    ((a.d = b.d) && (a.h < b.h)) ||
    (a.d = b.d && a.h = b.h && (a.g >= b.g))


let just_g a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand i expand hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let nd = n.depth +. 1. in
     List.map (fun (data, g) ->
		 let h,d = hd data in
		 { data = data;
		   h = h;
		   d = d;
		   g = g;
		   depth = nd;
		   generated = i.Limit.expanded;
		   pos = Dpq.no_position; }) (expand n.data n.g))


let search ?(est = (fun i n oplst child -> fun _ -> nan, nan))
    ?(sort = h_then_g)
    i key hash equals goal expand initial =
  let max_guess = 200 in
  let openlist = Dpq.create sort setpos max_guess initial
  and nodes = Htable.create hash equals max_guess in
  let sample, fop, output = Progress_est.make_output_stream
    Progress_est.output_row in

  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (let state = key n in
	    try
	      let prev = Htable.find nodes state in
		Limit.incr_dups i;
		if n.g < prev.g
		then (Htable.replace nodes state n;
		      let pos = getpos prev in
			if pos != Dpq.no_position
			then Dpq.swap openlist pos n)
	    with Not_found -> (* new state *)
	      Dpq.insert openlist n;
	      Htable.add nodes state n) in

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
    then (let n = Dpq.extract_first openlist in
	    (* here is where you would do the estimation I guess. *)
	    setpos n Dpq.no_position;
	    if not (Limit.promising_p i n)
	    then (Limit.incr_prune i;
		  Htable.remove nodes (key n);
		  expand_best ())
	    else if goal n
	    then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		  fop ();
		  Progress_est.output_row
		    i.Limit.expanded (truncate n.depth) (0.,1.))
	    else (let children = expand n in
		    sample i.Limit.expanded (truncate n.depth)
		      (est i n openlist children);
		    Limit.incr_exp i;
		    List.iter consider_child children;
		    Limit.curr_q i (Dpq.count openlist);
		    expand_best ()))
  in
    Progress_est.output_row 0 0 (initial.d, 0.);
    consider_child initial;
    expand_best ();
    i.Limit.log i;
    Limit.results6 i


let make_init hd init =
  let h,d = hd init in
    {h = h; d = d; g = 0.; depth = 0.; generated = 0;
     pos = Dpq.no_position; data = init}
