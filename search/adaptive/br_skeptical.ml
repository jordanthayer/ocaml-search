(**

    @author jtd7
    @since 2011-03-16

   Skeptical search using the weights learned from laso_br
*)

type 'a node = {
  data : 'a;
  g : float;
  f : float;
  cost : float;
  depth : float;
  mutable ppos : int;
  mutable fpos : int;
}


let do_calc wts features =
  (** Calculates the scalar that is a result of applying the weight vector to
      the feature vector. *)
  fst
    (Array.fold_left
       (fun (product,index) wt ->
	  (product +. wt *. features.(index), index + 1)) (0.,0) wts)

let ordered_p a b =
  a.cost > b.cost ||
    ((a.cost = b.cost) && a.g < b.g)
let better_p a b = a.g < b.g
and ordered_f a b = a.f < b.f
and set_pq_pos n i = n.ppos <- i
and get_p_pos n = n.ppos
and set_f_pos n i = n.fpos <- i
and get_f_pos n = n.fpos
and wrap fn = (fun n -> fn n.data)

let make_expand ?(get_features = (fun p c -> [||])) ?(weights = [||])
    hfun exp_fn =
  (fun n ->
     let depth' = n.depth +. 1. in
       List.map (fun (data,g) ->
		   let h = hfun data in
		   let dummy =
		     {data = data;
		      g = g;
		      f = g +. h;
		      depth = depth';
		      cost = nan;
		      ppos = Dpq.no_position;
		      fpos = Dpq.no_position;} in
		     {data = data;
		      g = g;
		      f = g +. h;
		      depth = depth';
		      cost = do_calc weights (get_features n dummy);
		      ppos = Dpq.no_position;
		      fpos = Dpq.no_position;})
	 (exp_fn n.data n.g))


let make_get_standard_features sface =
  let hd = sface.Search_interface.hd in
  let max_h, max_d = hd sface.Search_interface.initial in
    (fun parent child ->
       let h,d = hd child.data
       and ph, pd = hd parent.data
       and c = child.g -. parent.g in
       let cf = child.f
       and pf = parent.f in
	 [| child.g /. max_h ;
	    (cf -. pf) /. max_h;
	    (pd -. d) /. max_d;
	    h /. max_h;
	    d /. max_d;
	    c /. max_h|])


let make_get_unit_features sface =
  let h = sface.Search_interface.h in
  let max_h = h sface.Search_interface.initial in
    (fun parent child ->
       let h = child.f -. child.g
       and c = child.g -. parent.g in
       let cf = child.f
       and pf = parent.f in
	 [| child.g /. max_h ;
	    (cf -. pf) /. max_h;
	    h /. max_h; c /. max_h|])



let make_sface weights get_features sface =
  let def_log = (Limit.make_default_logger (fun n -> n.g)
		   (fun n -> truncate n.depth)) in
    Search_interface.make
      ~node_expand:(make_expand ~get_features ~weights
		      sface.Search_interface.h
		      sface.Search_interface.domain_expand)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	g = 0.; f = 0.; cost = 0.; depth = 0.;
	ppos = Dpq.no_position; fpos = Dpq.no_position;}

      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let get_node fq pq i bound =
  (** Returns the next node to be expanded *)
  let fn = Dpq.peek_first fq
  and incumbent = i.Limit.incumbent in
    match incumbent with
	Limit.Nothing -> raise Optimistic_framework.NoIncumbent
      | Limit.Incumbent(qual,inc) ->
	  if (fn.f *. bound) >= (inc.f)
	  then raise Optimistic_framework.Done;
	  (let trf = fn.fpos
	   and trp = fn.ppos in
	     Dpq.remove fq trf;
	     Dpq.remove pq trp;
	     fn)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let do_search sface args =
  let bound = Search_args.get_float "Br_skeptical.do_search" args 0
  and weights = Load_laso_weights.get_weight_vector sface 100 in
  let features = (match sface.Search_interface.domain with
		    | Search_interface.Dock_robot
		    | Search_interface.Heavy_vacuum
		    | Search_interface.Vacuum
		    | Search_interface.Inv_tiles
		    | Search_interface.LGrid -> make_get_standard_features sface
		    | _ -> make_get_unit_features sface) in
  let sface = make_sface weights features sface in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.dups sface get_node ordered_p bound better_p
	 ordered_f set_pq_pos set_f_pos get_p_pos get_f_pos)

(* EOF *)
