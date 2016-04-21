(**

   @author jtd7
   @since 2011-03-08

   An implementation of LaSO-BR as describe in
   "On Learning Linear Ranking Functions For Beam Search", figure 3
*)

type 'a node = {
  data : 'a;
  g : float;
  cost : float;
  depth : float;
mutable qpos : int;
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
let setpos n i = n.qpos <- i
and getpos n = n.qpos

let make_expand ?(get_features = (fun p c -> [||])) ?(weights = [||]) exp_fn =
  (fun n ->
     let depth' = n.depth +. 1. in
       List.map (fun (data,g) ->
		   let dummy =
		     {data = data;
		      g = g;
		      depth = depth';
		      cost = nan;
		      qpos = Dpq.no_position;} in
		     {data = data;
		      g = g;
		      depth = depth';
		      cost = do_calc weights (get_features n dummy);
		      qpos = Dpq.no_position;})
	 (exp_fn n.data n.g))


let wrap fn = (fun n -> fn n.data)


let make_get_standard_features sface =
  let hd = sface.Search_interface.hd in
  let max_h, max_d = hd sface.Search_interface.initial in
    (fun parent child ->
       let h,d = hd child.data
       and ph,pd = hd parent.data
       and c = child.g -. parent.g in
       let cf = child.g +. h
       and pf = parent.g +. ph in
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
       let h = h child.data
       and c = child.g -. parent.g
       and ph = h parent.data in
       let cf = child.g +. h
       and pf = parent.g +. ph in
	 [| child.g /. max_h ;
	    (cf -. pf) /. max_h;
	    h /. max_h; c /. max_h|])

let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let call_search weights get_features beam_width sface =
  let key = wrap sface.Search_interface.key
  and goal = wrap sface.Search_interface.goal_p
  and root = { data = sface.Search_interface.initial;
	       g = 0.; cost = 0.; depth = 0.; qpos = Dpq.no_position;}
  and expand = (make_expand ~get_features ~weights
		  sface.Search_interface.domain_expand)
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
		(Limit.make_default_logger (fun n -> n.g)
		   (fun n -> truncate n.depth))) in
    Generic_beam.breadth_first_dups ~dd:true info beam_width root goal expand
      ordered_p better_p sface.Search_interface.hash
      sface.Search_interface.equals key setpos getpos;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let do_search sface args =
  let beam_width = Search_args.get_int "Laso_br" args 0 in
  let weights = Load_laso_weights.get_weight_vector sface beam_width in
  let features = (match sface.Search_interface.domain with
		    | Search_interface.Dock_robot
		    | Search_interface.Heavy_vacuum
		    | Search_interface.Vacuum
		    | Search_interface.Inv_tiles
		    | Search_interface.LGrid -> make_get_standard_features sface
		    | _ -> make_get_unit_features sface) in
    call_search weights features beam_width sface



(* EOF *)
