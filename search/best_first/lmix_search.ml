(**

    @author jordan
    @since 2011-06-20

   like size cost search, but is supposed to transition gracefully from search on
   lnearest to search on lcheapest
*)

type kind =
  | Nearest
  | Cheapest
  | Both


type fpv = {
  depth : float;
  g : float;
  f : float;
  mutable score : float;
}


type 'a node = {
  data : 'a;
  fpv : fpv;
  mutable kind : kind;
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
    | Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.fpv.g)


let score_then_f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  let afp = a.fpv
  and bfp = b.fpv in
  (afp.score < bfp.score) ||
    ((afp.score = bfp.score) &&
	((afp.f < bfp.f) ||
	    (afp.f = bfp.f && afp.g > bfp.g)))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.fpv.f : float) <= b.fpv.f

let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i

let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand hd_cheap d_near =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  let lmix_expand alpha n =
    assert (alpha >=0.);
    assert (alpha <= 1.);
    let beta = 1. -. alpha in
    let depth' = n.fpv.depth +. 1. in
    List.map (fun (data, g) ->
      let hc,dc = hd_cheap data in
      let dn = d_near data in
      let f = hc +. g
      and ln = depth' +. dn
      and lc = depth' +. dc in
      let score = alpha *. ln +. beta *. lc in
      let fpv = { depth = depth'; g = g; f = f;
		  score = score; } in
      { data = data;
	pos = Dpq.no_position;
	fpv = fpv;
	kind = Both }) (expand n.data n.fpv.g) in lmix_expand


let search expand key goal hash equals root info =
  let openlist = Dpq.create score_then_f_then_g setpos 100 root
  and nodes = Htable.create hash equals 100
  and alpha = ref 1.
  and dec = 0.2 in

  let insert_both state c prev =
    Htable.replace nodes state [c];
    if prev.pos = Dpq.no_position
    then Dpq.insert openlist c
    else Dpq.swap openlist prev.pos c  in

  let consider_child c =
    Limit.incr_gen info;
    if not (Limit.promising_p info c)
    then Limit.incr_prune info
    else let state = key c in
	 if Htable.mem nodes state
	 then (let (prev: 'a node list) = Htable.find nodes state in
	       match prev with
		 | [p] ->
		   (if c.fpv.g < p.fpv.g
		    then (if c.fpv.depth <= p.fpv.depth
		          then insert_both state c p
		          else (p.kind <- Nearest;
		                c.kind <- Cheapest;
			        Htable.replace nodes state [c; p];
			        Dpq.insert openlist c))
		    else (if c.fpv.depth < p.fpv.depth
			  then (if c.fpv.g = p.fpv.g
			        then insert_both state c p
			        else (p.kind <- Cheapest;
				      c.kind <- Nearest;
				      Htable.replace nodes state [p; c];
				      Dpq.insert openlist c))))
		 | [cheap; near] ->
		   (if c.fpv.g < cheap.fpv.g
		    then (if c.fpv.depth <= near.fpv.depth
		          then (if cheap.pos <> Dpq.no_position
                                then Dpq.remove openlist cheap.pos;
				insert_both state c near)
		          else (if cheap.pos <> Dpq.no_position
                                then Dpq.swap openlist cheap.pos c
			        else Dpq.insert openlist c;
				c.kind <- Cheapest;
				Htable.replace nodes state [c; near]))
		    else (if c.fpv.g = cheap.fpv.g
		          then  (if c.fpv.depth <= near.fpv.depth
			         then(if cheap.pos <> Dpq.no_position
                                      then Dpq.remove openlist cheap.pos;
                                      insert_both state c near))
		          else (if c.fpv.depth < near.fpv.depth
			        then (c.kind <- Nearest;
                                      if near.pos <> Dpq.no_position
				      then Dpq.swap openlist near.pos c
				      else Dpq.insert openlist c;
				      Htable.replace nodes state [cheap; c]))))
		 | _ -> failwith "Unpossible!")
	 else insert_both state c c in


  let rec iterate () =
    if not ((Dpq.empty_p openlist) || (Limit.halt_p info)) then
      (let n = Dpq.extract_first openlist in
       setpos n Dpq.no_position;
       if not (Limit.promising_p info n)
       then (Limit.incr_prune info; iterate ())
       else (if goal n
	     then (Limit.new_incumbent info (Limit.Incumbent (0., n));
		   alpha := !alpha -. dec;
		   if !alpha < 0. then alpha := 0.;
		   iterate())
	     else (Limit.incr_exp info;
		   List.iter consider_child (expand !alpha n);
		   Limit.curr_q info (Dpq.count openlist);
		   iterate()))) in

  Dpq.insert openlist root;
  Htable.add nodes (key root) [root];
  iterate()


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let module H = Heuristic in
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and goal = wrap sface.SI.goal_p
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and root = { data = sface.SI.initial;
	       fpv = { depth = 0.; g = 0.; f = 0.; score = 0.;};
	       kind = Both;
	       pos = Dpq.no_position; }
  and hd_cheap = (H.default_hd sface.SI.heuristics).H.heuristic
  and d_near = H.get_fixed sface.SI.heuristics [H.Nearest; H.Distance] in
  let d_near = (List.hd d_near).H.heuristic in
  let expand = make_expand sface.SI.domain_expand hd_cheap d_near
  and def_log = (Limit.make_default_logger (fun n -> n.fpv.f)
		   (fun n -> truncate n.fpv.depth)) in
  let info = Limit.make Limit.Nothing sface.SI.halt_on just_f def_log in
  search expand key goal hash equals root info;
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


(* EOF *)
