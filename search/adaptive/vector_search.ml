(** Search code using offline learned information in the form of arbitrary
    vectors *)

let h_ind = 0
and d_ind = 1
and g_ind = 2
and depth_ind = 3
and h_rev = 4
and d_rev = 5


type 'a node =
    { data : 'a;
      features : float array;
      cost : float;
      f : float;
      mutable pos: int;}


let setpos n i =
  (** Updates the [pos] of node [n], setting it to [i] *)
  n.pos <- i


let getpos n =
  (** returns the current [pos] of node [n] *)
  n.pos


let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  a.features.(g_ind) <= b.features.(g_ind)


let ordered_p a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      high g values *)
  a.cost < b.cost ||
    (a.cost = b.cost &&
       (not (better_p a b)))


let unwrap_sol n =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match n with
    Limit.Nothing -> None
  | Limit.Incumbent (q, n) -> Some (n.data, n.features.(g_ind))


let calc_cost vector features =
  (** taking a weight [vector] and a [features] array, returs an
      estimate of the true cost to go from this node *)
  (assert ((Array.length vector) <= (Array.length features)));
  let sum = ref 0. in
    for i = 0 to ((Array.length vector) - 1)
    do
      sum := !sum +. vector.(i) *. features.(i);
    done;
    !sum


let wrap_expand expand hd rev_hd vector =
  (** Returns an expand function to be used by optimistic framework
      requires the domain [expand] a cost and distance heuristic estimator
      [hd] and the weight [vector] which was learned offline *)
  (fun n ->
     let nd = n.features.(depth_ind) +. 1. in
       List.map (fun (c,g) ->
		   (let (h,d) = hd c
		    and (rh,rd) = rev_hd c in
		    let feat = [| h; g; d; nd; rh; rd;|] in
		      { data = c;
			features = feat;
			cost = calc_cost vector feat;
			f = feat.(g_ind) +. feat.(h_ind);
			pos = Dpq.no_position;}))
	 (expand n.data n.features.(g_ind)))


let make_init data =
  (** returns the root of the search space *)
  { data = data;
    features = [| infinity; infinity; 0.; 0.|];
    cost = neg_infinity;
    f = neg_infinity;
    pos = Dpq.no_position;}


let wrap fn =
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> fn n.data)

(******************************** Searches *********************************)

let alt_col_name = "wt_vector"
let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name
    ["h"; "d"; "g"; "depth"; "rev_h"; "rev_d";]

let output_vector v =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%f\t%f\t%f\t%f\t%f\n"
    v.(h_ind) v.(d_ind) v.(g_ind) v.(depth_ind) v.(h_rev) v.(d_rev)


let output v =
  output_col_hdr ();
  output_vector v

let no_dups sface vector =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  output vector;
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd
		    sface.Search_interface.rev_hd vector)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 ordered_p
	 better_p)


let dups sface vector =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  output vector;
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd
		    sface.Search_interface.rev_hd vector)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 search_interface
	 ordered_p
	 better_p
	 setpos
	 getpos)


let drop_dups sface vector =
  (** Performs a search in domains where there are no duplicates.
      Duplicate states are never reexamined.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  output vector;
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd
		    sface.Search_interface.rev_hd vector)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups
	 search_interface
	 ordered_p
	 better_p
	 setpos
	 getpos)


(* EOF *)
