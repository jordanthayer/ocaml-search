(**

    @author jtd7
    @since 2011-02-15
   Search Interfaces for grids.
*)
open Grid

let get_default_heuristics w =
  let rev_board = reverse_board w in
  let h = get_cheapest_h w
  and d = get_cheapest_d w
  and hd = get_cheapest_hd w
  and rev_h = get_cheapest_h rev_board
  and rev_d = get_cheapest_d rev_board
  and rev_hd = get_cheapest_hd rev_board
  and hd1 = get_hd_1 w
  and rhd1 = get_hd_1 rev_board in
  let h1 = (fun n -> fst (hd1 n))
  and d1 = (fun n -> snd (hd1 n))
  and rh1 = (fun n -> fst (rhd1 n))
  and rd1 = (fun n -> snd (rhd1 n)) in
  let nearest_h = get_nearest_h w
  and nearest_d = get_nearest_d w
  and nearest_hd = get_nearest_hd w in
  let default_props = [Heuristic.Admissible;
		       Heuristic.Consistent;
		       Heuristic.Cheapest;]
  and nearest = [Heuristic.Admissible;
		       Heuristic.Consistent;
		       Heuristic.Nearest;] in
  let forwards = Heuristic.Forwards::default_props
  and backwards = Heuristic.Backwards::default_props in
    { Heuristic.hd = [{ Heuristic.name = "Manhattan HD";
			properties = forwards;
			heuristic = hd;};
		      { Heuristic.name = "Nearest Manhattan HD";
			properties = nearest;
			heuristic = nearest_hd;};
		      { Heuristic.name = "Manhattan HD";
			properties = backwards;
			heuristic = rev_hd;};
		      { Heuristic.name = "Constantly 1";
			properties = forwards;
			heuristic = hd1; };];
      fixed = [{ Heuristic.name = "Manhattan";
		 properties = Heuristic.Cost::forwards;
		 heuristic = h;};
	       { Heuristic.name = "Nearest Manhattan";
		 properties = Heuristic.Cost::Heuristic.Forwards::nearest;
		 heuristic = nearest_h;};
	       { Heuristic.name = "Constantly 1";
		 properties = Heuristic.Cost::forwards;
		 heuristic = h1; };
	       { Heuristic.name = "Manhattan";
		 properties = Heuristic.Distance::forwards;
		 heuristic = d;};
	       { Heuristic.name = "Nearest Manhattan";
		 properties = Heuristic.Distance::Heuristic.Forwards::nearest;
		 heuristic = nearest_d;};
	       { Heuristic.name = "Constantly 1";
		 properties = Heuristic.Distance::forwards;
		 heuristic = d1; };
	       { Heuristic.name = "Manhattan";
		 properties = Heuristic.Cost::backwards;
		 heuristic = rev_h;};
	       { Heuristic.name = "Constantly 1";
		 properties = Heuristic.Cost::backwards;
		 heuristic = rh1;};
	       { Heuristic.name = "Manhattan";
		 properties = Heuristic.Distance::backwards;
		 heuristic = rev_d;};
	       { Heuristic.name = "Constantly 1";
		 properties = Heuristic.Distance::backwards;
		 heuristic = rd1;};
	       { Heuristic.name = "Count Blocked";
		 properties = Heuristic.Cost::[Heuristic.Inadmissible];
		 heuristic = Grid_inadmiss.select_h w };
	       { Heuristic.name = "Count Blocked";
		 properties = Heuristic.Distance::[Heuristic.Inadmissible];
		 heuristic = Grid_inadmiss.select_d w };
	       { Heuristic.name = "Count Blocked";
		 properties = Heuristic.Cost::[Heuristic.Inadmissible];
		 heuristic = Grid_inadmiss.scaled_h w };
	       { Heuristic.name = "Count Blocked";
		 properties = Heuristic.Distance::[Heuristic.Inadmissible];
		 heuristic = Grid_inadmiss.scaled_d w };
              ];
      between = [];
      between_hd = [];}


let default_interface w lim =
  let rev_board = reverse_board w in
  let h = get_cheapest_h w
  and d = get_cheapest_d w
  and hd = get_cheapest_hd w
  and rev_h = get_cheapest_h rev_board
  and rev_d = get_cheapest_d rev_board
  and rev_hd = get_cheapest_hd rev_board in
  let heuristics = get_default_heuristics w in
  let i =
    Search_interface.make
      ~h
      ~d
      ~hd
      ~rev_h
      ~rev_d
      ~rev_hd
      ~heuristics
      ~domain_expand:(make_expand w)
      ~predecessor:(make_expand_wp w)
      ~key:key
      ~key_print:key_to_string
      ~equals:equals
      ~goal_p:(make_goal_p w)
      ~halt_on:lim
      ~get_sol_length:sol_length
      ~p_update:update_parent
      (get_type w)
      (make_root w)
      (fun _ _ -> false)
      (fun _ -> ())
  in
  let cost_between, dist_between = get_between_heuristics w in
    Search_interface.alter ~cost_between ~dist_between i


let default_from_string path lim =
  let w = Grid_instance.load path in
    default_interface w lim


let constantly_one w lim =
  let rev_board = reverse_board w in
  let hd = get_hd_1 w in
  let heuristics = get_default_heuristics w in
  let i =
    Search_interface.make
      ~h:(fun n -> fst (hd n))
      ~d:(fun n -> snd (hd n))
      ~hd
      ~heuristics:(Heuristic.by_name heuristics "Constantly 1")
      ~rev_h:(get_cheapest_h rev_board)
      ~rev_d:(get_cheapest_d rev_board)
      ~rev_hd:(get_cheapest_hd rev_board)
      ~domain_expand:(make_expand w)
      ~predecessor:(make_expand_wp w)
      ~key:key
      ~key_print:key_to_string
      ~equals:equals
      ~goal_p:(make_goal_p w)
      ~halt_on:lim
      ~get_sol_length:sol_length
      ~p_update:update_parent
      (get_type w)
      (make_root w)
      (fun _ _ -> false)
      (fun _ -> ())
  in
  let cost_between, dist_between = get_between_heuristics w in
    Search_interface.alter ~cost_between ~dist_between i


let wp_interface w lim =
  let rev_board = reverse_board w in
    (Search_interface.make
       ~h:(get_cheapest_h w)
       ~d:(get_cheapest_d w)
       ~hd:(get_cheapest_hd w)
       ~rev_h:(get_cheapest_h rev_board)
       ~rev_d:(get_cheapest_d rev_board)
       ~rev_hd:(get_cheapest_hd rev_board)
       ~domain_expand:(make_expand_wp w)
       ~predecessor:(make_expand_wp w)
       ~key:key
       ~key_print:key_to_string
       ~equals:equals
       ~goal_p:(make_goal_p w)
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       (get_type w)
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let avg_vect_interface model w lim =
  let rev_board = reverse_board w in
    (Search_interface.make
       ~h:(get_cheapest_h w)
       ~d:(get_cheapest_d w)
       ~hd:(get_cheapest_hd w)
       ~rev_h:(get_cheapest_h rev_board)
       ~rev_d:(get_cheapest_d rev_board)
       ~rev_hd:(get_cheapest_hd rev_board)
       ~domain_expand:(make_expand w)
       ~key:key
       ~key_print:key_to_string
       ~equals:equals
       ~goal_p:(make_goal_p w)
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~wt_vect:( get_avg_vector model w)
       (get_type w)
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let nearest_interface w lim =
  (Search_interface.make
    ~h:(get_cheapest_h w)
    ~d:(get_nearest_d w)
    ~hd:(get_nearest_hd w)
    ~domain_expand:(make_expand w)
    ~key:key
    ~key_print:key_to_string
    ~equals:equals
    ~goal_p:(make_goal_p w)
    ~halt_on:lim
    ~get_sol_length:sol_length
    (get_type w)
    (make_root w)
    (fun _ _ -> false)
    (fun _ -> ()))


let expand_parent_interface w lim =
  (Search_interface.make
     ~h:(get_cheapest_h w)
     ~d:(get_cheapest_d w)
     ~hd:(get_cheapest_hd w)
     ~domain_expand:(make_expand_wp w)
     ~key:key
     ~key_print:key_to_string
     ~equals:equals
     ~goal_p:(make_goal_p w)
     ~halt_on:lim
     ~get_sol_length:sol_length
     (get_type w)
     (make_root w)
     (fun _ _ -> false)
     (fun _ -> ()))


let record_interface w lim =
  let rev_board = reverse_board w in
    (Search_interface.make
       ~h:(get_cheapest_h w)
       ~d:(get_cheapest_d w)
       ~hd:(get_cheapest_hd w)
       ~rev_h:(get_cheapest_h rev_board)
       ~rev_d:(get_cheapest_d rev_board)
       ~rev_hd:(get_cheapest_hd rev_board)
       ~domain_expand:(make_expand w)
       ~key:key
       ~key_print:key_to_string
       ~equals:equals
       ~goal_p:(fun _ -> false)
       ~halt_on:lim
       ~get_sol_length:sol_length
       (get_type w)
       (make_root rev_board)
       (fun _ _ -> false)
       (fun _ -> ()))


let exhaust_interface w lim =
  let rev_board = reverse_board w in
    (Search_interface.make
       ~h:(get_cheapest_h w)
       ~d:(get_cheapest_d w)
       ~hd:(get_cheapest_hd w)
       ~rev_h:(get_cheapest_h rev_board)
       ~rev_d:(get_cheapest_d rev_board)
       ~rev_hd:(get_cheapest_hd rev_board)
       ~domain_expand:(make_expand w)
       ~key:key
       ~key_print:key_to_string
       ~equals:equals
       ~goal_p:(fun _ -> false)
       ~halt_on:lim
       ~get_sol_length:sol_length
       (get_type w)
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


(* EOF *)
