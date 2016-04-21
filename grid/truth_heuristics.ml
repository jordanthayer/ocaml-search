open Grid

let board_to_pairs b prob =
  ["obstacles", "uniform";
   "costs", (match b.costs with Unit -> "Unit" | Life -> "Life");
   "moves", (match b.moves with Fourway -> "Four-way" | Eightway -> "Eight-way");
   "prob", prob;
   "num",  string_of_int b.instance;]


let get_true_hd path =
  let truth_ht,_,_ = Run_reader.proc_truth_file Run_reader.grid_key_parse path in
    (fun n ->
       let tv = Hashtbl.find truth_ht (key n) in
	 List.hd tv.Run_reader.true_heuristics,
       List.hd (List.tl tv.Run_reader.true_heuristics))


(* Fetches the true heuristic *)
let get_true_values board p instance_root =
  let heuristic_pairs = ("type", "truths")::(board_to_pairs board p) in
    List.iter (fun (a,b) -> Verb.pe Verb.debug "%s , %s\n%!"a b) heuristic_pairs;
  let paths = Rdb.matching_paths instance_root heuristic_pairs in
    match paths with
	[path] -> get_true_hd path
      | _ -> failwith "No Stored Heuristic"


let corrupt_fixed_percent truth percent =
  (fun n -> let h,d = (truth n) in h *. percent, d *. percent)


let corrupt_random_percent ?(seed = 42) truth max_percent =
  Random.set_state !(Math.random_state_from seed);
  (fun n -> (truth n) *. (Random.float max_percent))


let helmert_roeger truth c =
  let cfloat = float_of_int c in
  (fun n -> Math.fmax ((truth n) -. cfloat) 0.)


let truth_interface ?(iroot = "") ?(p = "") corruption w lim =
  let p = string_of_int (Grid_instance.prob_path_to_instance p) in
  let hd = get_true_values w p iroot in
  let hd = corrupt_fixed_percent hd corruption in
    Verb.pe Verb.toplvl "heuristic loaded\n%!";
    (Search_interface.make
       ~h:(fun n -> let h,_ = hd n in h)
       ~d:(fun n -> let _,d = hd n in d)
       ~hd:hd
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


let true_h_degraded_d ?(iroot = "") ?(p = "") w lim =
  let hd = get_true_values w p iroot
  and d_ad = get_cheapest_d w in
    Verb.pe Verb.toplvl "heuristic loaded\n%!";
    (Search_interface.make
      ~h:(fun n -> let h,_ = hd n in h)
      ~d:d_ad
      ~hd:(fun n -> let h,_ = hd n
		    and d = d_ad n in h,d)
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


let true_d_degraded_h ?(iroot = "") ?(p = "") w lim =
  let hd = get_true_values w p iroot
  and h_ad = get_cheapest_h w in
    Verb.pe Verb.toplvl "heuristic loaded\n%!";
    (Search_interface.make
       ~h:h_ad
       ~d:(fun n -> let _,d = hd n in d)
       ~hd:(fun n -> let _,d = hd n in
	      h_ad n, d)
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


let corrupted_hhat ?(iroot = "") ?(p = "") corruption w lim =
  let perfect_hd = get_true_values w p iroot
  and hd =get_cheapest_hd w in
  let corrupted_hhat = (fun node -> corruption *. (fst (perfect_hd node))) in
    (Search_interface.make
       ~h:corrupted_hhat
       ~hd:hd
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


let make_true_hd_online w =
  (** Solves the problem up front, calculating the true h value *)
  let interface = Grid_interfaces.record_interface w [] in
  let true_h = Hashtbl.create 100 in
  let w_iface = Search_interface.alter
    ~domain_expand:(Some (fun n g ->
			    Hashtbl.replace true_h (key n)
			      (g,
			       if g = 0.
			       then 0.
			       else
				 (snd (Hashtbl.find true_h (key n.parent)))
				 +. 1.);
			    interface.Search_interface.domain_expand n g))
    interface in
    ignore (Uniform_cost_search.dups_silent w_iface [||]);
    (fun n -> Hashtbl.find true_h (key n))


let online_truth_interface w lim =
  let true_hd = make_true_hd_online w in
  let h = (fun n -> fst (true_hd n))
  and d = (fun n -> snd (true_hd n)) in
    (Search_interface.make
       ~h:h
       ~d:d
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


let online_corrupted_interface c w lim =
  Random.init 314159;
  let true_hd = make_true_hd_online w in
  let corrupted_hd = (fun n -> let h,d = true_hd n in
			h *. (1. -. (Random.float c)),
			d *. (1. -. (Random.float c))) in
  let hd = Grid.get_cheapest_hd w in
  let h = (fun n -> fst (corrupted_hd n))
  and d = (fun n -> snd (corrupted_hd n)) in
    (Search_interface.make
       ~h:h
       ~d:d
       ~hd:corrupted_hd
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



let online_bandwidth_interface bandwidth w lim =
  Random.init 314159;
  let true_hd = make_true_hd_online w in
  let corrupted_hd n =
    let rand = (Random.float 2.) -. 1. in
    let h,d = true_hd n in
    let err = rand *. bandwidth in
      h +. err, d +. err in
  let hd = Grid.get_cheapest_hd w in
  let h = (fun n -> fst (corrupted_hd n))
  and d = (fun n -> snd (corrupted_hd n)) in
    (Search_interface.make
       ~h:h
       ~d:d
       ~hd:corrupted_hd
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


(* EOF *)
