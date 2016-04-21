(** Aseps search as through the search interface
    Jordan Thayer - July 2009
*)

type 'a node = {
  f : float;
  g : float;
  d : float;
  (* position in q if applicable, or presence on closed *)
  mutable q_pos : int;
  data : 'a;
}

(* Sorting Predicates *)

let just_f a b =
  (** quality ordering predicate.  distinctions between nodes with the same
    f are made by the convenience ordering predicate. *)
  (*a.f <= b.f*)
  let dif = b.f -. a.f in
    Math.is_zero dif || Math.is_positive dif

let d_then_f_then_g a b =
  (** convenience ordering predicate *)
  (a.d < b.d) ||
  ((a.d = b.d) && ((a.f < b.f) ||
		   ((a.f = b.f) && (a.g >= b.g))))


let get_q_pos n =
  (** returns the position of node [n] in the dpq maintained by the geq*)
  n.q_pos

let set_q_pos n pos =
  (** sets the position of node [n] to [pos] *)
  n.q_pos <- pos


let make_close_enough_p c =
  (** builds the close enough predicate required by the geq *)
  (fun a b ->
     (** is b good enough compared to the benchmark a? *)
     b.f <= (a.f +. c))


let make_root initial =
  (** builds the root of the search tree *)
  { f = 0.;
    g = 0.;
    d = 0.;
    q_pos = Dpq.no_position;
    data = initial; }


let unwrap n =
  (** takes the solution found by the search and returns it in a format that
      the domain expects *)
  match n with
      Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let wrap_expand expand hd =
  (** takes a domain [expand] function and a cost and distance estimator [hd]
      and returns a node expand function *)
  (fun n ->
     List.map (fun (n, g) ->
		 let h, d = hd n in
		   { f = g +. h;
		     g = g;
		     d = d;
		     q_pos = Dpq.no_position;
		     data = n; })
     (expand n.data n.g))


let wrap fn =
  (** wraps the function [fn] which is made to operate on domain objects so
      that it can be applied to search objects *)
  (fun n -> fn n.data)


let no_record = (fun _ _ -> ())
let search_dups ?(record = no_record) sface ordered1 close_enough
    ordered2 better_p setpos getpos =
  (* special q_pos values *)
  let closed_pos = -2 in
    (** take [key] as additional argument *)
    (* like a_star_dups, stores all nodes in hashtable [nodes],
       distinguishing the closed list by a special q_pos. note 3 kinds of nodes:
       1) in openlist, garbage q_pos (init_pos or positive int)
       2) in openlist, valid q_pos (positive int)
       3) not in openlist, q_pos = closed_pos
    *)
  let openlist = Geq.create_with
    ~equals:(fun a b ->
	       sface.Search_interface.equals
		 (sface.Search_interface.key a)
		 (sface.Search_interface.key b))
    ordered1 ordered2 close_enough setpos getpos sface.Search_interface.initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in

  let insert_child n =
    (** return false iff [n] can be discarded *)
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      (let state = sface.Search_interface.key n in
	 try
	   let entry = Htable.find nodes state in
	     Limit.incr_dups i;
	     let prev = Geq.data entry in
	       if not (better_p prev n) then
		 (* better path to previous state. *)
		 (if (getpos prev) <> closed_pos
		    (* The current entry is somewhere in the geq *)
		  then (Geq.remove openlist entry;
			setpos prev closed_pos;
			Htable.replace nodes state
			  (Geq.insert openlist n))
		    (* The entry is not somewhere in the geq, we need to
		       re-insert it *)
		  else (Htable.replace nodes state
			  (Geq.insert openlist n)))
	 with Not_found ->
	   Htable.add nodes state (Geq.insert openlist n)) in

  let rec do_loop () =
    if ((not (Geq.empty_p openlist)) && (not (Limit.halt_p i)))
    then
      (record i openlist;
       let n = Geq.peek_best openlist in
	 Geq.remove_best openlist;
	 setpos n closed_pos;
	 if (not (Limit.promising_p i n))
	 then Limit.incr_prune i
	 else (if sface.Search_interface.goal_p n
	       then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	       else
		 (Limit.incr_exp i;
		  List.iter insert_child
		    (sface.Search_interface.node_expand n);
		  do_loop ())))
  in
    Htable.add nodes
      (sface.Search_interface.key sface.Search_interface.initial)
      (Geq.insert openlist sface.Search_interface.initial);
    do_loop ();
    let finc = (match i.Limit.incumbent with
		    Limit.Nothing -> 0.
		  | Limit.Incumbent (_,n) -> n.f) in
    let best_f = ref finc in
      Geq.iter (fun n -> best_f := min !best_f n.f) openlist;
      let post_bound = finc -. !best_f in
	Datafile.write_pairs stdout ["post_bound", string_of_float post_bound];
    Limit.results6 i


(****************** Searches *******************)
let make_iface sface =
Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd)
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial)
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))


let dups sface args =
  (** A* epsilon search on domains many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  let wt = Search_args.get_float "Aseps.dups" args 0 in
  Limit.unwrap_sol6 unwrap
    (search_dups
       (make_iface sface)
       just_f
       (make_close_enough_p wt)
       d_then_f_then_g
       just_f
       set_q_pos
       get_q_pos)



(* EOF *)
