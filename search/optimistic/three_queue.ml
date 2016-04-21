(** Three Queued Search Uses Safegeq instead of geq for debugging mostly
    Jordan - Oct 2009 *)

let no_record = (fun _ _ _ -> ())


(* Three queue search w/o duplicate delaying.  Duplicates are put back into
   the search *)
let search record sface f_order ordered_focal ordered_fhat
    close_enough better_p set_qpos get_qpos set_fhpos get_fhpos set_geqe
    get_geqe get_node update_node =

  let goal = ref false in
  let openlist = Safe_geq.create_with ordered_fhat ordered_focal close_enough
    set_fhpos get_fhpos sface.Search_interface.initial
  and fq = Dpq.create f_order set_qpos 100 sface.Search_interface.initial
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in
(*  and key = sface.Search_interface.key in*)

  let in_fq n =
    (*
    let ret = ref false in
      (Dpq.iter (fun node -> if (key node) = (key n) then ret := true) fq);
      !ret*) true in

(*  let safe_remove n =
    let pos = ref (-1) in
      (Dpq.iteri (fun i node -> if (key node) = (key n) then pos := i) fq);
      assert (!pos = (get_qpos n));
      Dpq.remove fq !pos in *)

  let insert n =
    (* inserts node n into both the open list and the f ordered queue *)
    Dpq.insert fq n;
    set_geqe n (Safe_geq.insert openlist n);
    Htable.replace closed (sface.Search_interface.key n) (get_geqe n);
    assert (in_fq n);
  in

  let add_node n =
    (* add node is called on all of the children of an expanded node
       it checks to see if the nodes have been encounterd before.  If they
       have not they are inserted.  If they have been, they are checked to
       see if the new node is better.  If it is, the old node is removed
       and the new node is added. *)
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      (let state = sface.Search_interface.key n in
	 try
	   let entry = Htable.find closed state in
	     Limit.incr_dups i;
	     let prev = Safe_geq.data entry in
	       if not (better_p prev n)
	       then (if (get_qpos prev) <> Dpq.no_position
		     then ((*Verb.pe Verb.debug "Inserting Not Yet Expanded duplicate\n";*)
			   assert (in_fq prev);
			   Dpq.remove fq (get_qpos prev);
			   Safe_geq.remove openlist entry;
			   (*Safe_geq.iter (fun n -> assert (in_fq n)) openlist;*)
			   insert n)
		     else ((*Verb.pe Verb.debug "Inserting Expanded duplicate\n";*)
			   insert n))
	       (*else Verb.pe Verb.debug "Discarding Duplicate Node\n"*)
	 with Not_found ->
	   (*Verb.pe Verb.debug "Inserting new node\n";*)
	   insert n) in

  let do_expand n =
    assert (in_fq n);
(*    Safe_geq.iter (fun n -> assert (in_fq n)) openlist;*)
    (*Verb.pe Verb.debug "Doing geq remove\n";*)
    Safe_geq.remove openlist (get_geqe n);
    (*Verb.pe Verb.debug "Doing fq remove %i\n" (get_qpos n);*)
    Dpq.remove fq (get_qpos n);
(*    Safe_geq.iter (fun n -> assert (in_fq n)) openlist;*)
    set_fhpos n Dpq.no_position;
    set_qpos n Dpq.no_position;
    if not (Limit.promising_p i n)
    then (Limit.incr_prune i;
	  [])
    else (if sface.Search_interface.goal_p n
	  then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		goal := true;
		[])
	  else
	    (let reorder,children = sface.Search_interface.resort_expand n in
	       (** Note -> currently ignores resorting information *)
	       (if reorder
		then
		  ((*Verb.pe Verb.debug "Updating est_f\n";*)
		   Dpq.iter update_node fq;
		   (*Verb.pe Verb.debug "Resorting fq\n";*)
		   Dpq.resort_old fq;
(*		   Verb.pe Verb.debug "Resorting geq\n";
		   Safe_geq.iter (fun n -> assert (in_fq n)) openlist;*)
		   Safe_geq.resort openlist;
(*		   Safe_geq.iter (fun n -> assert (in_fq n)) openlist;
		   Verb.pe Verb.debug "Update Complete\n"*)));
	       Limit.incr_exp i;
	       children)) in

  let rec do_loop () =
    if not (Safe_geq.empty_p openlist) && not (Limit.halt_p i) && not !goal
    then
      (record i openlist fq;
       assert ((Safe_geq.count openlist) = (Dpq.count fq));
(*       Safe_geq.iter (fun n -> assert (in_fq n)) openlist;*)
       let n = get_node fq openlist i in
	 assert (in_fq n);
	 if not !goal
	 then (let children = do_expand n in
		 List.iter add_node children;
		 Limit.curr_q i (Safe_geq.count openlist);
		 do_loop())) in
    Dpq.insert fq sface.Search_interface.initial;
    set_geqe sface.Search_interface.initial
      (Safe_geq.insert openlist sface.Search_interface.initial);
    Htable.replace closed (sface.Search_interface.key
			     sface.Search_interface.initial)
      (get_geqe sface.Search_interface.initial);
(*    Safe_geq.iter (fun n -> assert (in_fq n)) openlist;*)
    do_loop();
    i


(* Three queue search with duplicate delaying.
   Get node actually handles how we do the duplicate delaying *)
let search_dd ?(record = no_record) sface f_order ordered_focal ordered_fhat
    close_enough better_p set_qpos get_qpos set_fhpos get_fhpos set_geqe
    get_geqe set_dpos get_dpos get_node update_node =

  let goal = ref false in
  let openlist = Safe_geq.create_with ordered_fhat ordered_focal close_enough
    set_fhpos get_fhpos sface.Search_interface.initial
  and fq = Dpq.create f_order set_qpos 100 sface.Search_interface.initial
  and delayed = Dpq.create f_order set_dpos 100 sface.Search_interface.initial
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in

  let insert n =
    (* inserts node n into both the open list and the f ordered queue *)
    Dpq.insert fq n;
    set_geqe n (Safe_geq.insert openlist n);
    Htable.replace closed (sface.Search_interface.key n) (get_geqe n) in

  let add_node n =
    (* add node is called on all of the children of an expanded node
       it checks to see if the nodes have been encounterd before.  If they
       have not they are inserted.  If they have been, they are checked to
       see if the new node is better.  If it is, the old node is removed
       and the new node is added. *)
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      (let state = sface.Search_interface.key n in
	 try
	   let entry = Htable.find closed state in
	     Limit.incr_dups i;
	     let prev = Safe_geq.data entry in
	       if not (better_p prev n)
	       then (if (get_qpos prev) <> Dpq.no_position
		     then (Dpq.remove fq (get_qpos prev);
			   Safe_geq.remove openlist entry;
			   insert n)
		     else (Dpq.insert delayed n
			     (* I'm uncomfortable with this hack *)
			   (*Htable.replace closed state (get_geqe n)*)))
	 with Not_found ->
	   insert n) in

  let do_expand n =
    (if ((get_dpos n) = Dpq.no_position)
     then (Dpq.remove fq (get_qpos n);
	   Safe_geq.remove openlist (get_geqe n))
     else Dpq.remove delayed (get_dpos n));
    set_fhpos n Dpq.no_position;
    set_qpos n Dpq.no_position;
    set_dpos n Dpq.no_position;
    if not (Limit.promising_p i n)
    then (Limit.incr_prune i;
	  [])
    else (if sface.Search_interface.goal_p n
	  then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		[])
	  else
	    (let reorder,children = sface.Search_interface.resort_expand n in
	       (** Note -> currently ignores resorting information *)
	       Limit.incr_exp i;
	       children)) in

  let rec do_loop () =
    if not (Safe_geq.empty_p openlist) && not (Limit.halt_p i) && not !goal
    then
      (record i openlist fq;
       let n = get_node fq delayed openlist i goal in
	 if not !goal
	 then (let children = do_expand n in
		 List.iter add_node children;
		 Limit.curr_q i (Safe_geq.count openlist);
		 do_loop())) in

    Dpq.insert fq sface.Search_interface.initial;
    set_geqe sface.Search_interface.initial
      (Safe_geq.insert openlist sface.Search_interface.initial);
    Htable.replace closed (sface.Search_interface.key
			     sface.Search_interface.initial)
      (get_geqe sface.Search_interface.initial);
    do_loop();
    i


let wrap_records clean_record geq_record =
  (fun i fq geq ->
     clean_record i fq;
     geq_record i geq)


(************************** Searches **********************************)
let no_dups ?(clean_record = no_record) ?(geq_record = no_record)
    sface f_order ordered_focal ordered_fhat close_enough better_p
    set_qpos get_qpos set_fhpos get_fhpos set_geqe get_geqe get_node update =
  (** Performs the F hat epsilon search on domains with few / no duplicates *)
  Limit.results5
    (search (wrap_records clean_record geq_record) sface f_order ordered_focal
       ordered_fhat close_enough better_p set_qpos get_qpos set_fhpos
       get_fhpos set_geqe get_geqe get_node update)


and dups ?(clean_record = no_record) ?(geq_record = no_record)
    sface f_order ordered_focal ordered_fhat close_enough better_p
    set_qpos get_qpos set_fhpos get_fhpos set_geqe get_geqe get_node update =
  (** Performs the F hat epsilon search on domains with many duplicates *)
  Limit.results6
    (search (wrap_records clean_record geq_record) sface f_order ordered_focal
       ordered_fhat close_enough better_p set_qpos get_qpos set_fhpos
       get_fhpos set_geqe get_geqe get_node update)


and delay ?(clean_record = no_record) ?(geq_record = no_record)
    sface f_order ordered_focal ordered_fhat close_enough better_p
    set_qpos get_qpos set_fhpos get_fhpos set_geqe get_geqe set_dpos get_dpos
    get_node update =
  Limit.results6
    (search_dd sface f_order ordered_focal ordered_fhat
       close_enough better_p set_qpos get_qpos set_fhpos get_fhpos set_geqe
       get_geqe set_dpos get_dpos get_node update)


(* EOF *)
