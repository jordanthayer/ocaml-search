(** Three Queued Search -- Uses Geq instead of safe geq to get improved
    performance
    Jordan - July 2009 *)

exception Not_in_f
exception Not_in_geq
exception Not_in_either

let no_record = (fun _ _ _ -> ())

let make_eq key =
  (fun a b ->
     (key a) = (key b))


(* Three queue search w/o duplicate delaying.  Duplicates are put back into
   the search *)
let search ?(record = no_record) ?(kp = (fun _ _ -> ())) sface f_order
    ordered_focal ordered_fhat close_enough better_p set_qpos get_qpos
    set_fhpos get_fhpos set_geqe get_geqe get_node update_node g_update =

  let goal = ref false in
  let openlist = Geq.create_with
    (*~equals:(make_eq sface.Search_interface.key) ~pr:kp*)
    ordered_fhat ordered_focal close_enough
    set_fhpos get_fhpos sface.Search_interface.initial
  and fq = Dpq.create f_order set_qpos 100 sface.Search_interface.initial
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in

  let do_update () =
    ((*Verb.pe Verb.debug "Resorting geq\n";*)
     Geq.resort openlist set_geqe update_node;
     Dpq.resort_old fq;
     (*Verb.pe Verb.debug "Update Complete\n"*)) in

  let insert n =
    (* inserts node n into both the open list and the f ordered queue *)
    Dpq.insert fq n;
    set_geqe n (Geq.insert openlist n);
    Htable.replace closed (sface.Search_interface.key n) (get_geqe n)
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
	     let prev = Geq.data entry in
	       if not (better_p prev n)
	       then (if (get_qpos prev) <> Dpq.no_position
		     then ((*Verb.pe Verb.debug "Inserting Not Yet Expanded duplicate\n";*)
			   Dpq.remove fq (get_qpos prev);
			   Geq.remove openlist (get_geqe prev);
			   insert n)
		     else ((*Verb.pe Verb.debug "Inserting Expanded duplicate\n";*)
			   g_update prev n;
			   do_update()))
	       (*else Verb.pe Verb.debug "Discarding Duplicate Node\n"*)
	 with Not_found ->
	   (*Verb.pe Verb.debug "Inserting new node\n";*)
	   insert n) in

  let do_expand n =
    Geq.remove openlist (get_geqe n);
    (*Verb.pe Verb.debug "Doing fq remove %i\n" (get_qpos n);*)
    Dpq.remove fq (get_qpos n);
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
		then do_update());
	       Limit.incr_exp i;
	       children)) in

  let rec do_loop () =
    if not (Geq.empty_p openlist) && not (Limit.halt_p i) && not !goal
    then
      (record i openlist fq;
       assert ((Geq.count openlist) = (Dpq.count fq));
       let n = get_node fq openlist i in
	 (*in_both "get_node" n;*)
	 if not !goal
	 then (let children = do_expand n in
		 List.iter add_node children;
		 Limit.curr_q i (Geq.count openlist);
		 do_loop())) in
    Dpq.insert fq sface.Search_interface.initial;
    set_geqe sface.Search_interface.initial
      (Geq.insert openlist sface.Search_interface.initial);
    Htable.replace closed (sface.Search_interface.key
			     sface.Search_interface.initial)
      (get_geqe sface.Search_interface.initial);
    do_loop();
    i


(************************** Searches **********************************)
let no_dups ?(clean_record = no_record) ?(geq_record = no_record)
    sface f_order ordered_focal ordered_fhat close_enough better_p
    set_qpos get_qpos set_fhpos get_fhpos set_geqe get_geqe get_node update
    g_update =
  (** Performs the F hat epsilon search on domains with few / no duplicates *)
  Limit.results5
    (search sface f_order ordered_focal ordered_fhat
       close_enough better_p set_qpos get_qpos set_fhpos get_fhpos set_geqe
       get_geqe get_node update g_update)


and dups ?(clean_record = no_record) ?(geq_record = no_record)
    ?(kp = (fun _  _ -> ())) sface f_order ordered_focal ordered_fhat close_enough better_p
    set_qpos get_qpos set_fhpos get_fhpos set_geqe get_geqe get_node update
    g_update =
  (** Performs the F hat epsilon search on domains with many duplicates *)
  Limit.results6
    (search ~kp:kp sface f_order ordered_focal ordered_fhat
       close_enough better_p set_qpos get_qpos set_fhpos get_fhpos set_geqe
       get_geqe get_node update g_update)

(* EOF *)
