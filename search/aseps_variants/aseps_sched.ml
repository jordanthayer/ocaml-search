(** Scheduled version of A* epsilon
    Jordan - July 2009 *)

open Aseps


(****************** Node Getting Functions *******************************)


let mimic_aseps geq =
  (** When used as the get node call, will make search behave exactly like
      A* epsilon *)
  Geq.peek_best geq


let mimic_astar geq =
  (** When used as the get node call, will make search behave exactly like
      A* *)
  Geq.peek_doset geq


let make_schedule dur =
  (** returns a function which takes nodes from open once every [dur] steps *)
  assert (dur > 0);
  let counter = ref 0 in
    (fun geq ->
       if !counter = dur
       then (counter := 0;
	     Geq.peek_doset geq)
       else (counter := !counter + 1;
	     Geq.peek_best geq))


let first_n_doset dur =
  (** Takes the first [dur] nodes from the Doset (f-queue) *)
  let counter = ref 0 in
    (fun geq ->
       if !counter >= dur
       then Geq.peek_best geq
       else (counter := !counter + 1;
	     Geq.peek_doset geq))

let prob p =
  (** Flips a weighted coin [p], determining which queue to serve from next*)
  (fun geq ->
     if (Random.float 1.) > p
     then Geq.peek_best geq
     else Geq.peek_doset geq)



(****************** Searches *******************)
(* Never to be called directly from experiments *)
let no_dups get_node sface wt =
  (** Performs A* eps like search with scheduled serving on domains with
      few duplicate paths.
      [get_node] function giving you the next node to be exapanded
      [sface] domain search interface
      [wt] Desired Quality bound *)
  let search_interface = Search_interface.make
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
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol5 unwrap
    (Scheduled_focal.no_dups
       search_interface
       just_f
       d_then_f_then_g
       (make_close_enough_p wt)
       just_f
       set_q_pos
       get_q_pos
       get_node)


let dups get_node sface wt =
  (** Performs A* eps like search with scheduled serving on domains with
      many duplicate paths.
      [get_node] function giving you the next node to be exapanded
      [sface] domain search interface
      [wt] Desired Quality bound *)
  let search_interface = Search_interface.make
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
    (Limit.make_default_logger
       (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol6 unwrap
    (Scheduled_focal.dups
       search_interface
       just_f
       d_then_f_then_g
       (make_close_enough_p wt)
       just_f
       set_q_pos
       get_q_pos
       get_node)


(* Sanity checking searches *)
let aseps_dups sface args =
  let wt = Search_args.get_float "Aseps_sched.aseps_dups" args 0 in
    dups mimic_aseps sface wt

and astar_dups sface args =
  let wt = Search_args.get_float "Aseps_sched.astar_dups" args 0 in
    dups mimic_astar sface wt

and aseps sface args =
  let wt = Search_args.get_float "Aseps_sched.aseps" args 0 in
    no_dups mimic_aseps sface wt

and astar sface args =
  let wt = Search_args.get_float "Aseps_sched.astar" args 0 in
    no_dups mimic_astar sface wt


(* New Searches *)
let scheduled sface args =
  let wt = Search_args.get_float "Aseps_sched.scheduled" args 0
  and dur = Search_args.get_int "Aseps_sched.scheduled" args 1 in
    no_dups (make_schedule dur) sface wt

and scheduled_dups sface args =
  let wt = Search_args.get_float "Aseps_sched.scheduled_dups" args 0
  and dur = Search_args.get_int "Aseps_sched.scheduled_dups" args 1 in
    dups (make_schedule dur) sface wt

let alt sface args =
  let wt = Search_args.get_float "Aseps_sched.scheduled_dups" args 0
  and dur = 1 in
    dups (make_schedule dur) sface wt

and doset_sched sface args =
  let wt = Search_args.get_float "Aseps_sched.doset_sched" args 0
  and dur = Search_args.get_int "Aseps_sched.doset_sched" args 1 in
    no_dups (first_n_doset dur) sface wt

and doset_sched_dups sface args =
  let wt = Search_args.get_float "Aseps_sched.doset_sched_dups" args 0
  and dur = Search_args.get_int "Aseps_sched.doset_sched_dups" args 1 in
    dups (first_n_doset dur) sface wt

and prob sface args =
  let wt = Search_args.get_float "Aseps_sched.prob" args 0
  and p = Search_args.get_float "Aseps_sched.prob" args 1 in
    no_dups (prob p) sface wt

and prob_dups sface args =
  let wt = Search_args.get_float "Aseps_sched.prob" args 0
  and p = Search_args.get_float "Aseps_sched.prob" args 1 in
    dups (prob p) sface wt


(* EOF *)
