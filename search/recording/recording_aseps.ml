(** Aseps search with recording *)

open Aseps

let wrap_expand recorder expand hd =
  (** takes a domain [expand] function and a cost and distance estimator [hd]
      and returns a node expand function *)
  (fun n ->
     let children =
       List.map (fun (n, g) ->
		   let h, d = hd n in
		     { f = g +. h;
		       g = g;
		       d = d;
		       q_pos = Dpq.no_position;
		       data = n; })
	 (expand n.data n.g) in
       recorder n n children;
       children)


let make_iface sface node_record =
  let init = Search_interface.make
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial)
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    (Search_interface.alter
       ~node_expand:(Some (wrap_expand
			     (node_record init.Search_interface.info)
			     sface.Search_interface.domain_expand
			     sface.Search_interface.hd)) init)


let no_dups node_recorder queue_recorder sface wt =
  (** A* epsilon search on domains without many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  Limit.unwrap_sol5 unwrap
    (Focal_search.search
       ~record:queue_recorder
       (make_iface sface node_recorder)
       just_f
       (make_close_enough_p wt)
       d_then_f_then_g
       just_f
       set_q_pos
       get_q_pos)


let dups node_recorder queue_recorder sface wt =
  (** A* epsilon search on domains many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  Limit.unwrap_sol6 unwrap
    (Focal_search.search_dups
       ~record:queue_recorder
       (make_iface sface node_recorder)
       just_f
       (make_close_enough_p wt)
       d_then_f_then_g
       just_f
       set_q_pos
       get_q_pos)


(*** Recorders ***)
let exp_rec key_printer key =
  Recorders.expansion_recorder key_printer (wrap key)
    (fun n -> n.g) (fun n -> -1) (fun n -> n.f)

let focal_rec key_printer key =
  Recorders.geq_focal_recorder key_printer (wrap key)

let geq_rec key_printer key =
  Recorders.geq_all_recorder key_printer (wrap key)

(***************************************************************************)

let record_exp_nodups sface args =
  let wt = Search_args.get_float "Recording_aseps.record_exp_nodups" args 0 in
  no_dups (exp_rec sface.Search_interface.key_printer
	     sface.Search_interface.key) Recorders.none sface wt

let record_focal_nodups sface args =
  let wt = Search_args.get_float "Recording_aseps.record_focal_nodups" args 0 in
  no_dups  Recorders.no_node_record
    (focal_rec sface.Search_interface.key_printer
       sface.Search_interface.key) sface wt

let record_geq_nodups sface args =
  let wt = Search_args.get_float "Recording_aseps.record_geq_nodups" args 0 in
  no_dups  Recorders.no_node_record
    (geq_rec sface.Search_interface.key_printer sface.Search_interface.key)
    sface wt


let record_exp_dups sface args =
  let wt = Search_args.get_float "Recording_aseps.record_exp_dups" args 0 in
  dups (exp_rec sface.Search_interface.key_printer
	  sface.Search_interface.key) Recorders.none sface wt

let record_focal_dups sface args =
  let wt = Search_args.get_float "Recording_aseps.record_focal_dups" args 0 in
  dups  Recorders.no_node_record
    (focal_rec sface.Search_interface.key_printer
       sface.Search_interface.key) sface wt

let record_geq_dups sface args =
  let wt = Search_args.get_float "Recording_aseps.record_geq_dups" args 0 in
  dups Recorders.no_node_record
    (geq_rec sface.Search_interface.key_printer
       sface.Search_interface.key) sface wt

(* EOF *)
