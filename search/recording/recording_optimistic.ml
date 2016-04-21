(** Records the exicution of optimistic searches *)

open Optimistic_search


let make_expand info exp_rec expand h weight =
  (** takes the [expand] function from the domain and returns a search space
      expand function *)
  let make_child =
    (fun (n, g) ->
       let h = h n in
	 { fp = g +. (weight *. h);
	   h = h;
	   g = g;
	   fpos = Dpq.no_position;
	   ppos = Dpq.no_position;
	   dpos = Dpq.no_position;
	   data = n;})  in
    (fun parent ->
       let children = List.map make_child (expand parent.data parent.g)
       in exp_rec info parent parent children;
	 children)


(********************** Various recorders ******************************)

let exp_rec key_printer key =
  Recorders.expansion_recorder key_printer (wrap key)
    (fun n -> n.g) (fun n -> -1) (fun n -> n.h +. n.g)

let queue_rec key_printer key = Recorders.dpq_recorder key_printer (wrap key)

let clean_rec key_printer key =
  let recorder = queue_rec key_printer key in
    (fun info prime clean -> recorder info clean)

let prime_rec key_printer key =
  let recorder = queue_rec key_printer key in
    (fun info prime clean -> recorder info prime)


let no_queue_rec = (fun _ _ _ -> ())

(***************************************************************************)
let make_interface exp_rec sface wt =
  let init = Search_interface.make
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { fp = neg_infinity;
      h = neg_infinity;
      g = 0.;
      ppos = Dpq.no_position;
      fpos = Dpq.no_position;
      dpos = Dpq.no_position;
      data = sface.Search_interface.initial}
    better_p
    (Limit.make_default_logger (fun n -> n.h +. n.g)
       (wrap sface.Search_interface.get_sol_length)) in
    Search_interface.alter
      ~node_expand:(Some (make_expand init.Search_interface.info exp_rec
		      sface.Search_interface.domain_expand
		      sface.Search_interface.h wt)) init


let no_dups exp_rec queue_rec sface bound wt =
  (** Performs optimistic search in domains with few or no duplicates *)
  let search_interface = make_interface exp_rec sface wt in
    Limit.unwrap_sol5 unwrap_sol
      (Optimistic_framework.no_dups
	 ~queue_rec:queue_rec
	 search_interface
	 get_node
	 ordered_p
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos)


let dups exp_rec queue_rec sface bound wt =
  (** Performs optimistic search in domains with duplicates *)
  let search_interface = make_interface exp_rec sface wt in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.dups
	 ~queue_rec:queue_rec
	 search_interface
	 get_node
	 ordered_p
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos
	 get_pq_pos
	 get_f_pos)



(********************** specific callers ***************************)

let expand_recorder_nodups sface args =
  let bound = Search_args.get_float
    "Recording_optimistic.expand_recorder_nodups" args 0
  and wt = Search_args.get_float
    "Recording_optimistic.expand_recorder_nodups" args 1 in
  no_dups (exp_rec sface.Search_interface.key_printer
	     sface.Search_interface.key) no_queue_rec sface bound wt

let expand_recorder_dups sface args =
  let bound = Search_args.get_float
    "Recording_optimistic.expand_recorder_dups" args 0
  and wt = Search_args.get_float
    "Recording_optimistic.expand_recorder_dups" args 1 in
  dups (exp_rec sface.Search_interface.key_printer
	  sface.Search_interface.key) no_queue_rec sface bound wt

let prime_recorder_nodups sface args =
  let bound = Search_args.get_float
    "Recording_optimistic.prime_recorder_nodups" args 0
  and wt = Search_args.get_float
    "Recording_optimistic.prime_recorder_nodups" args 1 in
  no_dups
    Recorders.no_node_record
    (prime_rec
       sface.Search_interface.key_printer
       sface.Search_interface.key)
    sface bound wt

let prime_recorder_dups sface args =
  let bound = Search_args.get_float
    "Recording_optimistic.prime_recorder_nodups" args 0
  and wt = Search_args.get_float
    "Recording_optimistic.prime_recorder_nodups" args 1 in
  dups Recorders.no_node_record
    (prime_rec sface.Search_interface.key_printer
       sface.Search_interface.key)
    sface bound wt

let clean_recorder_nodups sface args =
  let bound = Search_args.get_float
    "Recording_optimistic.clean_recorder_nodups" args 0
  and wt = Search_args.get_float
    "Recording_optimistic.clean_recorder_nodups" args 1 in
    no_dups
      Recorders.no_node_record
      (clean_rec sface.Search_interface.key_printer
	 sface.Search_interface.key) sface bound wt

let clean_recorder_dups sface args =
  let bound = Search_args.get_float
    "Recording_optimistic.clean_recorder_nodups" args 0
  and wt = Search_args.get_float
    "Recording_optimistic.clean_recorder_nodups" args 1 in
    dups
      Recorders.no_node_record
      (clean_rec sface.Search_interface.key_printer
	 sface.Search_interface.key) sface bound wt

(* EOF *)
