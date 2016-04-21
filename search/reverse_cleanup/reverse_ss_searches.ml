(** Reverse ss searches calls reverse cleanup searches with different kinds
    of single step model
    Jordan - August 2009 *)

open Reverse_cleanup_search


(*** Simple Ideas using no learning follow *)

let fd sface args =
  (** fd -- Fixed duration search, searches in f-order for a fixed duration,
      then switches to the prime order of optimistic search, fixed optimism of
      2. *)
  let bound = Search_args.get_float "Reverse_ss_searches.fd" args 0
  and duration = Search_args.get_int "Reverse_ss_searches.fd" args 1
  and optimism = Search_args.get_float "Reverse_ss_searches.fd" args 2 in
  let h_calc, f_calc = Dummy_ss.make_wastar_correction (fun n -> n.g)
    (fun n -> n.h) (optimism *. (bound -. 1.) +. 1.) in
    no_dups sface bound (fixed_duration duration) Timers.reckless h_calc f_calc

let fd_dups sface args =
  let bound = Search_args.get_float "Reverse_ss_searches.fd_dups" args 0
  and duration = Search_args.get_int "Reverse_ss_searches.fd_dups" args 1
  and optimism = Search_args.get_float "Reverse_ss_searches.fd_dups" args 2 in
  let h_calc, f_calc = Dummy_ss.make_wastar_correction (fun n -> n.g)
    (fun n -> n.h) (optimism *. (bound -. 1.) +. 1.) in
    dups sface bound (fixed_duration duration) Timers.reckless h_calc f_calc


let scaling_duration sface args =
  (** Scaling duration assumes that the length of the initial cleanup scales
      linearly with the size of the bound. *)
  let bound = Search_args.get_float
    "Reverse_ss_searches.scaling_duration" args 0
  and optimism = Search_args.get_float
    "Reverse_ss_searches.scaling_duration" args 1 in
  let length_est = sface.Search_interface.d sface.Search_interface.initial in
    fd sface [|string_of_float bound; string_of_float (length_est /. bound);
	       string_of_float optimism |]

let scaling_duration_dups sface args =
  let bound = Search_args.get_float
    "Reverse_ss_searches.scaling_duration_dups" args 0
  and optimism = Search_args.get_float
    "Reverse_ss_searches.scaling_duration_dups" args 1 in
  let length_est = sface.Search_interface.d sface.Search_interface.initial in
    fd_dups sface [|string_of_float bound;
		    string_of_float (length_est /. bound);
		    string_of_float optimism |]



(*** Approaches which rely on learning in some way *)

let fq_only_seems_bounded sface args =
  let bound = Search_args.get_float
    "Reverse_ss_searches.fq_only_seems_bounded" args 0 in
  let h_calc, f_calc = (Global_h_ss.make_unbounded_correction (fun n -> n.g)
			  (fun n -> n.h) (fun n -> n.d))
  and continue = min_fhat_admiss bound in
    no_dups sface bound continue Timers.reckless h_calc f_calc

let fq_only_seems_bounded_dups sface args =
  let bound = Search_args.get_float
    "Reverse_ss_searches.fq_only_seems_bounded_dups" args 0 in
  let h_calc, f_calc = (Global_h_ss.make_unbounded_correction (fun n -> n.g)
			  (fun n -> n.h) (fun n -> n.d))
  and continue = min_fhat_admiss bound in
    dups sface bound continue Timers.reckless h_calc f_calc


let simple_nodups sface args =
  let bound = Search_args.get_float
    "Reverse_ss_searches.simple_nodups" args 0 in
  let h_calc, f_calc = (Global_h_ss.make_unbounded_correction (fun n -> n.g)
			  (fun n -> n.h) (fun n -> n.d))
  and continue = simple bound in
    no_dups sface bound continue Timers.reckless h_calc f_calc


let simple_dups sface args =
  let bound = Search_args.get_float
    "Reverse_ss_searches.simple_nodups" args 0 in
  let h_calc, f_calc = (Global_h_ss.make_unbounded_correction (fun n -> n.g)
			  (fun n -> n.h) (fun n -> n.d))
  and continue = simple bound in
    dups sface bound continue Timers.reckless h_calc f_calc


(* EOF *)
