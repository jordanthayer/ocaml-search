(** Calls single step clamped searches with various error models.
    Jordan Aug 2009 *)

open Single_step

let h_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.reckless h_calc f_calc


let h_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless h_calc f_calc


let h_ss_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_ss_reckless_dd"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface Timers.reckless h_calc f_calc



let greedy_h_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_h_fact (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface Timers.reckless h_calc f_calc


let greedy_h_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_h_fact (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface Timers.reckless h_calc f_calc


let greedy_h_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_h_fact (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    drop sface Timers.reckless h_calc f_calc



let greedy_recurh_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_hrecur_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface Timers.reckless h_calc f_calc


let greedy_recurh_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_hrecur_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface Timers.reckless h_calc f_calc


let greedy_recurh_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_hrecur_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    drop sface Timers.reckless h_calc f_calc



let h_ss_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_conservative_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.conservative h_calc f_calc


let h_ss_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_conservative_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.conservative h_calc f_calc


let h_ss_conservative_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_conservative_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface Timers.conservative h_calc f_calc


let h_ss_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_geo_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface (Timers.geometric 1. 1.2)  h_calc f_calc


let h_ss_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_geo_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_geo_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_ss_geo_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface (Timers.geometric 1. 1.2) h_calc f_calc



let h_ss_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_reckless_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.reckless h_calc f_calc


let h_ss_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_reckless_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless h_calc f_calc


let h_ss_fhp_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_reckless_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface Timers.reckless h_calc f_calc


let h_ss_fhp_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_conservative_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.conservative h_calc f_calc


let h_ss_fhp_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_conservative_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.conservative h_calc f_calc


let h_ss_fhp_conservative_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_conservative_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface Timers.conservative h_calc f_calc



let h_ss_fhp_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_geo_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_fhp_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_geo_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_fhp_geo_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_ss_fhp_geo_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface (Timers.geometric 1. 1.2) h_calc f_calc


let hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.reckless h_calc f_calc


let hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless h_calc f_calc


let hd_ss_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.hd_ss_reckless_dd"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface Timers.reckless h_calc f_calc


let hd_ss_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.hd_ss_fhp_reckless_no_dups" args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.reckless h_calc f_calc


let hd_ss_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.hd_ss_fhp_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless h_calc f_calc


let hd_ss_fhp_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.hd_ss_fhp_reckless_dd"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    drop sface Timers.reckless h_calc f_calc


let mult_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.mult_no_dups"
    args 0 in
  let h_calc,f_calc = Simple_mult.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound (((bound -. 1.) *. 2.) +. 1.) in
    no_dups sface Timers.reckless h_calc f_calc


let mult_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.mult_dups"
    args 0 in
  let h_calc,f_calc = Simple_mult.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound (((bound -. 1.) *. 2.) +. 1.) in
    dups sface Timers.reckless h_calc f_calc


let mult_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.mult_dd" args 0 in
  let h_calc,f_calc = Simple_mult.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound (((bound -. 1.) *. 2.) +. 1.) in
    drop sface Timers.reckless h_calc f_calc


let m1_ss_reckless_no_dups vect sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.m1_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Vector_h_ss.make_bounded_fhp_correction
    (fun n -> [|n.h; n.d; n.g; 0.; 0.; 0.;|]) vect bound in
    no_dups sface Timers.reckless h_calc f_calc


let m1_ss_reckless_dups vect sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.m1_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Vector_h_ss.make_bounded_fhp_correction
    (fun n -> [|n.h; n.d; n.g; 0.; 0.; 0.;|]) vect bound in
    dups sface Timers.reckless h_calc f_calc


let m1_ss_reckless_dd vect sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.m1_ss_reckless_dd"
    args 0 in
  let h_calc,f_calc = Vector_h_ss.make_bounded_fhp_correction
    (fun n -> [|n.h; n.d; n.g; 0.; 0.; 0.;|]) vect bound in
    drop sface Timers.reckless h_calc f_calc


let h_lms_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_lms_reckless_no_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let h_calc,f_calc = Lms_h.make_clamped_correction
    [| 1.; 0.; 0.; 0.;|] maxes.(2) maxes.(0) maxes.(1)
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.reckless h_calc f_calc


let h_lms_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_lms_fhp_reckless_no_dups" args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let h_calc,f_calc = Lms_h.make_clamped_fhp_correction
    [| 1.; 0.; 0.; 0.;|] maxes.(2) maxes.(0) maxes.(1)
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    no_dups sface Timers.reckless h_calc f_calc


let h_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_lms_reckless_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let h_calc,f_calc = Lms_h.make_clamped_correction
    [| 1.; 0.; 0.; 0.;|] maxes.(2) maxes.(0) maxes.(1)
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless h_calc f_calc


let h_lms_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_lms_fhp_reckless_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let h_calc,f_calc = Lms_h.make_clamped_fhp_correction
    [| 1.; 0.; 0.; 0.;|] maxes.(2) maxes.(0) maxes.(1)
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless h_calc f_calc


let h_ann_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Clamped_searches.h_lms_fhp_reckless_no_dups" args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let h_calc,f_calc = Ann_hd.make_batch_delay_fhp_correction 100 bound
    maxes maxes.(Norm_values.h) (fun n -> [|n.h; n.d; n.g; 1.|])
    (fun n -> n.g) (fun n -> n.h) in
    no_dups sface Timers.reckless h_calc f_calc

let h_ann_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_lms_fhp_reckless_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let h_calc,f_calc = Ann_hd.make_batch_delay_fhp_correction 100 bound
    maxes maxes.(Norm_values.h) (fun n -> [|n.h; n.d; n.g; 1.|])
    (fun n -> n.g) (fun n -> n.h) in
    dups sface Timers.reckless h_calc f_calc


(********************************* Fixed fhat prime lms *********************)

let h_lms_reckless_dups_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_lms_reckless_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Clamped Lms Dups" args 1;
			  Search_args.get_float "Clamped Lms Dups" args 2;
			  Search_args.get_float "Clamped Lms Dups" args 3;
			  Search_args.get_float "Clamped Lms Dups" args 4;|]) in
  let h_calc,f_calc = Lms_h.make_clamped_correction
    i_weights maxes.(2) maxes.(0) maxes.(1)
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless (fun _ _ _ -> ()) f_calc


let h_lms_fhp_reckless_dups_no_learn sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Clamped_searches.h_lms_fhp_reckless_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Clamped Lms Dups" args 1;
			  Search_args.get_float "Clamped Lms Dups" args 2;
			  Search_args.get_float "Clamped Lms Dups" args 3;
			  Search_args.get_float "Clamped Lms Dups" args 4;|]) in
  let h_calc,f_calc = Lms_h.make_clamped_fhp_correction
    i_weights maxes.(2) maxes.(0) maxes.(1)
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    dups sface Timers.reckless (fun _ _ _ -> ()) f_calc


(* EOF *)
