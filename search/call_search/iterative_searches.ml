(** Searches with an iterative style to them *)

(** IDA* *)
let ida interface = Iterative_deepening_astar.no_dups interface
let ida_bnb interface = Iterative_deepening_astar.no_dups_bnb interface
let ida_total interface = Iterative_deepening_astar.no_dups_total interface
let ida_total_count interface =
  Iterative_deepening_astar.no_dups_total_count interface
and ida_dups interface = Iterative_deepening_astar.dups interface
and ida_dups_dep interface = Iterative_deepening_astar.dups interface
and ida_dd interface = failwith "Nonsensical"



let idastar_im_dups interface argv =
  let max_bins = Search_args.get_int "idastar_im_dups" argv 0
  and control_factor = Search_args.get_float "idastar_im_dups" argv 1
  and get_t = interface.Search_interface.t
  and get_d n = int_of_float (interface.Search_interface.d n) in
  let bound_model = Im.make max_bins in
  let reset_model = (fun () -> Im.reset bound_model)
  and iteration_complete = Im.iteration_complete bound_model
  and update_model = Im.update bound_model
  and find_bound _ _ _ = Im.find_bound bound_model
  and count_nodes _ _ bound = snd (Im.count_nodes bound_model bound) in
  let reset_bound, see_expansion, iteration_complete, check_bound =
    Idastar_learned_model.make
      reset_model
      iteration_complete
      update_model
      find_bound
      count_nodes
      get_t
      get_d
      control_factor
  in
    Iterative_deepening_astar.dups_with_model
      Iterative_deepening.no_dups_in_dups_dom_bnb
      interface
      reset_bound
      see_expansion
      iteration_complete
      check_bound


let idastar_im_no_dups interface argv =
  let max_bins = Search_args.get_int "idastar_im_no_dups" argv 0
  and control_factor = Search_args.get_float "idastar_im_no_dups" argv 1
  and get_t = interface.Search_interface.t
  and get_d n = int_of_float (interface.Search_interface.d n) in
  let bound_model = Im.make max_bins in
  let reset_model = (fun () -> Im.reset bound_model)
  and iteration_complete = Im.iteration_complete bound_model
  and update_model = Im.update bound_model
  and find_bound _ _ _ = Im.find_bound bound_model
  and count_nodes _ _ bound = snd (Im.count_nodes bound_model bound) in
  let reset_bound, see_expansion, iteration_complete, check_bound =
    Idastar_learned_model.make
      reset_model
      iteration_complete
      update_model
      find_bound
      count_nodes
      get_t
      get_d
      control_factor
  in
    Iterative_deepening_astar.no_dups_with_model
      Iterative_deepening.no_dups_bnb
      interface
      reset_bound
      see_expansion
      iteration_complete
      check_bound

module Im_lazy_vec = Im2.Im_lazy_vec
module Im_garray = Im2.Im_garray

let idastar_im_lazy_vec_no_dups interface argv =
  let max_bins = Search_args.get_int "idastar_im_lazy_vec_no_dups" argv 0
  and control_factor =
    Search_args.get_float "idastar_im_lazy_vec_no_dups" argv 1
  and get_t = interface.Search_interface.t
  and get_d n = int_of_float (interface.Search_interface.d n) in
  let bound_model = Im_lazy_vec.make max_bins in
  let reset_model = (fun () -> Im_lazy_vec.reset bound_model)
  and iteration_complete = Im_lazy_vec.iteration_complete bound_model
  and update_model = Im_lazy_vec.update bound_model
  and find_bound root_h _ children =
    Im_lazy_vec.find_bound bound_model root_h children
  and count_nodes _ _ _ = nan in
  let reset_bound, see_expansion, iteration_complete, check_bound =
    Idastar_learned_model.make reset_model iteration_complete
      update_model find_bound count_nodes get_t get_d control_factor
  in
    Iterative_deepening_astar.no_dups_with_model
      Iterative_deepening.no_dups_bnb interface reset_bound see_expansion
      iteration_complete check_bound


let idastar_im_lazy_vec_dups interface argv =
  let max_bins = Search_args.get_int "idastar_im_lazy_vec_dups" argv 0
  and control_factor = Search_args.get_float "idastar_lazy_vec_im_dups" argv 1
  and get_t = interface.Search_interface.t
  and get_d n = int_of_float (interface.Search_interface.d n) in
  let bound_model = Im_lazy_vec.make max_bins in
  let reset_model = (fun () -> Im_lazy_vec.reset bound_model)
  and iteration_complete = Im_lazy_vec.iteration_complete bound_model
  and update_model = Im_lazy_vec.update bound_model
  and find_bound root_h _ children =
    Im_lazy_vec.find_bound bound_model root_h children
  and count_nodes _ _ _ = nan in
  let reset_bound, see_expansion, iteration_complete, check_bound =
    Idastar_learned_model.make reset_model iteration_complete
      update_model find_bound count_nodes get_t get_d control_factor
  in
    Iterative_deepening_astar.dups_with_model
      Iterative_deepening.no_dups_in_dups_dom_bnb interface reset_bound
      see_expansion iteration_complete check_bound

let idastar_im_garray_no_dups interface argv =
  let max_bins = Search_args.get_int "idastar_im_garray_no_dups" argv 0
  and control_factor =
    Search_args.get_float "idastar_im_garray_no_dups" argv 1
  and get_t = interface.Search_interface.t
  and get_d n = int_of_float (interface.Search_interface.d n) in
  let bound_model = Im_garray.make max_bins in
  let reset_model = (fun () -> Im_garray.reset bound_model)
  and iteration_complete = Im_garray.iteration_complete bound_model
  and update_model = Im_garray.update bound_model
  and find_bound root_h _ children =
    Im_garray.find_bound bound_model root_h children
  and count_nodes _ _ _ = nan in
  let reset_bound, see_expansion, iteration_complete, check_bound =
    Idastar_learned_model.make reset_model iteration_complete
      update_model find_bound count_nodes get_t get_d control_factor
  in
    Iterative_deepening_astar.no_dups_with_model
      Iterative_deepening.no_dups_bnb interface reset_bound see_expansion
      iteration_complete check_bound


let idastar_im_garray_dups interface argv =
  let max_bins = Search_args.get_int "idastar_im_garray_dups" argv 0
  and control_factor = Search_args.get_float "idastar_garray_dups" argv 1
  and get_t = interface.Search_interface.t
  and get_d n = int_of_float (interface.Search_interface.d n) in
  let bound_model = Im_garray.make max_bins in
  let reset_model = (fun () -> Im_garray.reset bound_model)
  and iteration_complete = Im_garray.iteration_complete bound_model
  and update_model = Im_garray.update bound_model
  and find_bound root_h _ children =
    Im_garray.find_bound bound_model root_h children
  and count_nodes _ _ _ = nan in
  let reset_bound, see_expansion, iteration_complete, check_bound =
    Idastar_learned_model.make reset_model iteration_complete
      update_model find_bound count_nodes get_t get_d control_factor
  in
    Iterative_deepening_astar.dups_with_model
      Iterative_deepening.no_dups_in_dups_dom_bnb interface reset_bound
      see_expansion iteration_complete check_bound


(*
let idastar_gkre_model_dups interface model_path =
  let gkre_in = open_in model_path in
  let model = (Marshal.from_channel gkre_in : Gkre.t) in
    close_in gkre_in;
    let find_bound root_h root_t children nodes =
      let cdp cost =
	Gkre.cdp
	  model
	  (int_of_float root_h)
	  root_t
	  (* This mapping assumes d = h. *)
	  (List.map (fun (t, d, _) -> d, t) children)
	  (int_of_float cost)
      in fst (Num_opt.invert cdp nodes 0. root_h 1. 100)
    in
    let reset_bound, see_expansion, iteration_complete, check_bound =
      Idastar_learned_model.make
	(fun () -> ())			(* reset_model *)
	(fun _ _ -> ())			(* iteration complete *)
	(fun _ _ _ -> ())		(* update_model *)
	find_bound
	(fun _ _ _ -> ~-.1.)		(* count nodes *)
	interface.Search_interface.t
	(fun n -> int_of_float (interface.Search_interface.d n))
	2.0
    in
      Iterative_deepening_astar.dups_with_domain_model
	Iterative_deepening.no_dups_in_dups_dom_bnb
	interface
	reset_bound
	see_expansion
	iteration_complete
	check_bound
*)

(*
let idastar_domain_model_total_dups interface =
    Iterative_deepening_astar.dups_domain_model_total interface

let idastar_domain_model_dups interface =
  Iterative_deepening_astar.dups_domain_model_bnb interface

let idastar_domain_model interface =
  Iterative_deepening_astar.domain_model_bnb interface
*)

let idastar_double_bound_dups interface =
  Iterative_deepening_astar.dups_double_bound interface

let idastar_cr_dups interface args =
  Iterative_deepening_astar.dups_idastar_cr interface args


let idastar_cr_no_dups interface args =
  Iterative_deepening_astar.no_dups_idastar_cr interface args


(** Iterative deepening search *)
let ids interface = Iterative_deepening_search.no_dups interface
and ids_dups interface = Iterative_deepening_search.dups interface
and ids_dd interface = failwith "Nonsensical"


(* EOF *)
