(**

    @author jtd7
    @since 2011-10-18
*)

type vals = {
  g : float;
  d : float;
  h : float;
  depth : float;
  h_err : float;
  d_err : float;
  fhat : float;
}

type 'a node = {
  data : 'a;
  values : vals;
}

let alt_col_name = "iterations"

let output_col_hdr () =
    Datafile.write_alt_colnames stdout alt_col_name ["iter no";
						     "iter bound";
						     "iter expanded"; ]
let output_row iter bound expansions =
    Datafile.write_alt_row_prefix stdout alt_col_name;
    Verb.pr Verb.always "%d\t%f\t%d\n" iter bound expansions


let wrap f = (fun n -> f n.data)

let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
    | Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.values.g)

let better_p a b =
  (** Sorts nodes solely on total cost information *)
  a.values.g <= b.values.g


let compute_fhat w d_err h_err depth h d g =
  let derr = d_err /. depth
  and herr = h_err /. depth in
  let d' = Math.fmax d (d /. (1. -. derr)) in
    g +. w *. (h +. (Math.fmax 0. (herr *. d')))


let make_expand i expand hd w =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let pvals = n.values in
     let depth' = pvals.depth +. 1.
     and pf = pvals.g +. pvals.h in
     List.map (fun (data, g) ->
		 Limit.incr_gen i;
		 let h,d = hd data in
		 let h_err = g +. h -. pf
		 and d_err = pvals.d -. d -. 1. in
		 let d_err' = pvals.d_err +. d_err
		 and h_err' = pvals.h_err +. h_err in
		 let values = { g = g; d = d; h = h; depth = depth';
				h_err = h_err'; d_err = d_err';
				fhat = (compute_fhat w
					  d_err' h_err' depth' h d g)} in
		 { data = data;
		   values = values;}) (expand n.data pvals.g))


let do_iteration goal i expand bound it root =
  let exp = ref 0 in
  let next_bound = ref infinity in
  let rec help node =
    if not (Limit.halt_p i)
    then (if node.values.fhat > bound || !next_bound < 0.
	  then next_bound := min !next_bound node.values.fhat
	  else (if goal node
		then (next_bound := -1.;
		      Limit.new_incumbent i (Limit.Incumbent (0.,node)))
		else (Limit.incr_exp i;
		      exp := !exp + 1;
		      List.iter help (expand node)))) in
    next_bound := infinity;
    help root;
    output_row it bound !exp;
    !next_bound


let do_search goal i expand root =
  let rec help it current_bound =
    let next_bound = do_iteration goal i expand current_bound it root in
      if next_bound > 0. && not (Limit.halt_p i)
      then help (it + 1) next_bound in
    output_col_hdr();
    help 0 root.values.fhat


let dups_with_cr sface args =
   let module SF = Search_interface in
  let nbins = Search_args.get_int
    "Iterative_sspath_cr.dups_with_cr" args 0
  and control_factor = Search_args.get_float
    "Iterative_sspath_cr.dups_with_cr" args 1
  and wt = Search_args.get_float
    "Iterative_sspath_cr.dups_with_cr" args 2 in
  let get_f n = n.values.fhat in
  let reset, see, complete, check =
    Idastar_cr_model.make nbins control_factor get_f in
  let goal = wrap sface.SF.goal_p
  and log = (Limit.make_default_logger (fun n -> n.values.g)
	       (wrap sface.SF.get_sol_length)) in
  let h,d = sface.SF.hd sface.SF.initial in
  let root = { data = sface.SF.initial;
	       values = { g = 0.; h = h; d = d; depth = 1.;
			  h_err = 0.; d_err = 0.;
			  fhat =   h;}; } in
  let i = Limit.make Limit.Nothing sface.SF.halt_on better_p log in
  let expand = make_expand i sface.SF.domain_expand sface.SF.hd wt in
  let key = wrap sface.SF.key
  and hash = sface.SF.hash
  and equals = sface.SF.equals in
  reset root.values.fhat;
  Verb.pe Verb.debug "Bound set to initial value, calling.\n";
  Limit.unwrap_sol6 unwrap_sol
    (Iterative_deepening.no_dups_in_dups_dom_no_sface i goal expand root
       key hash equals better_p see check complete)


let dups_with_cr_tiles sface args =
   let module SF = Search_interface in
  let nbins = 100
  and control_factor = 3.32 (* hard coded for the 5x5 puzzle, ignoring parent pointer *)
  and wt = Search_args.get_float "Iterative_sspath_cr.dups_with_cr" args 0 in
  let get_f n = n.values.fhat in
  let reset, see, complete, check =
    Idastar_cr_model.make nbins control_factor get_f in
  let goal = wrap sface.SF.goal_p
  and log = (Limit.make_default_logger (fun n -> n.values.g)
	       (wrap sface.SF.get_sol_length)) in
  let h,d = sface.SF.hd sface.SF.initial in
  let root = { data = sface.SF.initial;
	       values = { g = 0.; h = h; d = d; depth = 1.;
			  h_err = 0.; d_err = 0.;
			  fhat =   h;}; } in
  let i = Limit.make Limit.Nothing sface.SF.halt_on better_p log in
  let expand = make_expand i sface.SF.domain_expand sface.SF.hd wt in
  let key = wrap sface.SF.key
  and hash = sface.SF.hash
  and equals = sface.SF.equals in
  reset root.values.fhat;
  Verb.pe Verb.debug "Bound set to initial value, calling.\n";
  Limit.unwrap_sol6 unwrap_sol
    (Iterative_deepening.no_dups_in_dups_dom_no_sface i goal expand root
       key hash equals better_p see check complete)
