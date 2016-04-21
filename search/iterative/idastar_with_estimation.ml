(** Perform a standard IDA* search, but also output alt-cols for the
    estimations from various models: KRE, GKRE, incremental.

    @author eaburns
    @since 2009-10-10
*)


let expansions = ref 0
let iter_no = ref 0
let this_bound = ref 0.
and next = ref infinity

(*
let kre_model = ref Kre.no_model
and kre_start_wts = ref [| |]
and kre_n_fun = ref (fun t i -> [| |])

let gkre_model = ref Gkre.no_model

let inc_model = ref Incremental_model.no_model
  *)

let init_h = ref 0.
and init_t = ref 0
and init_children = ref [0 (* type *), 0 (* d *), 0. (* h *), 0. (* f *)]
and seen_init = ref false
  (* initial node information *)


let output_model_col_hdr model =
  Datafile.write_alt_colnames stdout model [ model ^ " no";
					     model ^ " bound";
					     model ^ " expanded"; ]


let output_model_row model iter bound estimation =
    Datafile.write_alt_row_prefix stdout model;
    Verb.pr Verb.always "%d\t%f\t%f\n" iter bound estimation


let reset_bound models vl =
  Iterative_deepening.output_col_hdr ();
  List.iter (fun (name, reset, _, _, _) ->
	       output_model_col_hdr name;
	       reset ();)
    models;
  iter_no := 0;
  seen_init := false;
  expansions := 0;
  this_bound := vl;
  next := infinity


let see_expansion models get_t get_d (n, g, h) children =
  incr expansions;
  List.iter (fun (_, _, _, update_model, _) ->
	       update_model
		 (get_t n, get_d n, g, h)
		 (List.map
		    (fun (c, g, h) -> get_t c, get_d c, h, g)
		    children))
    models;
  if not !seen_init
  then begin
    seen_init := true;
    init_h := h;
    init_t := get_t n;
    init_children :=
      List.map (fun (c, g, h) -> get_t c, get_d c, h, g +. h) children;
  end

let iteration_complete models _ =
  Iterative_deepening.output_row !iter_no !this_bound !expansions;
  List.iter (fun (name, _, prep_bound, _, estimate) ->
	       let est =
		 estimate
		   !init_h
		   !init_t
		   (List.map (fun (t, d, h, f) -> t, d, h, f) !init_children)
		   !next
	       in
		 output_model_row name (!iter_no + 1) !next est;
		 prep_bound ();)
    models;
    incr iter_no;
    expansions := 0;
    this_bound := !next;
    next := infinity


let check_bound (n, h, g) =
  let f = g +. h in
    if f > !this_bound && f < !next
    then next := f;
    f <= !this_bound


let model = reset_bound, see_expansion, iteration_complete, check_bound
