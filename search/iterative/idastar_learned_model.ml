(** IDA* which is controlled by a learned model.

    TODO: clean this up so that it doesn't use a ton of globals.

    @author eaburns
    @since 2009-10-12
*)


module Idastar = Iterative_deepening_astar

let output_iteration_row iter_no bound prev_nodes expansions expected desired =
  (** [output_iteration_row iter_no bound prev_nodes expansions
      expected desired] outputs row information for each iteration. *)
  Iterative_deepening.output_row iter_no bound expansions;
  if iter_no > 0
  then begin
    Datafile.write_alt_row_prefix stdout "factors";
    Verb.pr Verb.always "%d\t%.4f\t%d\t%f\t%f\n%!"
      iter_no expected expansions
      ((float expansions) /. (float prev_nodes))
      ((float expansions) /. expected);
  end

let output_estimation_row iter_no desired bound =
  (** [output_estimation_row iter_no desired boun ] outputs row
      information for estimation at each iteration. *)
  Datafile.write_alt_row_prefix stdout "estimation";
  Verb.pr Verb.always "%d\t%f\t%f\n%!" iter_no desired bound


let get_bound iter_no count_nodes init_h init_t init_children
    find_bound min_out_of_bound desired =
  (** [get_bound iter_no count_nodes init_h init_t init_children
      find_bound min_out_of_bound desired] find the bound that will
      give [desired] nodes by using the bound estimation function
      [find_bound].  If the estimated bound is too low then clamp it
      to the minimum out of bounds cost. *)
  let estimated_bound, expected =
    find_bound
      init_h
      init_t
      (List.map (fun (t, d, _, f) -> t, d, f) init_children)
      desired
  in
    output_estimation_row iter_no desired estimated_bound;
    if min_out_of_bound > estimated_bound
      || not (Math.finite_p estimated_bound)
    then begin
      let expected =
	count_nodes [init_h]
	  (List.map (fun (t, d, _, f) -> t, d, f) init_children)
	  min_out_of_bound
      in
	Verb.pr Verb.toplvl "clamping estimated bound %.10f to %0.10f\n%!"
	  estimated_bound min_out_of_bound;
	min_out_of_bound, expected
    end else estimated_bound, expected


let get_desired iter_no expansions mul =
  (** [get_desired mul] gets the desired number of nodes for the next
      iteration. *)
  Math.fmax ((float expansions) *. mul) (mul ** (float iter_no))


let make reset_model iteration_complete
    update_model find_bound count_nodes get_t get_d mul =
  (** [make mul] make an IDA* bound model using a learned model. *)

  let expansions = ref 0
  and prev_nodes = ref 0
  and min_out_of_bound = ref infinity
  and min_out_of_bound_prev = ref infinity
  and desired = ref 0.
  and iter_no = ref 0
  and this_bound = ref 0.
  and prev_bound = ref 0.
  and expected = ref 0. in

  (* functions *)
  let get_t n = get_t n.Idastar.data
  and get_d n = get_d n.Idastar.data
  and get_f n = n.Idastar.f
  in

  (* Initial node information *)
  let init_h = ref 0.
  and init_t = ref 0
  and init_children = ref []
  and seen_init = ref false in

  let reset_bound vl =
    (** [reset_bound vl] resets the bound estimation model.  This is
	called before a search begins. *)
    Iterative_deepening.output_col_hdr ();
    Datafile.write_alt_colnames stdout "factors" [
      "factor iteration";
      "expected nodes";
      "actual nodes";
      "growth factor";
      "estimation factor";
    ];
    Datafile.write_alt_colnames stdout "estimation" [
      "estimation iteration";
      "desired nodes";
      "estimated bound";
    ];

    reset_model ();
    iter_no := 0;
    seen_init := false;
    min_out_of_bound := infinity;
    min_out_of_bound_prev := infinity;
    expansions := 0;
    prev_nodes := 0;
    desired := 1.;
    this_bound := vl;
    prev_bound := vl
  in

  let see_expansion depth n children =
    (** [see_expansion depth n children]
	is called each expansion to update the learned model.  *)
    incr expansions;
    if not !seen_init
    then begin
      assert (depth = 0);
      seen_init := true;
      init_h := n.Idastar.h;
      init_t := get_t n;
      init_children :=
	List.map
	  (fun c -> get_t c, get_d c, c.Idastar.g, c.Idastar.f)
	  children;
    end;
    update_model get_t get_d get_f !this_bound depth n children
  in


  let iteration_complete completed_depth last =
    (** [iteration_complete completed_depth last] updates the model to
	the bound for the next iteration. *)
    if (float !expansions) > !desired *. mul
    then Verb.pr Verb.toplvl "\n";

    output_iteration_row !iter_no !this_bound
      !prev_nodes !expansions !expected !desired;
    prev_nodes := !expansions;
    if not last
    then begin
      (* Get the desired number of nodes. *)
      incr iter_no;
      desired := get_desired !iter_no !expansions mul;

      iteration_complete completed_depth !this_bound;
      let bound, expect =
	get_bound !iter_no count_nodes !init_h !init_t !init_children
	  find_bound !min_out_of_bound !desired
      in
	(*
	  iteration_complete completed_depth !this_bound;
	*)
	if not (Math.finite_p bound)
	then begin
	  Verb.pr Verb.always "min_out=%f\n%!" !min_out_of_bound;
	end;
	Verb.pr Verb.toplvl "next bound: %f\n%!" bound;
	expected := expect;
	min_out_of_bound := infinity;
	expansions := 0;
	this_bound := bound;
    end
  in

  let check_bound n =
    (** [check_bound n] check if [n] is within the bound for
	the current iteration. *)
    (*
      if (float !expansions) > !desired *. mul
      then begin
      Verb.pr Verb.toplvl ".";
      false
      end else
    *)
    if n.Idastar.f <= !this_bound
    then true
    else begin
      if n.Idastar.f < !min_out_of_bound
      then min_out_of_bound := n.Idastar.f;
      false
    end

  in reset_bound, see_expansion, iteration_complete, check_bound
