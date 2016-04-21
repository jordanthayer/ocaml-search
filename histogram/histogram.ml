(** The histogram implementation.  Mostly written in C, but some of
    the higher level functionality is written in OCaml.

    This is designed to be a drop-in replacement for the Hist module
    in the structs system.

    @author eaburns
    @since 2009-12-23
*)

type t

type kind = Nothing | Points | Bins
    (** The various kinds of histograms.  Rarely needed... *)

let string_of_kind = function
  | Nothing -> "Nothing"
  | Points -> "Points"
  | Bins -> "Bins"

external make : int -> t = "histogram_make_stub"
    (** [make max] creates a new histogram with [max] maximum points
	and bins. *)

external make_bins : int -> float -> float -> t = "histogram_make_bins_stub"
    (** [make_bins max start end] creates a new bins histogram with
	[max] max bins and the bins range from [start] to [end].. *)

external kind : t -> kind = "histogram_kind_stub"
    (** [kind t] gets the kind of histogram. *)

external copy : t -> t = "histogram_copy_stub"
    (** [copy t] returns a fresh copy of the histogram [t]. *)

external add_into : t -> t -> t = "histogram_add_stub"
    (** [add_into a b] adds the weight of [b] into [a] the result is
	[a]. *)

external add_mass : float -> float -> t -> unit = "histogram_add_mass_stub"
    (** [add_mass vl wt t] adds [wt] mass to [t] at value [vl]. *)

external total_weight : t -> float = "histogram_total_weight_stub"
    (** [total_weight t] gets the total amount of weight in [t]. *)

external min_val : t -> float = "histogram_min_value_stub"
    (** [min_val t] gets the minimum value in [t]. *)

external max_val : t -> float = "histogram_max_value_stub"
    (** [max_val t] gets the maximum value in [t]. *)

external fill : t -> int = "histogram_fill_stub"
    (** [fill t] gets the fill of the histogram.  If it is points then
	this is the number of points in the histogram.  If it is bins
	then it is the max value.  If it is a nothing histogram then
	the fill is zero. *)

external size : t -> int = "histogram_size_stub"
    (** [size t] gets the size (max bins or points) of the histogram. *)

let all_points_total_min_max lst =
  (** [all_points_total_min_max lst] (like in Wheeler's histogram)
      this checks if all histograms in the list are points and if they
      are then the result is Some c where c is the total number of
      distinct points.  This also gets the minimum and the maximum. *)
  let rec do_all_points_total bin_seen accum min max lst =
    assert (List.for_all (fun h -> (kind h) <> Nothing) lst);
    match lst with
      | [] -> (not bin_seen), accum, min, max
      | hd :: tl ->
	  let hd_min_v = min_val hd
	  and hd_max_v = max_val hd in
	  let min_v = if min < hd_min_v then min else hd_min_v
	  and max_v = if max > hd_max_v then max else hd_max_v
	  in
	    assert ((kind hd) <> Nothing);
	    begin match kind hd with
	      | Nothing -> failwith "Nothing";
	      | Points ->
		  do_all_points_total bin_seen (accum + (fill hd)) min_v max_v tl
	      | Bins ->
		  do_all_points_total true (accum + (fill hd)) min_v max_v tl
	    end
  in do_all_points_total false 0
       (min_val (List.hd lst))
       (max_val (List.hd lst)) lst


let add ?(max_bins=(~-1)) list =
  (** [add ?max_bins lst] adds a list of histograms together.  The
      result is a new histogram. *)
  let lst = List.filter (fun h -> (kind h) <> Nothing) list in
    assert (List.for_all (fun h -> (kind h) <> Nothing) lst);
    if lst = []
    then invalid_arg "Histogram.add: list is empty";
    let res, lst, min, max = match all_points_total_min_max lst with
      | true, accum, min, max ->
	  if max_bins > 0
	  then make max_bins, lst, min, max
	  else copy (List.hd lst), (List.tl lst), min, max
      | false, _, min, max ->
	  if max_bins > 0
	  then make_bins max_bins min max, lst, min, max
	  else make_bins (size (List.hd lst)) min max, lst, min, max
    in List.fold_left (fun s h -> add_into s h) res lst


external is_empty : t -> bool = "histogram_is_empty_stub"
	(** [is_empty t] tests if [t] is empty. *)

let has_mass t = not (is_empty t)
  (** [has_mass t] tests if there has been mass added to [t]. *)

external prune_value_right : float -> t -> unit = "histogram_prune_above_stub"
  (** [prune_value_right bound t] prunes all weight in [t] above
      [bound]. *)

external scale : float -> t -> unit = "histogram_scale_stub"
  (** [scale factor t] scales the mass in [t] by [factor]. *)

external normalize : float -> t -> unit = "histogram_normalize_stub"
  (** [normalize wt t] normalizes the weights in [t] to sum to
      [wt]. *)

external weight_left_of : float -> t -> float = "histogram_weight_left_of_stub"
  (** [weight_left_of vl t] gets the amount of weight to the left of
      [vl] in [t]. *)

external val_for_weight : float -> t -> float = "histogram_val_for_weight_stub"
  (** [val_for_weight wt t] gets the bound that has [wt] weight left
      of it in [t]. *)

external bound_for_wted_combo :
  float -> t -> t -> float -> float = "histogram_bound_for_wted_combo_stub"
  (** [bound_for_wted_combo desired a b b_wt] finds the bound that
      will have [desired] weight to its left after adding [a] and [b]
      when [b] is scaled by [b_wt]. *)


external convolve_pruning : t -> t -> float -> t = "histogram_convolve_stub"
  (** [convolve a b bound] gives a new histogram that is the
      result of convolving [a] and [b] and pruning above
      [bound]. *)


let convolve a b = convolve_pruning a b infinity
  (** [convolve a b] gets a new histogram that is the convolution of
      [a] and [b] without any pruning. *)


let debug_points = 1 lsl 0
  (** [debug_points] enables debugging output for points
      histograms. *)

let debug_bins = 1 lsl 1
  (** [debug_bins] enables debugging output for bins histograms. *)

let debug_convert = 1 lsl 2
  (** [debug_convert] enables debugging for converting from points to
      bins. *)

let debug_add = 1 lsl 3
  (** [debug_add] enables debugging for adding histograms. *)

let debug_convolve = 1 lsl 4
  (** [debug_convolve] enables debugging for convolving histograms. *)

let debug_stubs = 1 lsl 5
  (** [debug_stubs] enables debugging for OCaml/C stubs. *)

let debug_copy = 1 lsl 6
  (** [debug_copy] enables debugging for copying histograms. *)

let debug_bounds = 1 lsl 7
  (** [debug_bounds] enables debugging for bound finding. *)

let debug_prune = 1 lsl 8
  (** [debug_prune] enables debugging for pruning histograms. *)

let debug_sanity_check = 1 lsl 9
  (** [debug_sanity_check] enables possibly expensive sanity checks. *)

external set_debug_mask : int -> unit = "histogram_set_debug_mask_stub"
  (** [set_debug_mask mask] sets the debugging mask to the given
      value.  This should be a bunch of debugging flags [lor]ed
      together.  If debugging is not enabled in the histogram
      (compiled out) then this call fails. *)

external get_debug_mask : unit -> int = "histogram_get_debug_mask_stub"
  (** [get_debug_mask ()] gets the debug mask.  If debugging is not
      enabled in the histogram (compiled out) then this fails. *)
