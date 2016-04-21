(* $Id: hist.mli,v 1.3 2004/06/14 20:30:46 ruml Exp ruml $

   histograms

   designed for BLFS tree size estimation
*)


type t

val make : int -> t


(**** non-destructive operations *****)


val fprint : out_channel -> t -> unit

(*
val plot : ?title:string -> string -> t -> unit
  (** writes postscript to the given filename *)
*)

val check : t -> unit
  (** some consistency checks *)

val max_pts : t -> int

val mean : t -> float
  (** returns the mean or 0 for if no data *)

val total_weight : t -> float

val has_mass : t -> bool
  (** has non-zero total weight *)

val min_val : t -> float
  (** min value actually present (neg_infinity if empty) *)

val max_val : t -> float
  (** max value actually present (infinity if empty) *)

val val_for_weight : float -> t -> float
  (** returns the value that has the [desired] weight to its left
    (inclusive).  infinity if empty.  *)

val sample : t -> float
  (** [sample t] samples a value from this distribution.  Results in
      nan if the histogram is empty. *)

val val_for_weight_and_bound : float -> t -> float * float * float
  (** returns the value that has the [desired] weight to its left
      (inclusive).  infinity if empty. also returns upper bound *)

val weight_left_of : float -> t -> float
 (** [weight_left_of vl t] gets the weight that is to the left of
     [vl] (inclusive). *)

val weight_for_val : t -> float -> float
  (** [weight_for_val t vl] gets the weight for a given value. *)

val bound_for_wted_combo : float -> t -> t -> float -> float
  (** returns a value for which [a] and [b], when weighting the
    weights in [b] by [b_wt] and considering both together, have at
    least the [desired] amount weight to the left *)

val add : ?max_bins:int -> t list -> t
  (** adds the given histograms and returns the sum.  Nondestructive. *)


val copy : t -> t
  (** [copy t] copies a histogram. *)


(*
  val convolve : t -> t -> t
(** values are added and weights are multipled.  nondestructive,
  returns a fresh histogram *)
  *)

val convolve_pruning : t -> t -> float -> t
  (** like convolve but chops any resultant weight over the bound *)


  (*
val convolve_max : t -> t -> t
  (** values are maxed and weights are multipled.  nondestructive,
    returns a fresh histogram *)
  *)

val convolve_max_pruning : t -> t -> float -> t
  (** values are maxed and weights are multipled.  nondestructive,
    returns a fresh histogram *)


(**** destructive operations *****)


val add_mass : float -> float -> t -> unit
  (** takes value and weight.  destructive *)

val scale : float -> t -> unit
  (** destructively multiply current weights by [factor] *)

val normalize : float -> t -> unit
  (** destructively multiply current weights so that total mass is
    [desired].  will fail if no mass! *)

val multiply : t -> float -> unit
  (** Destructively multiply the values in the histogram by a scalar.
      This stretches out the bins. *)

val prune_value_right : float -> t -> unit
  (** destructively remove some of the weight that is on values
      greater than [limit].  rather heuristic - will leave last bin. *)

(*
val prune_weight_right : float -> t -> unit
  (** destructively remove some of the weight that is in excess of
    [desired], taking it away from the right-hand side.  rather
    heuristic - will leave last bin. *)
*)

(*
  val cdf : ?exclude_point:bool -> float -> t -> float

  val density : float -> t -> float

  val expectation_of : (float -> float) -> t -> float

  val cdf_data : t -> float array * float array

  val density_data : t -> float array * float array

  mass in interval
*)

(************************************************************)

val get_plot_data : t -> (float * float) array * int * string
  (** [get_plot_data t] gets data for plotting.  This is required
      because external modules don't have access to the internal histogram
      representation.  (see blfs/hist_plot.ml). *)

(* EOF *)
