(* $Id: wrrandom.mli,v 1.1 2003/09/13 21:28:54 ruml Exp ruml $

   random number generators
*)

val randbetween : int -> int -> int
val randbetween_f : float -> float -> float

val float_in_range : float -> float -> float
val int_in_range : int -> int -> int

val with_seed : ('a -> 'b) -> 'a -> int -> 'b
  (** given [f], [arg], and [seed], calls [f] on [arg] with the state
      of Random initialized using [seed].  Restores the previous random
      state afterwards *)


(** fast generation of samples from complex distributions.  probably
  not so great for distributions with heavy tails. **)

type table

val init_table : (float -> float) -> table
  (** given distribution function that, given a probability mass,
    returns the value below which that much mass in the distribution
    lies, returns a table suitable for passing to [from_table] *)

val from_table : table -> float
  (** uses the built-in Random generator *)

val normal_table : float -> float -> table
  (** takes mean and stddev *)

val print_table : table -> unit
  (** for debugging *)


val std_normal_leva : unit -> float
  (** normal deviate *)

val normal : float -> float -> float
  (** mean, stddev *)

val log_normal : float -> float -> float
  (** mean, stddev *)

val normal_from_table : float -> float -> float
  (** mean, stddev.  uses a table, so only 8K possible results. *)


(* EOF *)
