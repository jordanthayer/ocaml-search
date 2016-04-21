(** expectation-maximization
    mixture of gaussians for d-dimensional data.

    @author eaburns
    @since 2009-08-23
*)

val fit_gaussians :
  ?iters:int -> ?range:float -> int -> float array array -> Normal.t array
  (** [fit_gaussians ?iters k data] fits [k] gaussians to [data] using
      [iters] iterations of EM. *)
