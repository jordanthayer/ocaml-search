(** BVLS bindings for OCaml.

    @author eaburns
    @since 2010-09-23
*)

external bvls :
  int * int -> float array -> float array -> float array -> float array
  -> float array = "bvls_stub"
    (** [bvls dims a b bl bu] *)


(* dimensions -> x's -> y's -> lower bound -> upper bound *)
