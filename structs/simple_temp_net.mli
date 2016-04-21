(** A functional simple temporal network.

    This is an implementation of the algorithms described in: Cesta
    and Oddi, TIME-96, "Gaining Efficiency and Flexibility in the
    Simple Temporal Problem."

    @author eaburns
    @since 2010-02-15
*)


type t

type node = int

type constrnt = node * node * int * int

exception Inconsistent
  (** [Inconsistent] is raised when the propogation proves the
      temporal network to be inconsistent. *)


val empty : t
  (** [empty] the empty STN. *)


val create : int -> t
  (** [create num] creates a new network with [num] nodes besides the
      0th node. *)


val create_with : int -> constrnt list -> t
  (** [create_with num cs] creates a new network with [num] nodes
      (besides the 0th) and with the given constraints. *)


val add_nodes : t -> int -> int * int * t
  (** [add_nodes t num] adds [num] new nodes to [t] with no bounds.
      The result is the range of node id values (first, last,
      new_stn). *)


val add_node : t -> int * t
  (** [add_node t] adds a single node to [t] and returns its id and
      the new network . *)


val add_constraint : t -> constrnt -> t
  (** [add_constraint t (i, j, a, b)] adds a constraint:

      [a <= j - i <= b]. *)

val add_constraints : t -> constrnt list -> t
  (** [add_constraints t cs] adds a list of constraints to [t]. *)


val remove_constraint : t -> node -> node -> t
  (** [remove_constraint t i j] removes a constraint between [i] and
      [j].

      Slow!
  *)


val all_bounds : t -> (node * int * int) array
  (** [all_bounds t] gets a list of (id, lower, upper) bounds for all
      nodes. *)


val bounds : t -> node -> (int * int)
  (** [bounds t n] gets the bounds for node [n]. *)


val output : out_channel -> t -> unit
  (** [output outch t] prints to a channel. *)


(** {6 Building Constraints} ****************************************)

val before : node -> node -> int -> constrnt
  (** [before i j a] builds a constraint that ensures that [i] is at
      least [a] time units before [j]. *)


val after : node -> node -> int -> constrnt
  (** [after i j a] builds a constraint that ensures that [i] is at
      least [a] time units after [j]. *)


val no_later_than : node -> int -> constrnt
  (** [no_later_than i d] builds a constraint that ensures that [i]
      starts no later than [d], the deadline. *)


val not_earlier_than : node -> int -> constrnt
  (** [not_earlier_than i s] builds a constraint that ensures that [i]
      starts no earlier than [s], the start time. *)


val in_window : node -> int -> int -> constrnt
  (** [in_window i s d] builds a constraint that ensures that [i] is
      within the time window [s]-[d]. *)


val check_bounds : t -> unit
val output : out_channel -> t -> unit
