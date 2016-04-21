(** A destructive STN which can be used with backtracking searches.

    @author eaburns
    @since 2010-03-06
*)

type t

type node = int

type constrnt = node * node * int * int


val infinity : int
  (** [infinity] the time value that represents infinity. *)


val neg_infinity : int
  (** [neg_infinity] the time value that represents negative
      infinity. *)


val create : int -> t
  (** [create num] creates a new network with [num] nodes besides the
      0th node. *)


val copy : ?copy_undo:bool -> t -> t
  (** [copy ?copy_undo t] copies the STN [t].  If [copy_undo] is true
      then the undo stack is copied too.  If not then the undo stack is
      empty in the new copy. *)


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


val all_bounds : t -> (node * int * int) array
  (** [all_bounds t] gets a list of (id, lower, upper) bounds for all
      nodes. *)


val bounds : t -> node -> (int * int)
  (** [bounds t n] gets the bounds for node [n]. *)


val output : out_channel -> t -> unit
  (** [output outch t] prints to a channel. *)


val undo : t -> t
  (** [undo t] rolls back the last (set of) constrant(s) added to the
      STN. *)

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


(** Macros for testing legality of stn **)

val legal_task : t -> node -> bool
  (** is the given task still feasible, or does the stn tell us that
      this is now totally out of the question? *)

val legal : t -> bool
  (** checks to see if the entire stn is still valid *)
