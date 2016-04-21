(** equivalence classes

  for when you can compute the representative of each class a priori.
  maintains a set of sets, each identitied by its representative.
  updates are destructive.
*)


type ('a, 'b) t
  (** type of eqvclasses of objects of type 'b using representatives
    of type 'a. *)
  
val create : unit -> ('a, 'b) t
  
val add : 'a -> 'b -> ('a, 'b) t -> unit
  (** [add a b ec] adds [b] to the class represented by [a] in
    [ec]. [b]'s class does not have to exist beforehand.  Note that the
    representative [a] does not automatically become part of the
    class. *)

val add_new_p : 'a -> 'b -> ('a, 'b) t -> bool
  (** like [add] but returns true iff this addition is new (by = on
    [a]) *)
  
val fold : ('a -> 'b -> 'c) -> ('c -> 'd -> 'c) -> ('a -> 'c -> 'a) ->
  'a -> ('b, 'd) t -> 'a
  (** [fold f1 f2 f3 init ec] has the effect of folding [f2] over each
    class.  Initial values for the folding are generated using [f1] and
    [f3]. [f1] supplies initial values for [f2] using previous results
    and the class representative.  [f3] takes the previous results and
    the result of [f2] and yields the next result for [f1] (or the final
    return value of [fold]). The initial initial value for [f1] is
    [init]. *)

val from_list : ('a -> 'b) -> 'a list -> ('b, 'a) t
  (** creates equivalence classes from a list and a function that
    returns the representative of a given item *)
  
val to_lists : ('a, 'b) t -> 'b list list
  (** returns a list of classes, each represented by a list of objects *)


(* EOF *)
