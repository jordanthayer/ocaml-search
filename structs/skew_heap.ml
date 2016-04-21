(** A skew heap is a totally functional implementation of a heap data
    structure.

    [merge], [insert], [remove_min] and [extract_min] have amortized
    O(log n) time.

    [is_empty], [is_singleton], [push_min] and [peek_min] are constant
    time.

    [size] is O(n) time.

    @author eaburns
    @since 2009-12-30
*)

module type PartiallyOrderedType = sig
  (** A partially ordered type.  The only thing that is important here
      is that a predecessor function can be defined on the type. *)

type t
  (** The type of the heap elements *)

val before : t -> t -> bool
  (** [before a b] tests if [a] comes before [b]. *)

 end

module Make(Ord : PartiallyOrderedType) = struct

  type elm = Ord.t

  type t = Empty | Node of t * elm * t

  let empty = Empty

  let rec merge a b =
    (** [merge a b] merges the two trees [a] and [b].  This is suppose
	to be amortized O(log n) time. *)
    match a, b with
      | Empty, Empty -> Empty
      | (Node _ as n), Empty | Empty, (Node _ as n) -> n
      | (Node (ll, lv, lr) as l), (Node (rl, rv, rr) as r) ->
	  if Ord.before lv rv
	  then Node (merge r lr, lv, ll)
	  else Node (merge l rr, rv, rl)


  let insert t vl = merge t (Node (Empty, vl, Empty))
    (** [insert t vl] inserts a value [v] into the skew heap. *)


  let remove_min = function
      (** [remove_min t] removes the minimum element from the heap. *)
    | Empty -> invalid_arg "Skew_heap.remove_min"
    | Node (l, _, r) -> merge l r


  let push_min t vl = Node(t, vl, Empty)
    (** [push_min t vl] pushes [vl] onto the front of heap [t]. If
	[vl] is not truely a minimum element then the heap property will
	be violated. *)


  let peek_min = function
    | Empty -> invalid_arg "Skew_heap.remove_min"
    | Node (_, v, _) -> v


  let extract_min t =
    (** [extract_min t] remove the minimum element and return its
	value along with the new tree. *)
    let vl = peek_min t in remove_min t, vl


  let is_empty = function
      (** [is_empty t] tests if the tree is empty. *)
    | Empty -> true
    | _ -> false


  let is_singleton = function
      (** [is_singleton t] tests if this is a tree with only one
	  element in constant time. *)
    | Empty -> false
    | Node (l, _, r) -> l = Empty && r = Empty


  let rec size = function
      (** [size t] gets the number of elements in the tree.  This is
	  O(n). *)
    | Empty -> 0
    | Node (l, _, r) -> 1 + (size l) + (size r)


  let rec iter f = function
      (** [iter f t] iterates over [t] calling [f] on every value. *)
    | Empty -> ()
    | Node (l, v, r) ->
	iter f l;
	f v;
	iter f r


  let rec fold f t a =
    (** [fold f t a] computes [(f xN ... (f x2 (f x1 a))...)], where
	[x1 ... xN] are the elements of [t] in an unspecified
	order. *)
    match t with
      | Empty -> a
      | Node (l, v, r) -> f v (fold f r (fold f l a))
end
