(** A Lazy list implementation.  A lazy list is evaluated on-demand,
    this allows for infinite lists, and more efficient list mapping
    and filtering (when mapping/filtering multiple times at once,
    since the original list is only traversed a single time to create
    the resulting list... and only when needed).

    Ethan Burns <eaburns@unh.edu>
    11-11-2008
*)

type 'a t = Znil | Znode of 'a * 'a t lazy_t

exception Sequence_done

let rec construct i s =
  (** [construct i s] Construct a new, lazy list beginning with [i],
      then by continually applying the successor function [s] to the
      initial value [i]. *)
  lazy (try Znode(i, construct (s i) s) with Sequence_done -> Znil)


let rec take n zlst =
  (** [take n lst] Take the first [n] values from [zlst]. *)
  lazy (if n = 0 then Znil
	else match Lazy.force zlst with
	  | Znil -> Znil
	  | Znode(hd, tl) -> Znode(hd, take (n - 1) tl))


let rec ints_from n = construct n succ
  (** [ints_from n] Get the lazy list of integers from [n] to
      infinity. *)


let range min max = take ((max + 1) - min) (ints_from min)
  (** [range min max] Create a list that is the set of integers from
      [min] to [max] inclusive. *)



let rec zlist_of_list lst =
  (** [zlist_of_list lst] Convert [lst] into a lazy list. *)
  lazy (match lst with
	  | [] -> Znil
	  | hd :: tl -> Znode(hd, zlist_of_list tl))


let rec list_of_zlist ?(accum=[]) zlst =
  (** [list_of_zlist zlst] Convert the the lazy list [zlst] to a list. *)
  match Lazy.force zlst with
    | Znil -> List.rev accum
    | Znode(hd, tl) -> list_of_zlist ~accum:(hd :: accum) tl


let hd zlst =
  (** [hd zlst] Get the first element of [zlst]. *)
  match Lazy.force zlst with
    | Znil -> failwith "hd"
    | Znode(hd, tl) -> hd


let tl zlst =
  (** [tl zlst] Get all of the elements becides the head of [zlst]. *)
  match Lazy.force zlst with
    | Znil -> failwith "tl"
    | Znode(hd, tl) -> tl


let nth zlst n =
  (** [nth zlst n] Get the [n]th element of [zlst]. *)
  if n < 0 then invalid_arg "Zlist.nth"
  else
    let rec nth_aux zlst n =
      match Lazy.force zlst with
	| Znil -> failwith "nth"
	| Znode(hd, tl) -> if n = 0 then hd else nth_aux tl (n - 1)
    in nth_aux zlst n


let rec map f zlst =
  (** [map f zlst] Create a lazy list which is the list [zlst] with the
      function [f] over each element. *)
  lazy (match Lazy.force zlst with
	  | Znil -> Znil
	  | Znode(hd, tl) -> Znode(f hd, map f tl))


let rec filter p zlst =
  (** [filter p zlst] Get a lazy list that is all of the elements of
      [zlst] that evaluate to true for predicate [p].  Using this on an
      infinite list whet [p] never evaluates to true will create an
      infinite loop. *)
  lazy (match Lazy.force zlst with
	  | Znil -> Znil
	  | Znode(hd, tl) ->
	      if p hd then Znode(hd, filter p tl)
	      else Lazy.force (filter p tl))


let rec iter f zlst =
  (** [iter f zlst] Iterate over [zlst] calling [f] for each element. *)
  match Lazy.force zlst with
    | Znil -> ()
    | Znode(hd, tl) ->
	f hd;
	iter f tl


let rec fold f i zlst =
  (** [fold f i zlst] Fold [f] left across [i] then [zlst].  In other
      words: [f (... (f (f i b1) b2) ...) bn].  This should never be
      used on infinite lists. *)

  match Lazy.force zlst with
    | Znil -> i
    | Znode(hd, tl) -> (fold f (f i hd) tl)


let rec combine zlst1 zlst2 =
  (** [combine zlst1 zlst2] Combine two lazy lists into a list of
      tuples.  The two lists must be of the same size. *)
  lazy (match (Lazy.force zlst1), (Lazy.force zlst2) with
	  | Znil, Znil -> Znil
	  | Znode(hd1, tl1), Znode(hd2, tl2) ->
	      Znode((hd1, hd2), combine tl1 tl2)
	  | _, _ -> invalid_arg "Zlist.combine")


let rec exists p zlst =
  (** [exists p zlst] Test if the predicate [p] is true for any member
      of [zlst]. *)
  match Lazy.force zlst with
    | Znil -> false
    | Znode(hd, tl) -> p hd || exists p tl


let rec mem x zlst = exists (fun y -> compare x y = 0) zlst
  (** [mem x zlst] Test if [x] is a member of [zlst].  Do not use this
      on an infinite list. *)


let rec for_all p zlst =
  (** [for_all p zlst] Test if predicate [p] is true for all elements of
      [zlst].  Do not use this on an infinite list. *)
  match Lazy.force zlst with
    | Znil -> true
    | Znode(hd, tl) -> p hd && for_all p tl
