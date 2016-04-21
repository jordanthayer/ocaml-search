(** A simple, functional red black tree.

    @author eaburns
    @since 2010-08-12
*)

module Make(Ord : Map.OrderedType) = struct

  let cmp = Ord.compare

  type key = Ord.t
  type color = R | B
  type 'a t = L | T of color * 'a t * key * 'a * 'a t

  let empty = L

  let add_or_replace add key data t =
    (** [add_or_replace add key data t] inserts a new key into the
	tree. *)
    let rec do_insert key data = function
      | L -> T(R, L, key, data, L)
      | T(c, l, k, d, r) as t when (cmp key k) <= 0 ->
	  if add then T(c, balance (do_insert key data l), k, d, r) else t
      | T(c, l, k, d, r) -> T(c, l, k, d, balance (do_insert key data r))
    and balance = function
      | T(B, T(R, T(R, one, z, zd, two), y, yd, three), x, xd, four)
      | T(B, one, z, zd, T(R, two, y, yd, T(R, three, x, xd, four)))
      | T(B, T(R, one, z, zd, T(R, two, y, yd, three)), x, xd, four)
      | T(B, one, z, zd, T(R, T(R, two, y, yd, three), x, xd, four)) ->
	  T(R, T(B, one, z, zd, two), y, yd, T(B, three, x, xd, four))
      | t -> t
    in
      match balance (do_insert key data t) with
	| T(B, _, _, _, _) as t -> t
	| T(R, l, k, d, r) -> T(B, l, k, d, r)
	| L -> failwith "Impossible"


  let add key data t =
    (** [add key data t] adds a new binding to the tree. *)
    add_or_replace true key data t


  let replace key data t =
    (** [replace key data t] adds a binding to the tree or replaces
	the current one if there is one already. *)
    add_or_replace false key data t


  let rec minimum t =
    (** [minimum t] gets the minimum tree node below [t]. *)
    match t with
      | L -> L
      | T (_, L, _, _, _) as t -> t
      | T (_, t, _, _, _) -> minimum t


  let rec mem key t =
    (** [mem key t] tests if the given key is in the tree. *)
    match t with
      | L -> false
      | T(_, _, k, _, _) when (cmp key k) = 0 -> true
      | T(_, l, k, _, _) when (cmp key k) < 0 -> mem key l
      | T(_, _, k, _, r) -> mem key r


  let rec find_all key t =
    (** [find_all key t] finds all values bound to [key]. *)
    match t with
      | L -> []
      | T(_, l, k, d, r) when (cmp key k) = 0 ->
	  d :: (find_all key l) @ (find_all key r)
      | T(_, l, k, _, _) when (cmp key k) < 0 -> find_all key l
      | T(_, _, k, _, r) -> find_all key r


  let rec find key t =
    (** [find key t] finds the first value bound to [t]. *)
    match t with
      | L -> raise Not_found
      | T(_, l, k, d, r) when (cmp key k) = 0 -> d
      | T(_, l, k, _, _) when (cmp key k) < 0 -> find key l
      | T(_, _, k, _, r) -> find key r


  let verify t =
    (** [verify t] verifies that the red black tree properties hold
	for [t]. *)
    let rec do_verify check_key key = function
      | T(R, T(R, _, _, _, _), _, _, _) | T(R, _, _, _, T(R, _, _, _, _)) ->
	  failwith "Red node with red chidlren"
      | T(_, _, k, _, _) when not (check_key (cmp k key) 0) ->
	  failwith "Keys are not ordered correctly"
      | T(c, l, k, _, r) ->
	  let l_blacks = do_verify (<=) k l in
	  let r_blacks = do_verify (>=) k r in
	    if l_blacks <> r_blacks
	    then failwith "The number of black nodes do not match"
	    else l_blacks + (if c = B then 1 else 0)
      | L -> 1
    in match t with
      | L -> ()
      | T(R, _, _, _, _) -> failwith "The root is not black"
      | T(B, _, k, _, _) as t -> ignore (do_verify (fun a b -> true) k t)


end


module Of_ints = Make(struct type t = int let compare = compare end)
  (** [Of_ints] is a red black tree on integers. *)


module Of_strings = Make(String)
  (** [Of_strings] is a red black tree on strings. *)


let test () =
  let t = ref Of_ints.empty in
  let added = ref [] in
    for i = 0 to 100 do
      let i = Random.int 100000 in
	t := Of_ints.add i i !t;
	added := i :: !added;
	Of_ints.verify !t;
    done;
(*
    List.iter (fun i ->
		 t := Of_ints.remove i !t;
		 Of_ints.verify !t;)
      !added;
*)
    !t

