(** A simple trie.  This trades off memory for speed.  It may be
    fairly memory inefficient but should be pretty fast.

    @author eaburns
    @since 2010-08-02
*)

module type Vector_key = sig
  type t

  val element : t -> int -> int
    (** [element_value t index] gets the element (as an int) at the
	given index in the key. *)

  val nelements : t -> int
    (** [nelements t] gets the number of elements in the key. *)
end


module Make (Key : Vector_key) = struct

  module Int_map = Map.Make(struct
			      type t = int
			      let compare a b = a - b
			    end)

  type 'a map = 'a Int_map.t

  type key = Key.t

  and 'data t = Node of 'data t map * key option * 'data list | Nil

  let empty = Nil


  let successor e m =
    (** [successor e m] find [e] in [m] or return a fresh node. *)
    try Int_map.find e m with Not_found -> Nil


  let rec modify f key data nelms depth node =
    (** [modify f key data nelms depth node] make a modification to
	the trie.  [f] modifies the node matching the given key. *)
    match node with
      | Nil ->
	  let n = Node (Int_map.empty, None, [])
	  in modify f key data nelms depth n
      | (Node (cs, _, ds) as node) when depth = nelms ->
	  f node
      | Node (cs, k, ds) ->
	  let e = Key.element key depth in
	  let n = successor e cs in
	  let n' = modify f key data nelms (depth + 1) n in
	  let cs' = Int_map.add e n' cs in
	    Node(cs', k, ds)


  let add key data t =
    (** [add key data t] adds a mapping between [key] and [data] in
	the trie. *)
    let nelms = Key.nelements key in
      modify (function
		| Node (cs, _, ds) -> Node (cs, Some key,  data :: ds)
		| Nil -> failwith "Impossible")
	key data nelms 0 t


  let replace key data t =
    (** [replace key data t] replaces or adds a new mapping between
	[key] and [data] in the trie. *)
    let nelms = Key.nelements key in
      modify (function
		| Node (cs, _, ds) -> Node (cs, Some key,  [ data ])
		| Nil -> failwith "Impossible")
	key data nelms 0 t


  let find_all key t =
    (** [find_all key t] finds all data elements bound to [key] in the
	trie. *)
    let rec do_find_all key nelms depth = function
      | Nil -> []
      | Node (_, _, ds) when depth = nelms -> ds
      | Node (cs, _, _) ->
	  do_find_all key nelms (depth + 1)
	    (successor (Key.element key depth) cs)
    in
    let nelms = Key.nelements key in
      do_find_all key nelms 0 t


  let find key t =
    (** [find key t] finds the latest binding to the given key.
	Rasies Not_found if there was no binding. *)
    match find_all key t with
      | [] -> raise Not_found
      | e :: _ -> e


  let mem key t =
    (** [mem key t] tests if there is a binding to [key]. *)
    (find_all key t) <> []


  let rec iter f t =
    (** [iter f t] iterates [f] over the bindings. *)
    match t with
      | Nil -> ()
      | Node (cs, None, []) -> Int_map.iter (fun _ kid -> iter f kid) cs
      | Node (cs, Some k, ds) ->
	  List.iter (f k) ds;
	  Int_map.iter (fun _ kid -> iter f kid) cs;
      | Node (cs, None, _) -> failwith "Impossible: no key, but has data"


  let rec fold f init t =
    (** [fold f init t] fold [f] over the bindings. *)
    match t with
      | Nil -> init
      | Node (cs, None, []) ->
	  Int_map.fold (fun _ kid accum -> fold f accum kid) cs init
      | Node (cs, Some k, ds) ->
	  let v = List.fold_left (fun v e -> f v k e) init ds in
	    Int_map.fold (fun _ kid accum -> fold f accum kid) cs v;
      | Node (cs, None, _) -> failwith "Impossible: no key, but has data"


  let rec traverse f key ?(subs_only=false) ~i ~nelms ~edits node =
    (** [traverse f key ?subs_only ~i ~nelms ~edits node] traverses
	the trie and evaluates [f] on each node that is within [edits]
	edits of the given key. *)
    match node with
      | Nil -> ()
      | Node (kids, k, ds) ->
	  if i = nelms then f ds;
	  if i < nelms
	  then begin
	    let e = Key.element key i in
	    let n = successor e kids in
	      traverse ~subs_only f key ~i:(i + 1) ~nelms ~edits n;
	  end;
	  if edits > 0
	  then begin
	    subs subs_only f key ~i ~nelms ~edits kids;
	    if not subs_only
	    then begin
	      adds f key ~i ~nelms ~edits kids;
	      dels f key ~i ~nelms ~edits ds node;
	    end
	  end

  and subs subs_only f key ~i ~nelms ~edits kids =
    (** [subs subs_only f key ~i ~nelms ~edits kids] lookup with
	substitutions. *)
    if i < nelms
    then begin
      let i' = i + 1 in
      let e = Key.element key i in
      let edits' = edits - 1 in
	Int_map.iter
	  (fun vl n ->
	     if vl <> e
	     then traverse ~subs_only f key ~i:i' ~nelms ~edits:edits' n)
	  kids
    end

  and adds f key ~i ~nelms ~edits kids =
    (** [adds f key ~i ~nelms ~edits kids] lookup with adds. *)
    let edits' = edits - 1 in
      Int_map.iter
	(fun vl n -> traverse ~subs_only:false f key ~i ~nelms ~edits:edits' n)
	kids


  and dels f key ~i ~nelms ~edits ds node =
    (** [dels f key ~i ~nelms ~edits ds node] lookup with dels. *)
    let edits' = edits - 1 in
    let i' = i + 1 in
      traverse ~subs_only:false f key ~i:i' ~nelms ~edits:edits' node


  let lookup_with_edits key ?(subs_only=false) edits t =
    (** [lookup_with_edits key ?subs_only edits t] performs a lookup
	in the trie of [key] with some edits. *)
    let accum = ref [] in
    let nelms = Key.nelements key in
      traverse ~subs_only (fun ds -> accum := ds @ !accum)
	key ~i:0 ~nelms ~edits t;
      Wrlist.remove_duplicates !accum

end


module String_trie = Make(struct
			    type t = string
			    type elm = char
			    let element_width = 256
			    let element s i = int_of_char s.[i]
			    let nelements = String.length
			  end)


module Int_trie = Make(struct
			 type t = int
			 type elm = int
			 let element_width = 2
			 let element k i = ((1 lsl i) land k) lsr i
			 let nelements _ = Sys.word_size - 1
		       end)


let test () =
  let module S = String_trie in
  let st = S.empty in
  let st = S.add "a" "a" st in
  let st = S.add "b" "b" st in
  let st = S.add "c" "c" st in
  let st = S.add "aa" "aa" st in
  let st = S.add "ab" "ab" st in
  let st = S.add "ac" "ac" st in
  let st = S.add "ba" "ba" st in
  let st = S.add "bb" "bb" st in
  let st = S.add "bc" "bc" st in
  let st = S.add "ca" "ca" st in
  let st = S.add "cb" "cb" st in
  let st = S.add "cc" "cc" st in
  let st = S.add "abc" "abc" st in
    st
