(** A ternary tree.  This allows for lookups with various edits.

    @author eaburns
    @since 2010-08-03
*)


module type Vector_key = sig
  type t

  type elm

  val element : t -> int -> elm
    (** [element_value t index] gets the element at the given index in
	the key. *)

  val element_cmp : elm -> elm -> int
    (** [element_cmp a b] compares the elements in the style of
	[compare]. *)

  val nelements : t -> int
    (** [nelements t] gets the number of elements in the key. *)
end


module Make (Key : Vector_key) = struct

  type key = Key.t
  type elm = Key.elm

  type 'data node =
    | Nil
    | Node of elm * 'data node * 'data node * 'data node
	* key option * 'data list


  let empty = Nil


  let rec compare_elements nelms a b i =
    (** [compare_elements nelms a b i] compares the elements of of key
	[a] and [b] which have equal lengths. *)
    if i >= nelms
    then 0
    else begin
      let ea = Key.element a i and eb = Key.element b i in
      let cmp = Key.element_cmp ea eb in
	match cmp with
	  | 0 -> compare_elements nelms a b (i + 1)
	  | cmp when cmp < 0 -> ~-1
	  | cmp -> 1
    end


  let compare_keys a b =
    (** [compare_keys a b] compares the two keys. *)
    let na = Key.nelements a in
    let nb = Key.nelements b in
      if na < nb
      then ~-1
      else begin
	if nb > na
	then ~-1
	else compare_elements na a b 0
      end


  let make_node e ~l ~c ~r k ds =
    (** [make_node e ~l ~c ~r k ds] makes a node and collapses the node
	into Nil if it is totally empty. *)
    if l = Nil && c = Nil && r = Nil && ds = []
    then Nil
    else Node (e, l, c, r, k, ds)


  let next_elm key i =
    (** [next_elm key i] gets the next element of the key. *)
    let i' = i + 1 in
    let elm' = Key.element key i' in
      elm', i'


  let rec modify f key elm ~i ~nelms node =
    (** [modify f key elm ~i ~nelms node] modifies the tree by calling
	[f] on the data for the node matching the key. *)
    match node with
      | Nil ->
	  let n = Node (elm, Nil, Nil, Nil, None, [])
	  in modify f key elm ~i ~nelms n
      | Node (e, l, c, r, k, ds) ->
	  begin match Key.element_cmp elm e with
	    | 0 when i = nelms - 1 ->
		let ds' = f ds in make_node e ~l ~c ~r (Some key) ds'
	    | 0 ->
		let elm', i' = next_elm key i in
		let c' = modify f key elm' ~i:i' ~nelms c
		in make_node e ~l ~c:c' ~r k ds
	    | cmp when cmp < 0 ->
		let l' = modify f key elm ~i ~nelms l
		in make_node e ~l:l' ~c ~r k ds
	    | _ ->
		let r' = modify f key elm ~i ~nelms r
		in make_node e ~l ~c ~r:r' k ds
	  end


  let add key data t =
    (** [add key data t] adds the binding of [data] to [key] to the
	ternary tree. *)
    let elm0 = Key.element key 0 in
    let nelms = Key.nelements key in
      modify (fun ds -> data :: ds) key elm0 ~i:0 ~nelms t


  let replace key data t =
    (** [replace key data t] replaces or adds the binding of [data] to
	[key] to the ternary tree. *)
    let elm0 = Key.element key 0 in
    let nelms = Key.nelements key in
      modify (fun ds -> [ data ]) key elm0 ~i:0 ~nelms t


  let remove key t =
    (** [remove key data t] removes the first piece of data bound to
	[key]. *)
    let elm0 = Key.element key 0 in
    let nelms = Key.nelements key in
      modify (function [] -> [] | _ :: ds -> ds) key elm0 ~i:0 ~nelms t


  let remove_all key t =
    (** [remove_all key data t] removes all bindings to [key]. *)
    let elm0 = Key.element key 0 in
    let nelms = Key.nelements key in
      modify (fun _ -> []) key elm0 ~i:0 ~nelms t


  type edit =
    | A of elm
    | D of elm
    | S of elm * elm
    | N of elm


  let consider_evaluating f edits key ds =
    (** [consider_evaluating f edits key ds] evaluates [f] on the key
	and data if there is data and a key. *)
    match key, ds with
      | None, [] -> ()
      | None, _ -> failwith "Data without a key"
      | Some k, [] -> failwith "Key without data"
      | Some k, ds -> List.iter (f edits k) ds


  let rec traverse f key elm ~i ~nelms ?(edits=0) ?(eds=[]) node =
    (** [traverse f key elm ~i ~nelms ?edits ?eds node] traverses the
	tree evaluating [f] on all nodes that match [key] or are are a
	specified number of edits different from the key. *)
    traverse_no_edit f key elm ~i ~nelms ~edits eds node;
    if edits > 0
    then begin
      traverse_subs f key elm ~i ~nelms ~edits eds node;
      traverse_adds f key elm ~i ~nelms ~edits eds node;
      traverse_dels f key elm ~i ~nelms ~edits eds node;
    end;
    begin match node with
      | Node (e, l, c, r, _, ds) ->
	  let cmp = Key.element_cmp elm e in
	    if cmp < 0 || edits > 0
	    then traverse f key elm ~i ~nelms ~edits ~eds l;
	    if cmp > 0 || edits > 0
	    then traverse f key elm ~i ~nelms ~edits ~eds r;
      | _ -> ()
    end


  and traverse_no_edit f key elm ~i ~nelms ~edits eds node =
    (** [traverse_no_edit f key elm ~i ~nelms ?edits eds node]
	traverses the tree evaluating [f] on all nodes that are within
	[edits] substitutions from [key]. *)
    begin match node with
      | Node (e, l, c, r, k, ds) when i <= nelms - 1 ->
	  let cmp = Key.element_cmp elm e in
	    if cmp = 0
	    then begin
	      if i = nelms - 1
	      then begin
		let eds' = (N elm) :: eds in
		  consider_evaluating f (List.rev eds') k ds;
		  traverse f key elm ~i:(i + 1) ~nelms ~edits ~eds:eds' c
	      end else begin
		let elm', i' = next_elm key i in
		  traverse f key elm' ~i:i' ~nelms ~edits
		    ~eds:((N elm) :: eds) c
	      end
	    end
      | Nil | _ -> ()
    end


  and traverse_subs f key elm ~i ~nelms ~edits eds node =
    (** [traverse_subs f key elm ~i ~nelms ?edits eds node] traverses
	the tree evaluating [f] on all nodes that are within [edits]
	substitutions from [key]. *)
    begin match node with
      | Node (e, _, c, _, k, ds) when i <= nelms - 1 ->
	  let cmp = Key.element_cmp elm e in
	    if cmp <> 0
	    then begin
	      if i = nelms - 1
	      then begin
		let eds' = (S (elm, e)) :: eds in
		  consider_evaluating f (List.rev eds') k ds;
		  traverse f key elm ~i:(i + 1) ~nelms ~edits:(edits - 1)
		    ~eds:eds' c
	      end else begin
		let elm', i' = next_elm key i in
		  traverse f key elm' ~i:i' ~nelms ~edits:(edits - 1)
		    ~eds:((S (elm, e)) :: eds) c
	      end;
	    end;
      | Nil | _ -> ()
    end


  and traverse_adds f key elm ~i ~nelms ~edits eds node =
    (** [traverse_adds f key elm ~i ~nelms ?edits eds node] traverses
	the tree evaluating [f] on all nodes that are within [edits]
	adds from [key]. *)
    begin match node with
      | Node (e, _, c, _, k, ds) ->
	  let eds' = (A e) :: eds in
	    if i <= nelms - 1
	    then traverse f key elm ~i ~nelms ~edits:(edits - 1) ~eds:eds' c
	    else begin
	      consider_evaluating f (List.rev eds') k ds;
	      traverse f key elm ~i:(i + 1) ~nelms ~edits:(edits - 1)
		~eds:eds' c;
	    end;
      | Nil -> ()
    end


  and traverse_dels f key elm ~i ~nelms ~edits eds node =
    (** [traverse_dels f key elm ~i ~nelms ?edits eds node] traverses
	the tree evaluating [f] on all nodes that are within [edits]
	deletes from [key]. *)
    begin match node with
      | Node (e, _, c, _, k, ds) when i <= nelms - 2 ->
	  if i = nelms - 2 && (Key.element_cmp elm e) = 0
	  then begin
	    let elm', i' = next_elm key i in
	    let eds' = (D elm') :: (N elm) :: eds in
	      consider_evaluating f (List.rev eds') k ds;
	      match c with
		| Node (_, _, cc, _, _, _) ->
		    traverse f key elm' ~i:nelms ~nelms ~edits:(edits - 1)
		      ~eds:eds' cc
		| _ -> ()
	  end else begin
	    let eds' = (D elm) :: eds in
	    let elm', i' = next_elm key i in
	      traverse f key elm' ~i:i' ~nelms ~edits:(edits - 1)
		~eds:eds' node
	  end
      | Nil | _ -> ()
    end


  let lookup key ?(edits=0) t =
    (** [lookup key ?edits t] finds data associated with keys that are
	various edit distances from the given key. *)
    let accum = ref [] in
    let elm0 = Key.element key 0 in
    let nelms = Key.nelements key in
      traverse (fun es k d ->
		    accum := (es, (k, d)) :: !accum)
	key elm0 ~i:0 ~nelms ~edits t;
      let paths, ds = List.split !accum in
      let eds = List.map (fun path ->
			    List.filter (function N _ -> false | _ -> true)
			      path)
	paths
      in
	List.iter (fun ed -> assert ((List.length ed) <= edits)) eds;
	Wrlist.remove_duplicates ds


  let find key t =
    (** [find key t] finds the value assocated with the given key. *)
    match lookup key ~edits:0 t with
      | [] -> raise Not_found
      | (k, d) :: es -> d


  let find_all key t =
    (** [find_all key t] finds all of the values assocated with the
	given key. *)
    match lookup key ~edits:0 t with
      | [] -> raise Not_found
      | es -> snd (List.split es)


  let mem key t =
    (** [mem key t] is the membership test. *)
    (lookup key ~edits:0 t) <> []


  let is_empty =
    (** [is_empty t] tests if the tree is empty. *)
    function Nil -> true | _ -> false

end


module With_string_keys = Make(struct
				 type t = string
				 type elm = char
				 let element k i = k.[i]
				 let element_cmp = compare
				 let nelements = String.length
			       end)



let test () =
  let module S = With_string_keys in
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
