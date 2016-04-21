(* $Id: wrht.ml,v 1.1 2003/06/27 21:15:55 ruml Exp ruml $

   misc hash table utility code
*)


(*************** construction **************)


let of_pairs ?(allow_dups = false) list =
  let ht = Hashtbl.create (List.length list) in
    List.iter (fun (k,v) ->
		 if not allow_dups
		 then assert (not (Hashtbl.mem ht k));
		 Hashtbl.replace ht k v)
      list ;
    ht


let of_lists ?(allow_dups = false) keys values =
  let ht = Hashtbl.create (List.length keys) in
    List.iter2 (fun k v ->
		  if not allow_dups
		  then assert (not (Hashtbl.mem ht k));
		  Hashtbl.replace ht k v)
      keys values;
    ht


let on_key f l =
  (** returns a hashtbl of the elements of [l] using [key].
    duplicates under a key value will be in the reverse order from [l] *)
  (* add presrve_dups keyword arg? *)
  let ht = Hashtbl.create (List.length l) in
    List.iter (fun x ->
		 Hashtbl.add ht (f x) x)
      l;
    ht


let of_list value list =
  (** maps all elements of [list] to [value] *)
  on_key (Fn.constantly1 value) list


let add_new ht key v =
  (** raises Failure iff [key] already in [ht] *)
  if Hashtbl.mem ht key then
    failwith "Wrht.add_new: already a member"
  else
    Hashtbl.add ht key v


(*************** iteration **************)


let map f ht =
  (** a list of the results of evaluating [f] on the keys and values *)
  Hashtbl.fold (fun k v accum ->
		  (f k v)::accum)
    ht []


let mapi f ht =
  let i = ref (-1) in
    map (fun k v ->
	   incr i;
	   (f !i k v))
      ht


(*************** querying **************)


let reverse_find ht value =
  (** list of all keys that have a binding = to [value] *)
  Hashtbl.fold (fun k v accum ->
		  if v = value then
		    k::accum
		  else
		    accum)
    ht []


let keys ht =
  (** returns a list of the keys in [ht] *)
  map (fun k _ -> k) ht


let values ht =
  (** returns a list of the values in [ht] *)
  map (fun _ v -> v) ht


let subset a b =
  (** true iff keys in Hashtbl [a] are a subset of those in Hashtbl [b] *)
  try
    Hashtbl.iter (fun k _ ->
		    if not (Hashtbl.mem b k) then raise Not_found)
      a;
    true
  with Not_found -> false


let subset_union a b1 b2 =
  (** true iff keys in Hashtbl [a] are a subset of those in Hashtbls [b1]
    and [b2] *)
  try
    Hashtbl.iter (fun k _ ->
		    if not ((Hashtbl.mem b1 k) ||
			    (Hashtbl.mem b2 k)) then
		      raise Not_found)
      a;
    true
  with Not_found -> false


(*************** searching **************)


let find_or_default ht key def =
  (** looks up [key] in [ht], using [def] as value on failure *)
  try
    Hashtbl.find ht key
  with Not_found -> def


let find_or_compute ht f key =
  (** looks up [key] in [ht], using [f] to compute and add the value on
    failure *)
  try
    Hashtbl.find ht key
  with Not_found ->
    let v = f key in
      Hashtbl.add ht key v;
      v


let push ht key v =
  (** for Hashtbls in which the value is a list *)
  Hashtbl.replace ht key (v::(find_or_default ht key []))


exception Found

let find_if p h =
  let r = ref None in
    try
      Hashtbl.iter (fun k v ->
		      if p k v then
			(r := Some (k,v);
			 raise Found))
	h;
      raise Not_found
    with Found ->
      match !r with
	None -> failwith "impossible in Wrht.find_if"
      | Some p -> p


(* EOF *)
