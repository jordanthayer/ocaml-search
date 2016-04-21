(* $Id: wrlist.ml,v 1.6 2006/08/07 18:25:57 ruml Exp ruml $

   List functions
*)

(*
  checks if the list has stuff in it
*)

let empty lst =
  match lst with
      [] -> true
    | _ -> false


let rec make n x =
  (** returns a list of length [n] filled with [x] *)
  if n < 0 then
    invalid_arg "make: negative length"
  else if n = 0 then
    []
  else
    x::(make (n - 1) x)


let of_opt = function
    (** returns a list of zero or one elements *)
    None -> []
  | Some x -> [ x ]


(* Build a list that is a range of values between min and max.
   See Wrutils.map_n for a function that may be more efficient than
   this *)
let rec range ?(accum=[]) ?(step=pred) ?(min=1) max =
  if max < min then accum
  else range ~accum:(max :: accum) ~step:step ~min:min (step max)


let split list point =
  (* Splits the list, giving you a tuple of everything before the point'th
     element and everything after *)
  let rec do_it accum n l =
    if n > 0
    then (match l with
	    | hd :: tl -> do_it (hd::accum) (n-1) tl
	    | _ -> list, [])
    else (List.rev accum), l in
    if point > (List.length list) then list,[]
    else (do_it [] point list)

(******* iteration and mapping ************)

let sample size lst =
  (** [sample size lst] gets a sample of the elements in the list.

      This is a modified version of Reservoir sampling from Knuth's
      "The Art of Computer Programming" vol. 2. *)
  assert ((List.length lst) > size);
  assert ((List.length lst) > 0);
  let t = ref 0 in
  let sample = Array.make size (List.hd lst) in
    List.iter (fun e ->
		 if !t < size
		 then sample.(!t) <- e
		 else begin
		   let r = Random.int (!t + 1) in
		     if r < size then sample.(r) <- e
		 end;
		 incr t)
      lst;
    sample


let mapi f l =
  (** maps [f] over [l], calling [f] with the position in the list and
      the corresponding element *)
  let i = ref (-1) in
    List.map (fun x ->
		incr i;
		f !i x)
      l


let map2i f a b =
  let i = ref (-1) in
    List.map2 (fun x y ->
		 incr i;
		 f !i x y)
      a b


let iteri f l =
  (** iterates [f] over [l], calling [f] with the position in the list
    and the corresponding element *)
  let i = ref (-1) in
    List.iter (fun x ->
		 incr i;
		 f !i x)
      l


let iter2i f a b =
  (** iterates [f] over two lists at once *)
  let count = ref (-1) in
    List.iter2 (fun a b ->
		  incr count;
		  f !count a b)
      a b


let rec rev_iter (f : 'a -> unit) = function
    (** iterates [f] over the element of list in reverse order.  uses
      stack space in proportion to the length of the list *)
    [] -> ()
  | x::rest ->
      rev_iter f rest;
      f x;
      ()


let rec mapcan f = function
    (** evaluates [f] on all elements of a list, concatenating the
      resulting lists.  Not tail recursive. *)
    [] -> []
  | h::t ->
      (* ensure evaluating f on head first *)
      let this = f h in
	List.append this (mapcan f t)


let mapcan2 f list =
  (** evaluates [f] on all elements of a list, concatenating the
      resulting lists. tail recursive but allocates more. *)
  let rec helper so_far = function
      [] -> so_far
    | h::t -> helper (List.rev_append (f h) so_far) t
  in
    List.rev (helper [] list)


let mapcani f list =
  (** like [mapcan] but passed index as first value *)
  let rec aux i = function
      [] -> []
    | h::t ->
	List.append (f i h) (aux (i+1) t)
  in
    aux 0 list


let rec iterlist f = function
    (** iterates [f] over the sublists of [list] *)
    [] -> ()
  | (_::rest) as l -> f l; iterlist f rest


let rec for_all_list f = function
    (** like [List.for_all], but calls [f] on sublists instead of
      elements *)
    [] -> true
  | (_::t) as l ->
      if f l
      then for_all_list f t
      else false


let exists_list f l =
    (** like [List.exists], but calls [f] on sublists instead of
      elements *)
  (* for_all = not exists not.  exists = not forall not. *)
  not (for_all_list (fun x -> not (f x)) l)


let constant_p = function
    [] -> true
  | first::rest ->
      List.for_all (fun x -> x = first) rest


let map_opt f l =
  (** [f] returns an optional result.  [map_opt] returns a list of the
    non-None results on [l] *)
  let l = List.fold_left
	    (fun accum x ->
	       match f x with
	       | None -> accum
	       | Some y -> y::accum)
	    [] l
  in
    List.rev l


let mapi_opt f l =
  (** [f] on index and element should return an optional result.
    [mapi_opt] returns a list of the non-None results on [l] *)
  let i = ref (-1) in
    List.fold_left
      (fun accum x ->
	 incr i;
	 match f !i x with
	 | None -> accum
	 | Some y -> y::accum)
      [] l


let rec exists_opt f l =
  (** [f] returns an optional result.  [exists_opt] returns None or
    the first non-None result of [f] on elements of [l] (from the left) *)
  match l with
  | [] -> None
  | h::t -> match f h with
    | Some x as r -> r
    | None -> exists_opt f t


let rec mapcan_to_3 f = function
    (** maps a function that returns 3 lists down a list, returning 3
      lists as a result *)
    [] -> [], [], []
  | h::t ->
      let a, b, c = f h in
      let x, y, z = mapcan_to_3 f t in
	(a @ x), (b @ y), (c @ z)


let rec mapcan_to_4 f = function
    [] -> [], [], [], []
  | h::t ->
      let a, b, c, d = f h in
      let w, x, y, z = mapcan_to_4 f t in
	(a @ w), (b @ x), (c @ y), (d @ z)


let rec foldrest_left f start list =
  (** fold [f] over all tails of [list] *)
  match list with
      [] -> start
    | _::tail -> foldrest_left f (f start list) tail


let rec fold_pairs f initial = function
    (** folds [f], starting with [initial], over all unordered pairs of
      elements from [list] *)
    [] -> initial
  | h::t ->
      let initial = List.fold_left (fun prev x ->
				      f prev h x)
		      initial t in
	fold_pairs f initial t


let fold_combos f initial l1 l2 =
  (** folds [f] over all combinations of elements from [l1] and [l2] *)
  List.fold_left (fun accum e1 ->
		    List.fold_left (fun accum e2 ->
				      f accum e1 e2)
		    accum l2)
    initial l1


(************* lists as sets **********)


let set_equal l1 l2 =
  (** must have same values by [=].  n log(n) time. *)
  let l1 = List.sort compare l1
  and l2 = List.sort compare l2 in
  let rec and2 l1 l2 =
    match l1 with
      h1::r1 -> (match l2 with
		   h2::r2 ->
		     if h1 = h2
		     then and2 r1 r2
		     else false
		 | [] -> false)
    | [] -> (match l2 with
	       [] -> true
	     | _ -> false)
  in
    and2 l1 l2


let is_subset ?(strict = false) l1 l2 =
  (** true iff [l1] is a subset of [l2].  Ie, [l2] must contain all
    elements of [l1] (by =).  n log(n) time. *)
  let l1 = List.sort compare l1
  and l2 = List.sort compare l2
  and extra = ref false in
  let rec sub2 l1 l2 =
    match l1 with
      h1::r1 -> (match l2 with
		   h2::r2 ->
		     (* is h1 in l2? *)
		     if (h1 = h2) then
		       sub2 r1 r2
			 (* h1 can be greater than h2 since l2
			    can have more elements *)
		     else if (h1 > h2) then
		       (extra := true;
		        sub2 l1 r2)
		     else
		       false
		 | [] -> false)
    | [] ->
	if l2 <> [] then extra := true;
	true
  in
  let base = sub2 l1 l2 in
    base && ((not strict) || !extra)


let intersection_by key lista listb =
  (** returns list of elements that have the same key value *)
  List.filter (fun a ->
		 let k = key a in
		   List.exists (fun b -> (key b) = k)
		     listb)
    lista


let intersection lista listb =
  intersection_by Fn.identity lista listb


let intersects_q a b =
  (** true iff a and b have any == elements *)
  List.exists (fun a_elt ->
		 List.memq a_elt b)
    a


let subtractq a b =
  (** returns elements of [a] not in [b] *)
  List.filter (fun a_elt ->
		 not (List.memq a_elt b))
    a


let random_elt = function
    (** returns a random element *)
    [] -> invalid_arg "random_elt: empty list"
  | h::t ->
      let e = ref h
      and count = ref 1 in
	List.iter (fun x ->
		     count := !count + 1;
		     let p = 1. /. (float !count) in
		       if (Random.float 1.) < p
		       then e := x)
	  t;
	!e


let partition_and_map pred f list =
  (** returns elements of [list] that pass and that fail [pred].  The
    elements are run through [f] before being returned. *)
  let rec part yes no = function
      [] -> (List.rev yes), (List.rev no)
    | h::t ->
	if pred h
	then part ((f h)::yes) no t
	else part yes ((f h)::no) t
  in
    part [] [] list


let is_singleton = function
    _::[] -> true
  | _ -> false

(** Get all unique (ignoring ordering) pairs of elements in the
    list. *)
let rec pairs = function
  | [] -> []
  | h :: t -> (List.map (fun e -> h, e) t) @ (pairs t)

(** Gets the cartesian product of list [a] and list [b]. *)
let rec cross a b = match a with
  | [] -> []
  | hd :: tl -> (List.map (fun b -> hd, b) b) @ cross tl b

let best (fn:'a -> 'a -> 'a) (lst:'a list) =
  (**looks at all items in the list and gives back the one that best
     satisfies the fn*)
  match lst with
      [] -> failwith "error selecing best item from an empty list"
    | _ -> let best_so_far = ref (List.hd lst) in
	List.iter (fun next_item ->
		     best_so_far := fn next_item !best_so_far;
		  ) lst;
	!best_so_far

(************* detecting duplication **********)


let dups_p key l =
  (** true iff there are duplicates (by = on results of [key]) in [l] *)
  let c = Dset.create 64 in
  let rec examine = function
      [] -> false
    | h::t ->
	if (not (Dset.add_new_p (key h) c))
	then true
	else examine t
  in
    examine l


let duplicates_p l = dups_p Fn.identity l


let remove_dups key l =
  (** leftmost element to be [=] to a given key is retained.  order is
    preserved. *)
  let s = Dset.create 64 in
  let unique = List.fold_left (fun prev x ->
				 if (Dset.add_new_p (key x) s)
				 then x::prev
				 else prev)
		 [] l
  in
    List.rev unique


let remove_duplicates l = remove_dups Fn.identity l


let rec remove_dups_if eq_test = function
    (** O(n^2) in time and space. *)
    [] -> []
  | h::t ->
      let rest = List.filter (fun x -> not (eq_test h x)) t
      in
	h::(remove_dups_if eq_test rest)


let classes key l =
  (** returns list of equivalence classes of elements in L.  Classes
    are according to [=] on [key] *)
  Eqvclass.to_lists (Eqvclass.from_list key l)


let dups key l =
  (** returns a list of lists of items from [l], one list for each
    item in L that has a duplicate (according to [=] on the results of
    [key]) *)
  let ec = Eqvclass.from_list key l in
    (* very much like Eqvclass.to_lists, but throw out singletons *)
    Eqvclass.fold
      (* before each class, start with [] *)
      (fun dups rep -> [])
      (* in each class, accumulate objects *)
      (fun so_far x -> x::so_far)
      (* after each class, keep only those with multiple objects *)
      (fun dups eqv ->
	 match eqv with
	   a::_::_ -> eqv::dups
	 | _ -> dups)
      [] ec


let duplicates l =
  (** returns a list of the items which appear multiple times in [l]
    according to = *)
  List.map List.hd (dups Fn.identity l)


let rec common_prefix l1 l2 =
  match l1 with
  | h1::t1 ->
      (match l2 with
       | h2::t2 when h1 = h2 ->
	   h1::(common_prefix t1 t2)
       | _ -> [])
  | [] -> []


let rec remove_common_prefix l1 l2 =
  match l1 with
  | h1::t1 -> (match l2 with
	       | h2::t2 when h1 = h2 ->
		   remove_common_prefix t1 t2
	       | _ -> l1, l2)
  | [] -> l1, l2


(******* finding and removing *********)


let second list =
  List.nth list 1


let rec last = function
    (** returns the last element in the list *)
    [] -> raise Not_found
  | x::[] -> x
  | _::rest -> last rest


let butlast ?(n = 1) l =
  (** list containing all but the last [n] elements of [l] *)
  let rec skip n l =
    match l with
      [] -> [], n
    | h::t ->
	let t, n = skip n t in
	  if n = 0 then
	    (h::t), n
	  else
	    t, (n-1)
  in
    fst (skip n l)


let rec firstn list n =
  (** list containing first [n] elements of [list] *)
  if n < 0 then
    invalid_arg "negative n to Wrlist.firstn"
  else if n = 0 then
    []
  else
    match list with
      [] -> []
    | h::t ->
	let rest = firstn t (n - 1) in
	  h::rest


let rec prefix n l =
  (** returns the first [n] elements of [l] and the rest of the list *)
  if n < 0 then
    invalid_arg "negative n to divide"
  else if n = 0 then
    [], l
  else
    match l with
	[] -> invalid_arg "list too short in divide"
      | f::r ->
	  let a,b = prefix (n-1) r in
	    (f::a),b


let rec mem_key key target list =
  (** true iff [key], applied to an element of [list], gives [target]
    (by =) *)
  List.exists (fun x -> (key x) = target) list


let rec remove target list =
  (** fresh list with all (=) occurrences of [target] removed *)
  match list with
    [] -> []
  | h::t -> (if h = target then remove target t else h::remove target t)


let rec remove_first target list =
  (**  [list] without leftmost (=) occurrence of [target].  no exception if
    [target] is not found. *)
  match list with
    [] -> []
  | h::t -> if h = target then t else h::(remove_first target t)


let rec remove_firstq target list =
  (** [list] without leftmost occurrence of [target], using physical
    equality.  no exception is raised if [target] is not found.  *)
  match list with
    [] -> []
  | h::t -> if h == target then t else h::(remove_firstq target t)


let rec remove_if_first pred list =
  (** fresh list with first element satisfying [pred] removed *)
  match list with
    [] -> []
  | h::t -> if pred h then t else h::(remove_if_first pred t)


let rec remove_n_if n pred list =
  (** fresh list with the [n] leftmost elements satisfying [pred] removed *)
  assert (n >= 0);
  if n = 0 then
    list
  else
    match list with
	[] -> []
      | h::t ->
	  if pred h then
	    remove_n_if (n-1) pred t
	  else
	    h::(remove_n_if n pred t)

(* Remove and return the nth element in the list.  Indexes begin at
   zero. *)
let rec takenth ?(accum=[]) n = function
  | [] -> invalid_arg "Wrlist.takenth: List is not of sufficient length"
  | hd :: tl when n = 0 -> hd, List.rev_append accum tl
  | hd :: tl -> takenth ~accum:(hd :: accum) (n - 1) tl



let find_index pred list =
  (** leftmost index in [list] where [pred] is true.  Can raise
      Not_found *)
  let rec aux i = function
      [] -> raise Not_found
    | h::t ->
	if pred h
	then i
	else aux (i+1) t
  in
    aux 0 list


let rec find_and_remove pred = function
    (** returns first element to satisfy [pred] and a list of the
      other elements in the list.  Can raise Not_found. *)
    [] -> raise Not_found
  | h::t ->
      if pred h then
	h, t
      else
	let ret, rest = find_and_remove pred t in
	  ret, h::rest


let rec replace target replacement l =
  (** replaces [target] with [replacement] throughout list [l] *)
  match l with
    [] -> []
  | h::t ->
      (if target = h then replacement else h)::(replace target replacement t)


let rec replace_alist pairs l =
  (** [pairs] is an alist of targets and correponding replacements for
    list L *)
  match l with
    [] -> []
  | h::t ->
      (try List.assoc h pairs
       with Not_found -> h)::(replace_alist pairs t)


let rec replace_nth i repl list =
  (** copies [list], except that the [i]th element becomes [repl] *)
  if i < 0 then
    failwith "replace_nth: negative index"
  else
    match list with
      [] -> failwith "replace_nth: end of list - bad index?"
    | h::t ->
	if i = 0 then
	  repl::t
	else h::(replace_nth (i - 1) repl t)

let copy lst =
  Array.to_list (Array.copy (Array.of_list lst))


let count pred l =
  (** number of elements in [l] that satisfy [pred] *)
  let c = ref 0 in
    List.iter (fun x -> if (pred x) then incr c)
      l;
    !c


exception Multiple

let get_singleton = function
    (** given a list of one element, returns the element.  raises
      Not_found if no elements, Wrlist.Multiple if more than one *)
    x::[] -> x
  | _::_ -> raise Multiple
  | [] -> raise Not_found


let find_unique pred l =
  (** like List.find but ensures that there is only one match.  raises
    Not_found if no matches, Failure _ if more than one. *)
  get_singleton (List.filter pred l)


let rec find_opt f l =
  (** [f] returns an optional result.  [find_opt] returns the first
    non-None result of [f] on elements of [l] (from the left) or raises
    Not_found *)
  match l with
  | [] -> raise Not_found
  | h::t -> match f h with
    | Some x -> x
    | None -> find_opt f t


let rec remove_doublet target l =
  (** finds [target] in [l], returning the next element and the list
    without either of them.  Can raise Not_found.  useful for command line
    argument processing. *)
  match l with
    []
  | _::[] -> raise Not_found
  | k::v::rest ->
      if k = target then
	v, rest
      else
	let res,rest = remove_doublet target rest in
	  res, (k::v::rest)


(****************** lists of numbers ******************)


let max_by f = function
    (** returns element and score *)
    [] -> invalid_arg "Wrlist.max_by: given empty list"
  | h::t ->
      let max = ref (f h)
      and max_x = ref h in
	List.iter (fun x ->
		     let this = f x in
		       if (this > !max) then
			 (max := this;
			  max_x := x))
	  t;
	!max_x, !max


let fmax_by f = function
    (** returns element and score *)
    [] -> invalid_arg "Wrlist.max_by: given empty list"
  | h::t ->
      let max = ref (f h)
      and max_x = ref h in
	List.iter (fun x ->
		     let this = f x in
		       if (this > !max) then
			 (max := (this : float);
			  max_x := x))
	  t;
	!max_x, !max


let min_by f = function
    (** returns element and score *)
    (* can't just wrap max_by using negation - that would assume a
       numeric type! *)
    [] -> invalid_arg "Wrlist.min_by: given empty list"
  | h::t ->
      let min = ref (f h)
      and min_x = ref h in
	List.iter (fun x ->
		     let this = f x in
		       if (this < !min) then
			 (min := this;
			  min_x := x))
	  t;
	!min_x, !min


let fmin_by f = function
    (** returns element and score *)
    (* can't just wrap max_by using negation - that would assume a
       numeric type! *)
    [] -> invalid_arg "Wrlist.min_by: given empty list"
  | h::t ->
      let min = ref (f h)
      and min_x = ref h in
	List.iter (fun x ->
		     let this = f x in
		       if (this < !min) then
			 (min := (this : float);
			  min_x := x))
	  t;
	!min_x, !min


let minf_by f = function
    (** returns score (a float) *)
    [] -> invalid_arg "Wrlist.minf_by: given empty list"
  | h::t ->
      let min = ref (f h) in
	List.iter (fun x ->
		     let this = f x in
		       if (this < !min) then
			 min := (this : float))
	  t;
	!min


let maxes_by f = function
    (** returns a list of elements and also a score *)
    [] -> invalid_arg "Wrlist.maxes_by: given empty list"
  | h::t ->
      let max = ref (f h)
      and ties = ref [h] in
	List.iter (fun x ->
		     let this = f x in
		       if (this > !max) then
			 (max := this;
			  ties := [x])
		       else if (this = !max) then
			 ties := x::!ties)
	  t;
	!ties, !max


let mins_by f = function
    (** returns a list of elements and also a score *)
    [] -> invalid_arg "Wrlist.mins_by: given empty list"
  | h::t ->
      let min = ref (f h)
      and ties = ref [h] in
	List.iter (fun x ->
		     let this = f x in
		       if (this < !min) then
			 (min := this;
			  ties := [x])
		       else if (this = !min) then
			 ties := x::!ties)
	  t;
	!ties, !min

let fmins_by f = function
    (** returns a list of elements and also a score *)
    [] -> invalid_arg "Wrlist.mins_by: given empty list"
  | h::t ->
      let min = ref (f h)
      and ties = ref [h] in
	List.iter (fun x ->
		     let this = f x in
		       if ((this:float) < !min) then
			 (min := this;
			  ties := [x])
		       else if (this = !min) then
			 ties := x::!ties)
	  t;
	!ties, !min


let min_and_max_by f = function
    (** returns elements scoring min and max according to [f] *)
    [] -> invalid_arg "Wrlist.min_and_max_by: given empty list"
  | h::t ->
      let min = ref (f h)
      and min_x = ref h
      and max = ref (f h)
      and max_x = ref h in
	List.iter (fun x ->
		     let this = f x in
		       if (this > !max) then
			 (max := this;
			  max_x := x)
		       else if (this < !min) then
			 (min := this;
			  min_x := x))
	  t;
	!min_x, !max_x


let fmin_and_fmax_by f = function
    (** returns elements scoring min and max according to [f] *)
    [] -> invalid_arg "Wrlist.min_and_max_by: given empty list"
  | h::t ->
      let min = ref (f h)
      and min_x = ref h
      and max = ref (f h)
      and max_x = ref h in
	List.iter (fun x ->
		     let this = f x in
		       if (this > !max) then
			 (max := (this : float);
			  max_x := x)
		       else if (this < !min) then
			 (min := (this : float);
			  min_x := x))
	  t;
	!min_x, !max_x


let min_and_max_across f = function
    (** [f] should return a min,max pair *)
    [] -> invalid_arg "Wrlist.min_and_max_across: given empty list"
  | h::t ->
      let min1, max1 = f h in
      let min = ref min1
      and max = ref max1 in
	List.iter (fun x ->
		     let this_min, this_max = f x in
		       if (this_min < !min) then
			 min := this_min;
		       if (this_max > !max) then
			 max := this_max)
	  t;
	!min, !max


let fmin_and_fmax_across f = function
    (** [f] should return a min,max pair *)
    [] -> invalid_arg "Wrlist.min_and_max_across: given empty list"
  | h::t ->
      let min1, max1 = f h in
      let min = ref min1
      and max = ref max1 in
	List.iter (fun x ->
		     let this_min, this_max = f x in
		       if (this_min < !min) then
			 min := (this_min : float);
		       if (this_max > !max) then
			 max := (this_max : float))
	  t;
	!min, !max


let sum_ints key l =
  List.fold_left (fun a x -> a + (key x)) 0 l


let sum_floats key l =
  List.fold_left (fun a x -> a +. (key x)) 0. l


let mean key l =
  (sum_floats key l) /. (float (List.length l))


let rec transpose = function
    (**  given a list of lists m(all the same length), returns a list
      of all the first elements, all the second elements,...  uses stack
      space proportional to the number of lists.  allocates one
      unecessary cons cell for every element. *)
    [] -> failwith "transpose"
  | list::[] -> List.map (fun x -> [x]) list
  | this::rest ->
      let tails = transpose rest in
	List.map2 (fun h t -> h::t) this tails


let transpose2 = function
  (** uses constant stack space, but allocates two unnecessary cons
    cells for every element.  *)
    [] -> failwith "transpose2"
  | (first::_) as lists ->
      let accumulators = List.map (fun _ -> []) first in
      let results = List.fold_left (fun accums list ->
				      List.map2 (fun prev this ->
						   this::prev)
				      accums list)
		      accumulators lists
      in
	List.map List.rev results


(******** sorting **********)


let sort_on ?(cmp=compare) key list =
  (** calls [key] every time to get obj for sorting [list] into increasing
    order (via Pervasives.compare) *)
  List.fast_sort (fun a b -> cmp (key a) (key b)) list


let sort_using key list =
  (** calls [key] once for each element of [list] and returns the
    elements in increasing order of that value *)
  List.map snd
    (sort_on fst (List.map (fun x ->
			      (key x), x)
		    list))


let shuffle list =
  sort_using (fun x -> Random.bits ())
    list


let rec insert compare item = function
    (** inserts [item] into a [list] that is already ordered by [compare],
      which should be positive iff its args are out of order (for instance,
      [Pervasives.compare] would give ascending order).  Advantage over
      [List.merge] is no wasted cons for [item]. In case of ties, [item] is
      put first. *)
    [] -> [ item ]
  | (h::t as l) ->
      if ((compare item h) > 0) then
	h::(insert compare item t)
      else item::l


(*
  let insertion_sort (l : (float * 'a) list) =
(** takes a list of (float * 'a) and returns in decreasing order *)
  (* start at end of old list and work backwards, creating new list *)
  let rec sorted_tail = function
      [] -> []
    | h::t -> add_to_sorted h (sorted_tail t)
  in
    sorted_tail l
    *)


(******** alists **********)


let get_entry alist type_name key =
  (** for alists from strings to 'a.  wraps List.assoc with error
    message generation *)
  try
    List.assoc key alist
  with Not_found ->
    Wrutils.pe "\nPossible %ss are:\n" type_name;
    List.iter (fun (n,_) -> Wrutils.pe "    \"%s\"\n" n) alist;
    failwith (Wrutils.str "unknown %s: %s" type_name key)


let make_lookup key_name alist =
  (fun key ->
     get_entry alist key_name key)


let assoc_default key alist_ref f =
  (** if [key] isn't in the alist at [alist_ref], creates entry using
    value returned by calling [f] on (). *)
  try
    List.assoc key !alist_ref
  with Not_found ->
    let res = f () in
      alist_ref := (key, res)::!alist_ref ;
      res


let rec assoc_remove key = function
    (** returns the value associated with [key] and the alist without
      that pair *)
    [] -> raise Not_found
  | ((k, v) as pair)::rest ->
      if k = key then
	v, rest
      else
	let r, t = assoc_remove key rest in
	  r, (pair::t)


let update_alist key f alist =
  (** calls [f] on the value for [key] in [alist], using the result as
    the value for [key] in a new alist which is returned.  Can raise
    Not_found. *)
  let value, rest = assoc_remove key alist in
  let new_value = f value in
    (key, new_value)::rest


let rec alist_values = function
    (** given an alist, returns a list of the second values *)
    [] -> []
  | (_,value)::rest -> value::(alist_values rest)


let rec merge_alists a1 a2 =
  (** merge two alists.  does NOT eliminate duplicate values. *)
  match a1 with
    [] -> a2
  | ((k,v) as p)::rest ->
      try
	let v2,others = assoc_remove k a2 in
	let updated = (k, (v @ v2))::others in
	  merge_alists rest updated
      with Not_found -> merge_alists rest (p::a2)


(***** printing ******)


let fprint_iter ch f interposed list =
  (** calls [f] on out_channel [ch] and elemets of [list], printing
    [interposed] to [ch] between the calls *)
  ignore (List.fold_left (fun start item ->
			    if not start then
			      output_string ch interposed ;
			    (f ch item) ;
			    false)
	    true list)


let fprint ch key interposed list =
  (** prints [list] on [ch], printing [interposed] between the
    elements.  [key] returns the string to print for each element *)
  fprint_iter ch (fun c item -> output_string c (key item))
    interposed list


let sprint key interposed list =
  (** forms a string from [list] *)
  String.concat interposed (List.map key list)


(***** pretty printing ******)


let pp_print ch
  ?(prefix = "")
  ?(delimiter = "")
  ?(print_space = true)
  ?(suffix = "")
  f list =
  (** calls [f] on elements.  takes a pretty printing formatter as [ch] *)
  (* box for everything *)
  Format.pp_open_box ch 0;
  Format.pp_print_string ch prefix;
  (* box for list elements *)
  Format.pp_open_box ch 0;
  ignore (List.fold_left (fun start item ->
			    if not start then
			      (Format.pp_print_string ch delimiter ;
			       Format.pp_close_box ch () ;
			       if print_space
			       then Format.pp_print_space ch ()
			       else Format.pp_print_cut ch ());
			    Format.pp_open_box ch 0 ;
			    f ch item;
			    false)
	    true list);
  (* box for last element *)
  Format.pp_close_box ch ();
  (* box for list elements *)
  Format.pp_close_box ch ();
  Format.pp_print_string ch suffix;
  (* box for everything *)
  Format.pp_close_box ch ()


let fpprint ch
  ?(prefix = "")
  ?(delimiter = "")
  ?(print_space = true)
  ?(suffix = "")
  f list =
  (** takes a output channel as [ch].  prints [prefix] and then a list of
    [f] over [list].  [f] should use pretty printing commands from the Format
    module. *)
  (* can't use Wrio.with_pp because it depends on wrstring which
     depends on us! *)
  flush ch;
  let ch = Format.formatter_of_out_channel ch in
    pp_print ch
      ~prefix:prefix
      ~delimiter:delimiter
      ~print_space:print_space
      ~suffix:suffix
      f list;
    Format.pp_print_flush ch ()


let test_fpprint n =
  fpprint
    stdout
    ~prefix:"A rather long prefix ("
    ~delimiter:","
    ~suffix:")\n"
    Format.pp_print_string
    (Wrutils.map_n (fun i -> "element-" ^ (string_of_int i)) n)


let test_fpprint2 n m =
  fpprint
    stdout
    ~prefix:"A rather long prefix ("
    ~delimiter:","
    ~suffix:")\n"
    (fun ch base ->
       pp_print ch
       ~prefix:(base ^ "-prefix (")
       ~delimiter:","
       ~suffix:")"
       Format.pp_print_string
       (Wrutils.map_n (fun i -> base ^ "-elt-" ^ (string_of_int i)) m))
    (Wrutils.map_n (fun i -> "elt-" ^ (string_of_int i)) n)


(* EOF *)
