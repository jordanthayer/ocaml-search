(* $Id: wrarray.ml,v 1.3 2006/06/25 19:40:41 ruml Exp ruml $

   array utility code
*)


exception Flag


(********* multi-dimensional arrays **********)

let make3darray i1 i2 i3 item =
  (*makes  a 3d array filled with COPIES of items.*)
  Array.init i1 (fun _ -> (
		   Array.init i2 (fun _ -> (
				    Array.init i3 (fun _ -> item)))));;

let init_matrix one two f =
  (** [init_matrix one two f] make a 2d array with the dimensions
      [one]x[two] given the initialization function [f] that takes 2
      argument, the index in each array. *)
  Array.init one (fun on -> Array.init two (fun tw -> f on tw))


let map_matrix mat fn =
  init_matrix (Array.length mat) (Array.length mat.(0)) (fun x y ->
							   fn mat.(x).(y))

let copy_matrix mat =
  map_matrix mat (fun n -> n)


let iteri_matrix f ary =
  (** [iter_matrix f ary] iterates [f] over a 2 dimensional array. *)
  Array.iteri
    (fun one ary1 ->
       Array.iteri
	 (fun two vl -> f one two vl)
	 ary1)
    ary


let make_3d one two three init =
  (** [make_3d one two three init] make a 3d array with the
      dimensions [one]x[two]x[three] given the initial value
      [init]. *)
  Array.init
    one
    (fun _ ->
       Array.init
	 two
	 (fun _ ->
	    Array.make three init))


let init_3d one two three f =
  (** [init_3d one two three f] make a 3d array with the
      dimensions [one]x[two]x[three] given the initialization
      function [f] that takes 3 argument, the index in each array. *)
  Array.init
    one
    (fun on ->
       Array.init
	 two
	 (fun tw -> Array.init three (fun th -> f on tw th)))


let iteri_3d f ary =
  (** [iter_3d f ary] iterates [f] over a 3 dimensional array. *)
  Array.iteri
    (fun one ary1->
       Array.iteri
	 (fun two ary2 ->
	    Array.iteri
	      (fun three vl -> f one two three vl)
	      ary2)
	 ary1)
    ary



let make_4d one two three four init =
  (** [make_4d one two three four init] make a 4d array with the
      dimensions [one]x[two]x[three]x[four] given the initial value
      [init]. *)
  Array.init
    one
    (fun _ ->
       Array.init
	 two
	 (fun _ ->
	    Array.init
	      three
	      (fun _ -> Array.make four init)))


let init_4d one two three four f =
  (** [init_4d one two three four init] make a 4d array with the
      dimensions [one]x[two]x[three]x[four] given the initialization
      function [f] that takes 4 argument, the index in each array. *)
  Array.init
    one
    (fun on ->
       Array.init
	 two
	 (fun tw ->
	    Array.init
	      three
	      (fun th -> Array.init four (fun fo -> f on tw th fo))))


let iteri_4d f ary =
  (** [iter_4d f ary] iterates [f] over a 4 dimensional array. *)
  Array.iteri
    (fun one ary1->
       Array.iteri
	 (fun two ary2 ->
	    Array.iteri
	      (fun three ary3 ->
		 Array.iteri
		   (fun four vl -> f one two three four vl)
		   ary3)
	      ary2)
	 ary1)
    ary

let make_5d one two three four five init =
  (** [make_5d one two three four five init] make a 5d array with the
      dimensions [one]x[two]x[three]x[four] given the initial value
      [init]. *)
  Array.init
    one
    (fun _ ->
       Array.init
	 two
	 (fun _ ->
	    Array.init
	      three
	      (fun _ ->
		 Array.init
		   four
		   (fun _ -> Array.make five init))))



(********* flow of control and mapping **********)


let fold_lefti f start ar =
  (** [f] is called on folded_val, index, and element *)
  let res = ref start in
    Array.iteri (fun i x ->
		   res := f !res i x)
      ar;
    !res


let fold_righti f start ar =
  let res = ref start in
    for i = ((Array.length ar) - 1) downto 0 do
      res := f !res i ar.(i)
    done;
    !res


let fold_left2 f start a b =
  let n = Array.length a in
    if ((Array.length b) <> n)
    then invalid_arg "fold_left2: different length arrays";
    let res = ref start in
      for i = 0 to n-1 do
	res := f !res a.(i) b.(i)
      done;
      !res


let iter2 f a b =
  let n = Array.length a in
    if ((Array.length b) <> n)
    then invalid_arg "iter2: different length arrays";
    for i = 0 to (n-1) do
      f a.(i) b.(i)
    done


let map2 f a b =
  if ((Array.length a) <> (Array.length b))
  then invalid_arg "map2: different length arrays";
  Array.mapi (fun i x ->
		let y = b.(i) in
		  f x y)
    a


let combine a b =
  (** as in List.combine, takes two arrays and returns array of pairs *)
  map2 (fun x y -> x,y) a b


let split a =
  (Array.map fst a), (Array.map snd a)


let flatten a =
  let lst =
    Array.fold_left (fun l a ->
		       (Array.fold_left (fun l e -> e :: l) [] a) @ l)
      [] a
  in Array.of_list (List.rev lst)


(** Given an arrays [domains] of where each entry is an array of the
    possible values for the given index, call [f] on all combinations of
    values from the given domains.

    for example:

    [for_all_combos f [| [| 1; 2 |]; [| 3; 4 |] |]] will call [f] on
    the following arguments:
    - [| 1; 3 |]
    - [| 1; 4 |]
    - [| 2; 3 |]
    - [| 2; 4 |]
*)
let for_all_combos f domains =
  if Array.length domains.(0) <> 0 then
    let n = Array.length domains in
    let init = domains.(0).(0) in
    let ary = Array.create n init in
    let rec combos ind =
      let vals = domains.(ind) in
	for i = 0 to Array.length vals - 1 do
	  ary.(ind) <- vals.(i);
	  if ind = n - 1 then
	    f ary
	  else
	    combos (ind + 1)
	done
    in
      combos 0

(********* tests **********)


let for_all f a =
  try
    Array.iter (fun x -> if (not (f x)) then raise Flag) a ;
    true
  with Flag -> false


let for_alli f a =
  try
    Array.iteri (fun i x -> if (not (f i x)) then raise Flag) a ;
    true
  with Flag -> false


let all_equal x a =
  for_all (fun e -> e = x) a


let all_within low high a =
  for_all (fun e -> (e >= low) && (e <= high)) a


let constant_p a =
  if (Array.length a) > 0 then
    for_all (fun x -> x = a.(0)) a
  else
    true


let exists f a =
  let n = Array.length a in
  let rec do_exists ind =
    if ind < n
    then f a.(ind) || do_exists (ind + 1)
    else false
  in do_exists 0


let existsi f a =
  not (for_alli (fun i x -> not (f i x)) a)


let count p a =
  Array.fold_left (fun count x ->
		     if (p x)
		     then count + 1
		     else count)
    0 a


let count2 p a b =
  let accum = ref 0 in
    iter2 (fun x y ->
	     if (p x y)
	     then incr accum)
      a b;
    !accum


let member x comparator start finish a =
  (** looks for [x] using [comparator] between [start] and [finish]
    (inclusive) in [a].  [x] is always passed first to [comparator] *)
  try
    for i = start to finish do
      if comparator x a.(i) then raise Flag
    done;
    false
  with Flag -> true


let mem x a =
  (** searches for [x] in [a] (using [=]) *)
  exists (fun y -> y = x) a


let filter p a =
  (** returns array of values from [a] that satisfy [p] *)
  let count = ref 0 in
  let b = Array.map (fun x ->
		       if (p x)
		       then (incr count; true)
		       else false)
	    a in
  let curr = ref (-1) in
    Array.init !count (fun _ ->
			 incr curr;
			 while not b.(!curr) do
			   incr curr
			 done;
			 a.(!curr))


let partition p a =
  (** [partition p a] returns two arrays, the first has all elements
      that satisfiy [p] and the second has the rest. *)
  let sat_count = ref 0 in
  let nosat_count = ref 0 in
  let b = Array.map (fun x ->
		       if (p x)
		       then (incr sat_count; true)
		       else (incr nosat_count; false))
    a in
  let sat_curr = ref (-1) in
  let nosat_curr = ref (-1) in
    (Array.init !sat_count (fun _ ->
			      incr sat_curr;
			      while not b.(!sat_curr) do
				incr sat_curr
			      done;
			      a.(!sat_curr)),
     Array.init !nosat_count (fun _ ->
				incr nosat_curr;
				while b.(!nosat_curr) do
				  incr nosat_curr
				done;
				a.(!nosat_curr)))


let filter_list p a =
  (** like [filter] but returns a list *)
  let accum = ref [] in
    Array.iter (fun x -> if p x then Wrutils.push x accum)
      a;
    List.rev !accum


(********** searching **********)


let find p a =
  (** returns the index of the left-most element of [a] that satisfies
    [p].  Can raise Not_found *)
  let al = Array.length a in
  let rec help i =
      if i >= al
      then raise Not_found
      else if (p a.(i)) then i
      else help (i + 1) in
    help 0


let rfind p a =
  (** returns the index of the right-most element of [a] that satisfies
    [p].  Can raise Not_found *)
  let i = ref ((Array.length a) - 1) in
    try
      while !i >= 0 do
	if (p a.(!i)) then raise Flag;
	decr i
      done;
      raise Not_found
    with Flag -> !i


let index a x =
  (** returns the index of the left-most occurance of [x] in [a].  Can
    raise Not_found *)
  find (fun y -> x = y) a


let nth_index a x n =
  (** returns the index of the n'th occerance of [x] in [a]. *)
  let found = ref n
  and ind = ref 0
  and max = (Array.length a) in
    while !found > 0 && !ind < max do
      (if a.(!ind) = x
       then found := !found - 1;
       if !found > 0 then ind := !ind + 1)
    done;
    if !found > 0 then raise Not_found
    else !ind


let max_by key a =
  (** returns element and score. Analagous to Wrlist.max_by.  works
    for ints or floats (hence probably slow) *)
  let l = Array.length a in
    if l > 0 then
      let max_e = ref a.(0) in
      let max = ref (key !max_e) in
	for i = 1 to l-1 do
	  let e = a.(i) in
	  let v = key e in
	    if v > !max then
	      (max := v;
	       max_e := e)
	done;
	!max_e, !max
    else
      invalid_arg "Wrarray.max_by: given empty array"


let min_by key a =
  (** returns element and score. Analagous to Wrlist.min_by.  works
    for ints or floats (hence probably slow) *)
  let l = Array.length a in
    if l > 0 then
      let min_e = ref a.(0) in
      let min = ref (key !min_e) in
	for i = 1 to l-1 do
	  let e = a.(i) in
	  let v = key e in
	    if v < !min then
	      (min := v;
	       min_e := e)
	done;
	!min_e, !min
    else
      invalid_arg "Wrarray.min_by: given empty array"


let max_index_by key a =
  (** returns the index of the leftmost highest scoring element.
    works for ints or floats (hence probably slow) *)
  let l = Array.length a in
    if l > 0 then
      let max_i = ref 0
      and max = ref (key a.(0)) in
	for i = 1 to l-1 do
	  let v = key a.(i) in
	    if v > !max then
	      (max := v;
	       max_i := i)
	done;
	!max_i
    else
      invalid_arg "Wrarray.max_index_by: given empty array"


let min_index_by key a =
  (** returns the index of the leftmost highest scoring element.
    works for ints or floats (hence probably slow) *)
  let l = Array.length a in
    if l > 0 then
      let min_i = ref 0
      and min = ref (key a.(0)) in
	for i = 1 to l-1 do
	  let v = key a.(i) in
	    if v < !min then
	      (min := v;
	       min_i := i)
	done;
	!min_i
    else
      invalid_arg "Wrarray.max_index_by: given empty array"



let min_and_max_by key a =
  (** returns elements scoring min and max according to [key] *)
  let l = Array.length a in
    if l > 0 then
      let min_e = ref a.(0)
      and max_e = ref a.(0) in
      let min = ref (key !min_e)
      and max = ref (key !max_e) in
	for i = 1 to l-1 do
	  let e = a.(i) in
	  let v = key e in
	    if v < !min then
	      (min := v;
	       min_e := e)
	    else if v > !max then
	      (max := v;
	       max_e := e)
	done;
	!min_e, !max_e
    else
      invalid_arg "Wrarray.min_and_max_by: given empty array"


let random_elt a =
  a.(Random.int (Array.length a))


let permute a =
  (** destructive alteration *)
  let len = Array.length a in
    for i = 0 to len - 2 do
      let j = i + (Random.int (len - i)) in
      let ai = a.(i) in
	a.(i) <- a.(j);
	a.(j) <- ai
    done


let permute_pair a b =
  (** destructive alteration *)
  let len = Array.length a in
    (assert ((Array.length b) = len));
    for i = 0 to len - 2 do
      let j = i + (Random.int (len - i)) in
      let ai = a.(i)
      and bi = b.(i) in
	a.(i) <- a.(j);
	a.(j) <- ai;
	b.(i) <- b.(j);
	b.(j) <- bi
    done


let sample size a =
  (** [sample size a] gets a random sample of size [size] from [a].

      This is a modified version of Reservoir sampling from Knuth's
      "The Art of Computer Programming" vol. 2. *)
  assert ((Array.length a) > size);
  assert ((Array.length a) > 0);
  let t = ref 0 in
  let sample = Array.make size a.(0) in
    Array.iter (fun e ->
		  if !t < size
		  then sample.(!t) <- e
		  else begin
		    let r = Random.int (!t + 1) in
		      if r < size then sample.(r) <- e
		  end;
		  incr t)
      a;
    sample

let lowres size a =
  assert (size > 0);
  let l = Array.length a in
    if size >= l then a
    else (let percent = l / size in
	    Array.init size (fun i -> a.(i * percent)))




(******* other ******)


let of_queue q =
  Array.init (Queue.length q)
    (fun _ -> Queue.take q)


let last a =
  let len = Array.length a in
    if (len < 1) then invalid_arg "last: array has no elements";
    a.(len - 1)


(************* modifying **************)


let copy_into src dest =
  let len = Array.length src in
    assert ((Array.length dest) >= len);
    Array.blit src 0 dest 0 len


let peel ?(n = 1) a =
  (** remove the first [n] (defaults to 1) elements from [a] *)
  Array.sub a n ((Array.length a) - n)


let fill_all a e =
  (** sets all elements of [a] to [e] *)
  Array.fill a 0 (Array.length a) e


let map_into dest f a =
  (** sets elements of [dest] to result of mapping [f] over [a] *)
  if (Array.length dest) <> (Array.length a) then
    invalid_arg "map_into: arrays differ in length";
  Array.iteri (fun i x ->
		 dest.(i) <- f x)
    a

let copy_except ary lst =
  (** [copy_except ary lst] returns a copy of [ary] except for each (ind, vl)
      tuple in [lst] the index is given the value. *)
  let n = Array.copy ary in
    List.iter (fun (ind, vl) -> n.(ind) <- vl;) lst;
    n


let extend a n init =
  (** extend [a] by [n] new places, which will be initialized to [init] *)
  let old_len = Array.length a in
    Array.init (old_len + n) (fun i ->
				if i >= old_len then init
				else a.(i))

let extend_ifun a n init =
  (** extend [a] by [n] new places, which will be initialized to [init] *)
  let old_len = Array.length a in
    Array.init (old_len + n) (fun i ->
				if i >= old_len then init i
				else a.(i))

let shift a i n amount =
  (** shifts the [n] items starting at [i] in [a] to the right by
    [amount].  doesn't handle wrap-around - the items must still fit. *)
  Array.blit a i a (i + amount) n


let swap a i j =
  let temp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- temp


let sort_on key a =
  (** calls [key] to get obj for sorting (via Pervasives.compare) *)
  Array.fast_sort (fun a b ->
		     compare (key a) (key b))
    a

let shuffle a =
  sort_on (fun x -> Random.bits ())
    a


(*
let invertible_sort key a =
  (** modifies [a] but returns the permutation used, where permute.(i) is
    the index in the result of the item originally at index i *)
  let indices = Array.init (Array.length a) Wrutils.identity in
    sort_on (fun x
*)

let rev a =
  (** destructively reverse order of elements in [a].  returns unit. *)
  let max = (Array.length a) - 1 in
    if max > 0 then
      for i = 0 to max / 2 do
	let temp = a.(i)
	and j = max - i in
	  a.(i) <- a.(j);
	  a.(j) <- temp
      done


let reverse a =
  (** returns a new array containing the elements of [a] in reverse
    order *)
  let l = Array.length a in
  let max = l - 1 in
    Array.init l (fun i -> a.(max - i))


let dups_p key a =
  (** true iff there are duplicates (by = on results of [key]) in [a] *)
  let c = Dset.create 64 in
  let rec examine i =
    if i < 0 then
      false
    else
      if (not (Dset.add_new_p (key a.(i)) c)) then
	true
      else
	examine (i-1)
  in
    examine ((Array.length a)-1)


let duplicates_p a = dups_p Fn.identity a


let remove_dups key a =
  (** returns fresh array without any duplicates (by = on results of
    [key]).  order is not preserved *)
  let t = Hashtbl.create 64 in
    Array.iter (fun x -> Hashtbl.replace t (key x) x) a;
    let b = Array.make (Hashtbl.length t) a.(0)
    and i = ref 0 in
      Hashtbl.iter (fun _ v ->
		      b.(!i) <- v;
		      incr i)
	t;
      b


let remove_dups_if eq_test a =
  (** retains the leftmost duplicate.  always returns a fresh array *)
  let a2 = Array.copy a
  and fp = ref 0
  and l = Array.length a in
    for i = 0 to l - 1 do
      let x = a.(i) in
	if not (member x eq_test 0 (!fp-1) a2) then
	  (a2.(!fp) <- x;
	   incr fp)
    done;
    if !fp <> l then
      Array.sub a2 0 !fp
    else
      a2


let remove_doublet a target =
  (** finds [target] in [a], returning the next element and the array
    without either of them.  Can raise Not_found.  useful for command line
    argument processing. *)
  let i = index a target
  and max = (Array.length a) - 1 in
    if i < max then
      let res = a.(i+1) in
      let b = Array.sub a 0 (max-1) in
	Array.blit a (i+2) b i ((max - i) - 1);
	res, b
    else
      failwith "Wrarray.remove_doublet: no element after target"


let remove_doublet_or_default a target default =
  try
    remove_doublet a target
  with Not_found -> default, a


(************* I/O *************)


let fprint_array outch key interposed ar =
  (** prints {ar] on [outch], printing [interposed] between the
      elements.  [key] returns the string to print for each element *)
  Array.iteri (fun i x ->
		 if (i != 0)
		 then output_string outch interposed ;
		 output_string outch (key x))
    ar



(************************ arrays of ints ********************)


let sum_using key a =
  Array.fold_left (fun a x -> a + (key x)) 0 a


let sum a =
  let sum = ref 0 in
    for i = 0 to (Array.length a)-1 do
      sum := !sum + a.(i)
    done;
    !sum


let add a b =
  map2 (+) a b

let subtract a b =
  map2 (-) a b


let ints_equal a b =
  (** [ints_equal a b] tests if two integer arrays are equal.  I think
      that this is faster than polymorphic comparison. *)
  let na = Array.length a and nb = Array.length b in
    if na <> nb
    then false
    else
      let eq = ref true in
      let i = ref 0 in
	while (!i < na) && !eq do
	  if a.(!i) <> b.(!i) then eq := false;
	  incr i
	done;
	!eq


let char_equal a b =
  (** [ints_equal a b] tests if two integer arrays are equal.  I think
      that this is faster than polymorphic comparison. *)
  let na = Array.length a and nb = Array.length b in
    if na <> nb
    then false
    else
      let eq = ref true in
      let i = ref 0 in
	while (!i < na) && !eq do
	  if Char.compare a.(!i) b.(!i) != 0
	  then eq := false;
	  incr i
	done;
	!eq



      (*** see Vector for arrays of floats ***)


(******* I/O ******)


let read_ints ic n =
  (** returns an array of [n] ints *)
  Array.init n (fun i -> Wrio.input_int ic)


let write_ints oc a =
  (** on current line, separated by spaces *)
  fprint_array oc string_of_int " " a


let write_ints_line oc a =
  (** on current line, separated by spaces *)
  fprint_array oc string_of_int " " a;
  Wrutils.newline oc


let write_ints_nl oc a =
  (** one per line *)
  fprint_array oc string_of_int "\n" a;
  Wrutils.newline oc


let print_array_index oc a =
  (**prints a string array with labels for each index.*)
  Array.iteri
    (
      fun index item ->
	Printf.fprintf oc "index %d:%s\n" index item
    )
    a



(* EOF *)
