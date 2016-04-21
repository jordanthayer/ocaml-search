(** A packed array of ints.  Each element is stored as a subset of the
    total number of bits in an int.  Elements are not optimally
    packed.  This means that if they don't fit evenly in an int there
    may be unused, leftover bits in each int.

    The reason for the [functions] interface is so that all of the
    overhead information (bits per element, elements per int, etc.) is
    stored in closures and not tagging along with each array.  This
    reduces the amount of memory used for each individual array.

    @author eaburns
    @since 2010-08-31
*)

type t = int array

type info = {
  (* Information on a set of packed ints with a specific element
     size. *)
  elm_max : int;
  mask : int;
  epi : int;
  bpe : int;
}

type create_fun = int -> t

type get_fun = t -> int -> int

type set_fun = t -> int -> int -> unit

let bits_per_int = Sys.word_size - 1


let bits_per_elm elm_max =
  (** [bits_per_elm elm_max] gets the minimum number of bits for each
      element. *)
  truncate (ceil (Math.log2 (float (elm_max + 1))))


let elms_per_int bpe =
  (** [elms_per_int bpe] gets the number of elements in an int. *)
  bits_per_int / bpe


let number_of_ints ~epi ~nelms =
  (** [number_of_ints ~epi ~nelms] gets the number of ints required
      for [nelms] when [epi] elemens fit in an int. *)
  let residue = nelms mod epi in
    nelms / epi + (if residue > 0 then 1 else 0)


(** {1 Functions} ****************************************)

let info elm_max =
  (** [info elm_max] makes an info record for packed ints with a given
      size element. *)
  let bpe = bits_per_elm elm_max in
  let epi = elms_per_int bpe in
  let mask = lnot ((lnot 0) lsl bpe) in
    Verb.pr Verb.debug "max element value: %d\n" elm_max;
    Verb.pr Verb.debug "bits per element: %d\n" bpe;
    Verb.pr Verb.debug "element per int: %d\n" epi;
    Verb.pr Verb.debug "mask: %0*x\n" (Sys.word_size / 4) mask;
    { elm_max = elm_max; mask = mask; epi = epi; bpe = bpe }



let get ~epi ~bpe ~mask ary i =
  let ind = i / epi and r = i mod epi in
    (ary.(ind) lsr (bpe * r)) land mask



let make_get info =
  let epi = info.epi and bpe = info.bpe and mask = info.mask in
    (fun ary i -> get ~epi ~bpe ~mask ary i)



let set ~epi ~bpe ~mask ary i vl =
  let ind = i / epi and r = i mod epi in
  let shift = bpe * r in
  let vl' = (vl land mask) lsl shift in
    (* Verb.pf Verb.always stdout "mask: %d shift: %d\n" mask shift; *)
    (* flush stdout; *)
  let clr = lnot (mask lsl shift) in	(* clear dest bits *)
    ary.(ind) <- (ary.(ind) land clr) lor vl'


let make_set info =
  let epi = info.epi and bpe = info.bpe and mask = info.mask in
    (fun ary i vl -> set ~epi ~bpe ~mask ary i vl)


let create ~epi ~nelms =
  let nints = number_of_ints ~epi ~nelms in
    Array.create nints 0


let make_create info =
  let epi = info.epi in
    (fun nelms -> create ~epi ~nelms)


let copy = Array.copy


let make_mapi info =
  let create = make_create info in
  let set = make_set info in
  let get = make_get info in
    (fun nelms f ary ->
       let c = create nelms in
	 for i = 0 to nelms - 1 do set c i (f i (get ary i)) done;
	 c)


let make_map info =
  let create = make_create info in
  let set = make_set info in
  let get = make_get info in
    (fun nelms f ary ->
       let c = create nelms in
	 for i = 0 to nelms - 1 do set c i (f (get ary i)) done;
	 c)


let make_fold_left info =
  let get = make_get info in
    (fun nelms f init ary ->
       let rec do_fold get nelms f ary vl i =
	 if i < nelms
	 then do_fold get nelms f ary (f vl (get ary i)) (i + 1)
	 else vl
       in do_fold get nelms f ary init 0)


let make_fold_lefti info =
  let get = make_get info in
    (fun nelms f init ary ->
       let rec do_fold get nelms f ary vl i =
	 if i < nelms
	 then do_fold get nelms f ary (f vl i (get ary i)) (i + 1)
	 else vl
       in do_fold get nelms f ary init 0)


let make_iter info =
  let get = make_get info in
    (fun nelms f ary -> for i = 0 to nelms - 1 do f (get ary i) done)


let make_iteri info =
  let get = make_get info in
    (fun nelms f ary ->
       for i = 0 to nelms - 1
       do
	 f i (get ary i)
       done)


let unpack paa max_val = 
  (**if paa is a packed int array, with value max_val, turns it into
     a normal array of size max_val.  Useful for unpacking sliding
     tile arrays, but may be suitable for other thigns as well.*)
  let i = info max_val in
  let getter = make_get i in
    Array.init max_val (fun ix -> getter paa ix);;
