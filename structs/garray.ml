(** Arrays that grow as necessary.

    @author Ethan Burns
    @since Thu Oct 22 13:34:02 EDT 2009
*)


type 'a t = {
  mutable data : 'a array;
  mutable fill : int;
(*
  mutable init_fun : int -> 'a;
*)
  init_size : int;
}


let current_size ga = 
  Array.length ga.data


let init ?(init_size=100) f =
  (** [init ?init_size f] creates a new growing array calling f on
      each element. *)
  if init_size < 1
  then invalid_arg "Garray.init: must have a size greater than zero"
  else
    let ary = Array.init init_size f in
      {
	data = ary;
	fill = 0;
(*
	init_fun = f;
*)
	init_size = init_size;
      }


(*
let set_init t f = t.init_fun <- f
  (** [set_init t f] set the initialization function. *)
*)


let make ?(init_size=100) init_elm =
  (** [make ?init_size init_elm] creates a new growing array. *)
  init ~init_size:init_size (fun _ -> init_elm)


let copy t =
  (** [copy t] makes a copy of the garray. *)
  { t with data = Array.sub t.data 0 t.fill }


let rec ensure_size init_fun t ind =
  (** [ensure_size init_fun t ind] ensure that the array is large
      enough to hold index [ind]. *)
  if ind >= (Array.length t.data)
  then begin
    let new_size = (Array.length t.data) * 2 in
    let new_ary = Array.init new_size (fun i ->
					 if i < t.fill
					 then t.data.(i)
					 else match init_fun with
					   | Some f -> f i
					   | None -> t.data.(0))
    in
      Verb.pr Verb.debug "Garray: growing to %d elements\n%!" new_size;
      t.data <- new_ary;
      ensure_size init_fun t ind
  end


let clear ?init_fun t =
  (** [clear ?init_fun t] removes all elements from [t] and resets the array
      to its initial size. *)
  let ary = match init_fun with
    | Some f -> Array.init t.init_size f
    | None -> Array.create t.init_size t.data.(0)
  in
    t.data <- ary;
    t.fill <- 0


let clear_range ?init_fun t start len =
  (** [clear_range ?init_fun t start len] clears a length of the array
      from index [start] for a length of [len] elements. *)
  if start >= t.fill
  then
    invalid_arg (Wrutils.str
		   "Garray.clear_range: start (%d) greater than fill (%d)"
		   start t.fill)
  else begin
    let finish = Math.imin (start + (len - 1)) (t.fill - 1) in
      for i = start to finish do
	t.data.(i) <- (match init_fun with
			 | Some f -> f i
			 | None -> t.data.(0))
      done
  end



let insert t ?init_fun elm =
  (** [insert t ?init_fun elm] insert an element at the end of the
      array. *)
  ensure_size init_fun t t.fill;
  t.data.(t.fill) <- elm;
  t.fill <- t.fill + 1


let set t ?init_fun ind vl =
  (** [set t ?init_fun ind val] sets index [ind] to value [vl]. *)
  ensure_size init_fun t ind;
  t.data.(ind) <- vl;
  t.fill <- max t.fill (ind + 1)


let get t ?init_fun ind =
  (** [get t ?init_fun ind] gets the value at index [ind].  If the array is
      not this big then it is grown to an appropriate size. *)
  ensure_size init_fun t ind;
  t.fill <- max t.fill (ind + 1);
  t.data.(ind)


let nth t ?init_fun n = get t ?init_fun n
  (** [nth t ?init_fun n] gets the nth element of the array. *)


let get_fill t = t.fill
  (** [get_fill t] gets the number of meaningful elements in the
      array. *)


let get_data t = t.data
  (** [get_data t] gets the array backing this structure. *)


let iter f t =
  (** [iter f t] calls [f] on each element of [t] that is filled
      in. *)
  for i = 0 to t.fill - 1 do f t.data.(i) done


let iteri f t =
  (** [iteri f t] calls [f] on the index and elements of [t] that are
      filled in. *)
  for i = 0 to t.fill - 1 do f i t.data.(i) done


let iteri_rev f t =
  (** [iteri_rev f t] calls [f] on the index and elements of [t] that
      are filled in, in reverse order. *)
  for i = t.fill - 1 downto 0 do f i t.data.(i) done


let to_list t =
  (** [to_list t] builds a list of the elements of the array. *)
  let lst = ref [] in
    for i = t.fill - 1 downto 0 do lst := t.data.(i) :: !lst done;
    !lst

let fold_left f init t =
  (** [fold_left f init t] folds [f] over [t] starting with [init]. *)
  let accum = ref init in
    for i = 0 to t.fill - 1 do accum := f !accum t.data.(i) done;
    !accum


module Double_ended = struct
  type 'a vec = 'a t

  type 'a t = {
    negative : 'a vec;
    non_neg : 'a vec;
    init_fun : int -> 'a;
  }

  let init ?(init_size=128) f =
    let s = init_size / 2 in
    { negative = init ~init_size:s f;
      non_neg = init ~init_size:s f;
      init_fun = f;
    }


  let make ?init_size init_elm =
    init ?init_size (fun _ -> init_elm)


  let get v i =
    if i < 0 then
      get ~init_fun:v.init_fun v.negative ((abs i) - 1)
    else
      get ~init_fun:v.init_fun v.non_neg i


  let set v i e =
    if i < 0 then
      set v.negative ~init_fun:v.init_fun ((abs i) - 1) e
    else
      set v.non_neg ~init_fun:v.init_fun i e


  let iteri f v =
    iteri_rev (fun i v -> f ~-(i + 1) v) v.negative;
    iteri f v.non_neg

end
