(** A sliding window is an array with indexes that don't start at
    zero.  It is dynamically sizing but can have its size fixed if
    desired.

    Since we may want to marshal this structure to a channel, we make
    sure that it doesn't hold onto any functions (hence requiring the
    init_fun parameter all over the place).

    @author eaburns
    @since 2009-12-17
*)

module type InitializerType =
sig
  type t
  val init : int -> t
end

module Make(Init : InitializerType) =
struct

  type 'a t = {
    mutable min_num : int;
    mutable max_size : int;
    mutable data : 'a Garray.t;
  }

  let init_fun = Init.init

  let ind_of_num t n = n - t.min_num
    (** [ind_of_num t n] gets the index for a sequence number. *)


  let num_of_ind t ind = ind + t.min_num
    (** [num_of_ind t ind] gets the seq number from an index. *)


  let ind_of_num_bound from t n =
    (** [ind_of_num_bound from t n] errors if [n] is out of the
	window otherwise the index is given. *)
    let ind = ind_of_num t n in
      if t.max_size >= 0 && ind >= t.max_size
      then invalid_arg (Wrutils.str
			  "Sliding_window.%s: %d is above the window (max=%d)"
			  from n (num_of_ind t (t.max_size - 1)))
      else
	if ind < 0
	then invalid_arg (Wrutils.str
			    "Sliding_window.%s: %d is below the window (min=%d)"
			    from n t.min_num)
	else ind


  let init min_num max_size =
    (** [init ?init_size min_num max_size] initializes a new sliding
	window where each element is initialized by calling the
	initialization function.  If [max_size] is less than one then
	the absolute value of it is used as the inital size. *)
    let init_size = if max_size >= 0 then max_size else ~-max_size in
    let t = {
      min_num = min_num;
      max_size = max_size;
      data =
	Garray.init ~init_size:init_size (fun i -> init_fun (min_num + i)); }
    in t


  let reset sw min_num =
    (** [reset sw min_num] resets the sliding window [min_num]
	is the new minimum number to use. *)
    sw.min_num <- min_num;
    Garray.clear ~init_fun sw.data


  let get t n =
    (** [get t i] gets the data at sequence number [n]. *)
    let ind = ind_of_num_bound "get" t n in Garray.get t.data ~init_fun ind


  let set t n v =
    (** [set t i] sets the data at sequence number [n] to [v]. *)
    let ind = ind_of_num_bound "set" t n in Garray.set t.data ~init_fun ind v


  let slide t delta =
    (** [slide t delta] slides the window so that the new
	minimum sequence number is the old plus [delta].  Elements that
	were slid out side of the window are lost forever.  *)
    if delta <> 0
    then begin
      if (t.max_size > 0 && (abs delta) > t.max_size)
      then
	(* The window was slid clear. *)
	Garray.clear t.data ~init_fun
      else begin
	let min_num' = t.min_num + delta in
	let min_ind' = ind_of_num t min_num' in
	let fill = Garray.get_fill t.data in
	  if delta < 0
	  then begin
	    (* slide window left. *)
	    for i = fill - 1 downto Math.imax min_ind' 0 do
	      if t.max_size < 0 || i - delta < t.max_size
	      then (Garray.set t.data ~init_fun (i - delta)
		      (Garray.nth t.data ~init_fun i))
	    done;
	    try
	      Garray.clear_range t.data ~init_fun 0 (abs delta);
	    with _ -> assert false;
	  end else begin
	    (* slide window right. *)
	    if fill > delta
	    then begin
	      for i = delta to fill - 1 do
		Garray.set t.data ~init_fun (i - delta)
		  (Garray.nth t.data ~init_fun i)
	      done;
	      Garray.clear_range t.data ~init_fun (fill - delta) delta
	    end else Garray.clear t.data ~init_fun
	  end;
	  t.min_num <- min_num';
      end
    end


  let get_max_used t = num_of_ind t ((Garray.get_fill t.data) - 1)
    (** [get_max_used t] gets the maximum sequence number that is in
	use. *)
end
