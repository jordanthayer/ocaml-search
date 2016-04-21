(** Get a fixed size uniform sample from a stream of records.

    This is from Knuth's Art of Computer Programming Vol. 2 it is
    called the reservoire method.

    @author eaburns
    @since 2010-01-13
*)


let make_stream_sampler n =
  (** [stream_sampler n] make a sample selector that will select a
      sample of size [n] from a stream of records. *)
  let t = ref 0
  and sample = ref None in
  let insert i r =
    (* Insert a record [r] into the sample at index [i]. *)
    match !sample with
      | None -> sample := Some (Array.make n r)
      | Some a -> a.(i) <- r
  in
  let consider ?(copy=Fn.identity) r =
    (* Consider the next record. *)
    incr t;
    if (!t - 1) < n
    then insert (!t - 1) (copy r)
    else
      let rnd = Random.int !t in
	if rnd < n then insert rnd (copy r)
  and get_sample () =
    (* Get the sample *)
    match !sample with
      | None -> [| |]
      | Some a when !t < n -> Array.sub a 0 !t
      | Some a -> a
  and reset () =
    t := 0;
    sample := None;
  in consider, get_sample, reset



let make_stream_sampler_prime n =
  (** [stream_sampler n] make a sample selector that will select a
      sample of size [n] from a stream of records. *)
  let t = ref 0
  and sample = ref None
  and last_rand = ref 0 in
  let insert i r =
    (* Insert a record [r] into the sample at index [i]. *)
    match !sample with
      | None -> sample := Some (Array.make n r)
      | Some a -> a.(i) <- r
  in
  let consider () =
    (* Consider the next record. *)
    if (!t - 1) < n
    then true
    else let rnd = Random.int !t in
      if rnd < n then (last_rand := rnd; true) else false in
  let pub_insert ?(copy=Fn.identity) r =
    (* Consider the next record. *)
    incr t;
    if (!t - 1) < n
    then insert (!t - 1) (copy r)
    else insert !last_rand (copy r)
  and get_sample () =
    (* Get the sample *)
    match !sample with
      | None -> [| |]
      | Some a when !t < n -> Array.sub a 0 !t
      | Some a -> a
  and reset () =
    t := 0;
    sample := None;
  in consider, pub_insert,get_sample, reset
