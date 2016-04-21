(** Basic default timing mechanisms for heuristic searches where nodes
    need to have values intermittently recalculated.  When the timers
    return true, it is time for the values to be recalculated and the
    queues will need to be resorted then. *)


let reckless =
  (** Never resort. *)
  (fun () -> false)

let conservative =
  (** always resort *)
  (fun () -> true)


let fixed_durration d =
  (** Resort every [d] steps. *)
  let i = ref 0 in
    (fun () ->
       if i > d
       then (i := 0;
	     true)
       else (i := !i + 1;
	     false))


let geometric d fact =
  (** Resorts on a geometrically growing scale *)
  let i = ref 1.
  and next = ref d in
    (fun () ->
       if !i > !next
       then (next := !next *. fact;
	     true)
       else (i := !i +. 1.;
	     false))


let random ?(seed = 314159) p =
  (** Randomly resorts with probability [p] *)
  Random.init seed;
  (fun () ->
     (Random.float 1.) < p)


let one_shot d =
  (** Resorts once at the [d]th step of the algorithm, and then never again *)
  let i = ref 0 in
    (fun () ->
       i := !i + 1;
       !i = d)


(* EOF *)
