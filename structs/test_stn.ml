(** Add some constraints to the STNs to see which is faster.  In both
    cases we 'copy' the STN after each constraint.

    @author eaburns
    @since 2010-03-07
*)


let nconstrnts = 5_000

let nnodes = 5_000

let rand_constraint rnd () =
  (** [rand_constraint rnd ()] gets a random constraint. *)
  let i = rnd nnodes in
  let j =
    let t = ref (rnd nnodes) in
      while !t = i do t := rnd nnodes done;
      !t
  in
  let a = rnd nnodes in
    (i, j, a, (rnd nnodes) + a + 1)



let rnd seed =
  (** [rnd seed] gets a seeded random integer function. *)
  Random.State.int (Random.State.make [| seed |])

let rec p_stn cs i num incons stn =
  if num = 0
  then stn, incons
  else
    let stn, incons =
      try
	let stn' = P_stn.add_constraint stn cs.(i) in
	  stn', incons
      with Simple_temp_net.Inconsistent ->
	stn, incons + 1
    in p_stn cs (i + 1) (num - 1) incons stn


let rec parc_stn cs i num incons ostn =
  if num = 0
  then ostn, incons
  else
    let nodea, nodeb, vla, vlb = cs.(i) in
    let ostn, incons =
      try
	let stn' = Old_stn.copy ostn in
	  Old_stn.add_constraint stn'
	    (Old_stn.range_constraint nodea nodeb vla vlb);
	  stn', incons
      with Old_stn.Inconsistent -> ostn, incons + 1
    in parc_stn cs (i + 1) (num - 1) incons ostn


let rec d_stn cs i num incons dstn =
  if num = 0
  then dstn, incons
  else
    let dstn = D_stn.copy dstn in
    let dstn, incons =
      try D_stn.add_constraint dstn cs.(i), incons
      with Simple_temp_net.Inconsistent -> dstn, incons + 1
    in d_stn cs (i + 1) (num - 1) incons dstn


let main () =
  let seed = truncate (Unix.gettimeofday ()) in
  let r = rand_constraint (rnd seed) in
  let cs = Array.init nconstrnts (fun _ -> r ()) in
  let pstn = P_stn.create nnodes in
  let dstn = D_stn.create nnodes in
  let ostn = Old_stn.create () in
  let _ =
    for i = 1 to nnodes do
      if i <> (Old_stn.add_point ostn) then failwith "Bad i"
    done
  in
  let _ = Gc.compact () in
  let (pstn', pincons), ptime =
    Wrsys.with_time (fun () -> p_stn cs 0 nconstrnts 0 pstn)
  in
  let _ = Gc.compact () in
  let (dstn', dincons), dtime =
    Wrsys.with_time (fun () -> d_stn cs 0 nconstrnts 0 dstn)
  in
  let _ = Gc.compact () in
  let (ostn', oincons), otime =
    Wrsys.with_time (fun () -> parc_stn cs 0 nconstrnts 0 ostn)
  in
    Printf.printf "dincons = %d, oincons = %d, pincons = %d\n%!"
      dincons oincons pincons;
    for n = 0 to nnodes - 1 do
      let dl, du = D_stn.bounds dstn' n in
      let pl, pu = P_stn.bounds pstn' n in
      let ol = Old_stn.earliest ostn' n in
      let ou = Old_stn.latest ostn' n in
	begin try
	  assert (ol = dl);
	  assert (ou = du);
	with e ->
	  Printf.printf "n=%d, dl=%d, du=%d, ol=%d, ou=%d\n" n dl du ol ou;
	  raise e
	end;
	begin try
	  assert (pl = dl);
	  assert (pu = du);
	with e ->
	  Printf.printf "n=%d, dl=%d, du=%d, pl=%d, pu=%d\n" n dl du pl pu;
	  raise e
	end;
    done;
    Printf.printf "dtime = %f, otime = %f, ptime = %g\n%!" dtime otime ptime

let _ = main ()

