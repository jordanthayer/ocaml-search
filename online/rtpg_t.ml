(*
  very similar to rtpg_c.ml; instead of propagate cost, we propagate time
*)

open Ground


type t = {
  atime : float array;
  ftime : float array;

  to_check : ground_atom -> ground_action list;
  achievers : ground_atom -> ground_action list;

  (* unsupported (pre)conditions *)
  usc : int array;
}


(***** debug functions *****)

let print_state current in_progress =
  (** debug: print the state that needs heuristic evaluation *)
  Wrutils.pe " Current: (";
  List.iter (fun f -> Wrutils.pe " %s " (atom_str f)) current;
  Wrutils.pe ")";
  Wrutils.pe " In-Progress: (";
  List.iter (fun (a,d) -> Wrutils.pe "(%s,%f) " a.printout d) in_progress;
  Wrutils.pe ")";
  flush_all ()


let print_test g a rp =
  Wrutils.pe "g = %s; a = %s; rp = " (atom_str g) a.printout;
  List.iter (fun (a,t) -> Wrutils.pe "(%s,%f) " a.printout t) rp;
  Wrutils.pe "\n";
  flush_all ()

(****  ****)


let activated pg act =
  pg.usc.(act.id) = 0


let reduce_usc pg act =
  pg.usc.(act.id) <- pg.usc.(act.id) - 1;
  if pg.usc.(act.id) < 0 then
    failwith "rtpg_t.reduce_usc: usc < 0"
  else pg.usc.(act.id)


let make_graph actions nF achievers to_check =
  let nA = List.length actions in
  let usc = Array.make nA 0 in
    List.iter (fun a ->
		 usc.(a.id) <- List.length a.pre) actions;
    { atime = Array.make (nA+1) infinity;
      ftime = Array.make (nF+1) infinity;
      to_check = to_check;
      achievers = achievers;
      usc = usc;},
    (fun pg ->
       for i = 0 to nA do
	 pg.atime.(i) <- infinity
       done;
       for j = 0 to nF do
	 pg.ftime.(j) <- infinity
       done;
       List.iter (fun a ->
		    usc.(a.id) <- List.length a.pre)
	 actions)


(********** relax time propagation (no mutex) ***********)

let init_update, add_update, pop_update, has_update, clear_update =
  (** update = (f,new-value) *)
  let dummy_event = (-999., -1)
  and event_earlier (t1,_) (t2,_) = (t1 <= t2) in
  let q = Dpq.create_with event_earlier dummy_event in
    (fun (current,in_prog) ->
       (* [in_prog] is empty for regression planner *)
       List.iter (fun f -> Dpq.insert q (0.,f)) current;
       List.iter (fun (a,d) -> List.iter (fun f -> Dpq.insert q (d,f)) a.add)
	 in_prog),
    (fun (t,f) -> Dpq.insert q (t,f)),
    (fun () -> Dpq.extract_first q),
    (fun () -> not (Dpq.empty_p q)),
    (fun () -> Dpq.clear q)


(*** for regression planner ***)

let update_fp pg (t,f) =
  (** [f] is added by some action at [t] *)
  if not (pg.ftime.(f) = infinity) then ()
  else
    ( pg.ftime.(f) <- t;
      List.iter (fun a ->
		   if (reduce_usc pg a) = 0 then
		     List.iter (fun f -> add_update ((t +. a.dur),f)) a.add)
	(pg.to_check f))


let build_rtpg_fp pg init =
  (** expand the graph until fix-point *)
  init_update init;
  while (has_update ()) do
    update_fp pg (pop_update ())
  done


(*** for progression planner ***)

let update pg (t,f) in_goal ng =
  (** [f] is added by some action at [t] *)
  if not (pg.ftime.(f) = infinity) then ()
  else
    ( pg.ftime.(f) <- t;
      List.iter (fun a ->
		   if (reduce_usc pg a) = 0 then
		     List.iter (fun f -> add_update ((t +. a.dur),f)) a.add)
	(pg.to_check f);
      if (in_goal f) then decr ng)


let build_rtpg pg init in_goal ng =
  (** expand the graph until all goals are reached *)
  clear_update ();
  init_update init;
  let rec prop () =
    if not (has_update ()) then infinity
    else
      let (t,f) = pop_update () in
	update pg (t,f) in_goal ng;
	if !ng = 0 then t
	else prop ()
  in prop ()



(***** relaxed plan extraction/counting the steps ******)



(*********** makespan estimation ************)

let h1_reg p =
  (** h1 (temporal pg with no mutex) for regression planner **)
  let init = List.filter p.in_initial p.atoms in
  let nf = List.length p.atoms in
  let pg,_ = make_graph p.actions nf p.establishers p.supportees in
    build_rtpg_fp pg (init,[]);
    (fun goals ->
       List.fold_left (fun t g -> Math.fmax pg.ftime.(g) t) 0. goals)


let h1_pro p goals =
  (** h1 for progression planner *)
  let ng = List.length goals in
  let map_goal = Hashtbl.create ng in
  let in_goal g = Hashtbl.mem map_goal g in
  let nf = List.length p.atoms in
  let pg,reset_pg = make_graph p.actions nf p.establishers p.supportees in
    List.iter (fun g -> Hashtbl.add map_goal g ()) goals;
    (fun current in_prog ->
       reset_pg pg;
       build_rtpg pg (current,in_prog) in_goal (ref ng))


(* EOF *)
