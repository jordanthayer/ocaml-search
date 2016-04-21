(*
  relaxed (temporal) planning graph. Do not use action duration when
  propagate right now. Estimate the number of actions (cost) needed to
  achieve each fact using sum propagation
*)

open Tpl_ground


type t = {
  acost : int array;
  fcost : int array;

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


let start_time, get_time =
  let t = ref 0. in
    (fun () -> t := Unix.gettimeofday ()),
    (fun () -> (Unix.gettimeofday ()) -. !t)

(*****  ***)


let activated pg act =
  pg.usc.(act.id) = 0


let reduce_usc pg act =
  pg.usc.(act.id) <- pg.usc.(act.id) - 1;
  if pg.usc.(act.id) < 0 then
    failwith "rtpg.reduce_usc: usc < 0"
  else pg.usc.(act.id)


let make_graph p  =
  let nA = List.length p.actions in
  let usc = Array.make nA 0 in
    List.iter (fun a -> usc.(a.id) <- List.length a.pre) p.actions;
    { acost = Array.make (nA+1) max_int;
      fcost = Array.make ((List.length p.atoms) +1) max_int;
      to_check = p.supportees;
      achievers = p.establishers;
      usc = usc;}



(********** sum cost propagation ***********)

let init_update, add_update, pop_update, has_update =
  (** update = (f,new-value) *)
  let q = Queue.create () in
    (fun atoms ->
       List.iter (fun f -> Queue.push (f,0) q) atoms),
    (fun item -> Queue.push item q),
    (fun () -> Queue.pop q),
    (fun () -> not (Queue.is_empty q))


let init_cost pg a =
  (** called when all preconds of act are first achieved *)
  pg.acost.(a.id) <- List.fold_left (fun sum f -> sum + pg.fcost.(f)) 0 a.pre


let update_cost pg a decr =
  (** update the cost of a when the cost to achieve one of it pre reduces *)
  pg.acost.(a.id) <- pg.acost.(a.id) - decr


let update pg (f,v) =
  let update_act act =
    (* when acost(act) reduces; possibly update cost of its effects *)
    List.iter (fun f ->
		 if pg.fcost.(f) > pg.acost.(act.id) + 1 then
		   add_update (f,pg.acost.(act.id)+1))
      act.add
  in
    if pg.fcost.(f) = max_int then
      ( pg.fcost.(f) <- v;
	List.iter (fun act ->
		     if (reduce_usc pg act) = 0 then
		       (init_cost pg act;
			update_act act))
	  (pg.to_check f))
    else
      if pg.fcost.(f) <= v then ()
      else
	( List.iter (fun act ->
		       if (activated pg act) then
			 (update_cost pg act (pg.fcost.(f) - v);
			  update_act act))
	    (pg.to_check f);
	  pg.fcost.(f) <- v)


let sum_propagate pg init =
  init_update init;
  while (has_update ()) do
    update pg (pop_update ())
  done



(*********** interface for needing "short" relaxed plan ************)

let easiest_act pg =
  (** easiest = closest = least steps *)
  (fun acts -> fst (Wrlist.min_by (fun a -> pg.acost.(a.id)) acts))


let cheap_fact pg =
  (fun (f1,_) (f2,_) -> pg.fcost.(f1) <= pg.fcost.(f2))


let closest p =
  (** the relaxed plan is extracted using both [current] and
    preconditions of in_progress actions; they are _time-stamped_ from
    the current state **)
  start_time ();
  let pg = make_graph p in
    sum_propagate pg (List.filter p.in_initial p.atoms);
    Verb.pe 3 "Cost propagation in: %f secs\n\n" (get_time ());
    flush_all ();
    cheap_fact pg, easiest_act pg


(* EOF *)



