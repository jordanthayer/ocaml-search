(** Records a structure *)

type 'a entry = {
  step : int;
  keys : 'a array;
}

type 'a t = {
  structure : (int, 'a entry) Hashtbl.t;
  mutable sequence : int list;
}

(***************************************************************************)
let new_struct () =
  { structure = Hashtbl.create 50;
    sequence = [];}


let add_step_raw t step keys =
  Hashtbl.add t.structure step {step = step; keys = keys};
  t.sequence <- step::t.sequence


let add_step t entry =
  Hashtbl.add t.structure entry.step entry;
  t.sequence <- entry.step::t.sequence


let reverse_sequence t =
  t.sequence <- (List.rev t.sequence)


let get_entry t step =
  try
    Hashtbl.find t.structure step
  with Not_found -> { step = step; keys = [||]}


let get_sequence t =
  t.sequence


let get_nth_struct t n =
  Hashtbl.find t.structure (List.nth t.sequence n)


let entry_mem ent key =
  Array.fold_left (fun accum k -> accum || k = key) false ent.keys


let mem t key =
  let found = ref false
  and steps = Array.of_list t.sequence in
    Array.iter
      (fun step ->
	  if not !found
	  then (let str = Hashtbl.find t.structure step in
		  found := entry_mem str key)) steps;
    !found

(* EOF *)
