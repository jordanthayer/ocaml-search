(* Coloring for various displays *)


type color =
  | Scaler of float
  | Fixed of (int * int * int)



let min_color = 0.
and max_color = 65535. *. 3.
let range = max_color -. min_color


let scale num min max =
  (** Selects an rgb value for num given the number, a minimum and a maximum.
      Gives a consistent color scaling for all possible values *)
  assert (Math.finite_p min);
  assert (Math.finite_p max);
  assert (max <> min);
  match num with
      Scaler num ->
	(let num =
	   if num > max then max else if num < min then min else num in
	 let num = (((num -. min) /. (max -. min)) *. range) in
	   (let v1 = int_of_float num in
	      Verb.pe Verb.debug "v: %i\n" v1;
	      (* from 125 0 0 to 255 255 125 *)
	      if v1 <= 65535
	      then (65535, 65535, (65535 - v1))
	      else
		(if v1 <= 131070
		 then (65535, (65535 - (v1 - 65535)), 0)
		 else ((65535 - (v1 - 131070)), 0, 0 ))))
    | Fixed (r,g,b) -> (r, g,b)


(************************************************************************)

let wrap_index_insensitive_scaler fn =
  (fun n _ -> Scaler (fn n))


let wrap_index_insensitive_scaler_or_mem ?(colors = Constants.colors)
    structures (fn,nm) =
  (fun n step ->
     let rec check_color ind =
       if ind >= (Array.length structures)
       then fn n
       else  if (Structure.entry_mem (Structure.get_entry structures.(ind) step)
		   n.Recorded_run.key)
       then Fixed colors.(ind)
       else check_color (ind+1)
     in check_color 0),nm


let wrap_mem ?(colors = Constants.colors) structures (getter,nm) =
  (fun n step ->
     let rec check_color ind =
       if ind >= (Array.length structures)
       then getter n step
       else if (Structure.entry_mem (Structure.get_entry structures.(ind) step)
	     n.Recorded_run.key)
       then Fixed colors.(ind)
       else check_color (ind+1) in
       check_color 0), nm


let was_in_list_previously color structure (getter,nm) =
    (fun n step ->
       let rec check_step s =
	 if s > step then getter n step
	 else if (Structure.entry_mem (Structure.get_entry structure s)
		    n.Recorded_run.key)
	 then Fixed color
	 else check_step (s+1) in
	 check_step 0),nm



let wrap_first_in_struct ?(colors = Constants.colors) structures (getter,nm) =
  (fun n step ->
     let rec check_struct ind =
       if ind >= (Array.length structures)
       then getter n step
       else (let str = (Structure.get_entry structures.(ind) step).Structure.keys in
	       if Array.length str > 0 && n.Recorded_run.key = str.(0)
	       then Fixed colors.(ind)
	       else check_struct (ind+1)) in
       check_struct 0), nm


(* EOF *)
