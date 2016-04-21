(* $Id: eqvclass.ml,v 1.1 2003/06/27 21:14:18 ruml Exp $
   
   equivalence classes

*)


type ('a, 'b) t = ('a, 'b Dset.t) Hashtbl.t
    (** implemented as a hash table of sets *)

    
let create () =
  Hashtbl.create 16


let get_class rep ec =
  try
    Hashtbl.find ec rep
  with Not_found ->
    let n = Dset.create 16 in
      (* representative is not added to the class *)
      Hashtbl.add ec rep n ;
      n

	
let add rep x ec =
  let c = get_class rep ec in
    Dset.add x c

	
let add_new_p rep x ec =
  let c = get_class rep ec in
  let new_p = Dset.mem x c in
    Dset.add x c ;
    new_p


let fold init_f class_f ec_f init ec =
  Hashtbl.fold (fun rep c accum ->
		  let init = init_f accum rep in
		  let this = Dset.fold class_f init c in
		    ec_f accum this)
    ec init


let from_list key l =
  let c = create () in
    List.iter (fun x -> add (key x) x c)
      l;
    c


let to_lists ec =
  fold
    (* before each class, start with [] *)
    (fun classes _ -> [])
    (* in each class, accumulate objects *)
    (fun objs x -> x::objs)
    (* after each class, accumulate lists *)
    (fun classes objs -> objs::classes)
    (* start with no lists *)
    []
    ec	  
    

(* EOF *)
