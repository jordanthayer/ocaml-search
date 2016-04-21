(* $Id: graph.ml,v 1.1 2003/07/02 17:40:19 ruml Exp ruml $
   
   directed graph with destructive updates

*)


(* list of outgoing edges *)
type 'a t = ('a, 'a list) Hashtbl.t

    
let default_size = 128


(**** queries ****)


let fold_vertices g f init =
  Hashtbl.fold (fun v _ prev ->
		  f prev v)
    g init
    

let num_vertices g =
  Hashtbl.length g
    
    
let out_neighbors g v =
  Hashtbl.find g v


let exists_path g src dest =
  (* depth-first search with cycle detection *)
  let seen = Hashtbl.create default_size in
  let rec visit n =
    let neighbors = out_neighbors g n in
      (List.mem dest neighbors) ||
      (Hashtbl.add seen n true;
       List.exists (fun v ->
		      if (Hashtbl.mem seen v)
		      then false
		      else visit v)
	 neighbors)
  in
    visit src


(**** construction ****)
    

exception Duplicate
		     

let create () =
  Hashtbl.create default_size
    

let add_vertex g v =
  if (Hashtbl.mem g v)
  then raise Duplicate
  else Hashtbl.add g v []


let ensure_vertex g v =
  if not (Hashtbl.mem g v)
  then Hashtbl.add g v []
      
      
let add_arc g src dest =
  let prev = out_neighbors g src in
    Hashtbl.replace g src (dest::prev)


let add_edge g a b =
  add_arc g a b;
  add_arc g b a


let from_edges pairs =
  let g = create () in
    List.iter (fun (a,b) ->
		 ensure_vertex g a;
		 ensure_vertex g b;
		 add_edge g a b)
      pairs;
    g
      

(* EOF *)
