(* $Id: dag.ml,v 1.1 2003/07/02 17:39:55 ruml Exp $
   
   directed graph with destructive updates

*)


(* list of outgoing edges *)
type 'a t = 'a Graph.t

    
exception Duplicate

exception Circularity
		     

let create () =
  Graph.create ()


let add_vertex g v =
  try 
    Graph.add_vertex g v
  with Graph.Duplicate -> raise Duplicate


let ensure_vertex g v =
  try 
    Graph.add_vertex g v
  with Graph.Duplicate -> ()
      

let out_neighbors g v =
  Graph.out_neighbors g v


let rec exists_path g src dest =
  (* depth-first search. no cycle detection since DAG *)
  let neighbors = out_neighbors g src in
    (List.mem dest neighbors) ||
    (List.exists (fun v -> exists_path g v dest)
       neighbors)
    
      
let add_arc g src dest =
  if (src = dest) || (exists_path g dest src)
  then raise Circularity
  else Graph.add_arc g src dest


let bottom_up g f =
  let cache = Hashtbl.create 20 in
  let set_cache x v = Hashtbl.add cache x v ; v in
  let rec get_f x =
    try
      Hashtbl.find cache x
    with Not_found ->
      (* no need to continue after first false - outer loop is exhaustive *)
      if (List.for_all get_f (out_neighbors g x))
      then set_cache x (f x)
      else set_cache x false
  in
    Graph.fold_vertices g (fun prev v ->
			   if get_f v
			   then prev
			   else false)
      true


let iter_bottom_up g f =
  ignore (bottom_up g (fun x -> f x; true))
      

(* EOF *)
