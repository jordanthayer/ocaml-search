(** The basic structure of the graph *)

type node = {
  id : int;
  mutable neighbors : (node * float) list;
}

type graph = {
  seed  : int;
  nodes : node array;
}

(**************************************************************************)

let node_to_string n =
  (** Converts a node [n] into a string representation of n *)
  let delim = " " in
  let rec neighbor_string cur nbrs =
    match nbrs with
	[] -> cur
      | (hd,_)::tl -> neighbor_string
	  (cur ^ delim ^ string_of_int hd.id) tl in
  (Wrutils.str "Name: %i\nNeighbors:%s" n.id
     (neighbor_string "" n.neighbors))


let graph_to_string g =
  (** Converts a graph [g] to a string *)
  Wrutils.str "Seed:%i%s" g.seed
    (Array.fold_left (fun accum node -> accum ^ "\n" ^ (node_to_string node))
       "" g.nodes)

(************************ Testing Code ***********************************)

let build_unit_connected n =
  (** builds a fully connected graph of size n *)
  let nodes = Array.init n (fun i -> { id = i; neighbors = []}) in
    for current = 0 to (n - 1) do
      for adding = 0 to (n - 1) do
	if current != adding
	then nodes.(current).neighbors <-
	  (nodes.(adding),1.)::nodes.(current).neighbors
      done
    done;
    {seed = -1;
     nodes = nodes;}



(* EOF *)
