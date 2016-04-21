(** Place for storing the most general purpose recorders for recordedxpan
    search. Jordan, Aug 2009*)


let make_key_printer pkey key =
  (** Returns a single function which will print the key of a node.
      takes a key printker [pkey] and a [key] function *)
  (fun n -> Verb.pe Verb.often "%s" (pkey (key n)))


let make_node_printer pkey key get_h get_d get_rh get_rd get_g get_depth =
  (** Prints single nodes, mostly useful for debugging.
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
      [get_h]      - returns the heuristic estimate of a node
      [get_d]      - returns the distance estimate of a node
      [get_rh]     - like h, but towards root
      [get_rd]     - like d, but towards root
      [get_g]      - returns the cost of arriving at a node
      [get_depth]  - returns the depth of a node *)
  (fun n ->
     Verb.pe Verb.often (pkey (key n));
     Verb.pe Verb.often "|%f,%f|%f,%f,%f,%f" (get_g n)
       (float_of_int (get_depth n)) (get_h n) (get_d n) (get_rh n) (get_rd n))


let none =
  (** A dummy function for when you need to not record something *)
  (fun _ _ -> ())

let no_node_record = (fun _ _ _ _ -> ())

let truth_recorder pkey key true_h true_d admiss_h admiss_d =
  (** records true heuristics and admissible heuristics of a node *)
  (fun _ parent _ _ ->
     Verb.pe Verb.often "%s" (pkey (key parent));
     Verb.pe Verb.often "|%f,%i|%f,%f\n" (true_h parent) (true_d parent)
       (admiss_h parent) (admiss_d parent))

let tab_delim_recorder pkey key true_h true_d admiss_h admiss_d rh rd =
  (fun _ parent _ _ ->
     Verb.pe Verb.often "%s" (pkey (key parent));
     Verb.pe Verb.often "\t%f\t%i\t%f\t%f\t%f\t%f\n"
       (true_h parent) (true_d parent)
       (admiss_h parent) (admiss_d parent) (rh parent) (rd parent))


let expansion_recorder pkey key get_g get_depth get_f =
  (** Records the expansion order of a search, for use with visualization
  tool
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
      [get_g]      - returns the cost of arriving at a node
      [get_depth]  - returns the depth of a node
      [get_f]      - returns the cost of a node *)
  let minimal_np =
    (fun n par ->
       Verb.pe Verb.often "%s" (pkey (key n));
       Verb.pe Verb.often "|%s|%f|%i|%f|" (pkey (key par)) (get_g n)
	 (get_depth n) (get_f n)) in
    (fun info parent gparent children ->
       Verb.pe Verb.often "%i|" info.Limit.expanded;
       minimal_np parent gparent;
       List.iter (fun c -> Verb.pe Verb.often "%s " (pkey (key c))) children;
       Verb.pe Verb.often "\n";
       List.iter (fun n -> minimal_np n parent; Verb.pe Verb.often "\n") children)


let dpq_recorder pkey key =
  (** Returns a recorder for a priority queue which will record elements in
      priority order.
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
  *)
  (fun info openlist ->
     Verb.pe Verb.often "%i" info.Limit.expanded;
     let iterator,reset = Dpq.make_iterator_unsafe openlist in
     let rec do_print node =
       match node with
	   Some thing -> (Verb.pe Verb.often " %s"
			  (pkey (key thing)); do_print (iterator()))
	 | None -> Verb.pe Verb.often "\n"
     in
       do_print (iterator ()))


let stack_recorder pkey key =
  (** Returns a recorder for a stack which will record elements in stack.
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
  *)
  (fun info openlist ->
     Verb.pe Verb.often "%i" info.Limit.expanded;
     let do_print node =
       Verb.pe Verb.often " %s" (pkey (key node)) in
       Stack.iter do_print openlist;
       Verb.pe Verb.often "\n")


let lq_recorder pkey key =
  (** Returns a recorder for a priority queue which will record elements in
      priority order.
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
  *)
  (fun info openlist ->
     Verb.pe Verb.often "%i" info.Limit.expanded;
     let iterator,reset = Lq.make_iterator_unsafe openlist in
     let rec do_print node =
       match node with
	   Some thing -> (Verb.pe Verb.often " %s"
			    (pkey (key thing)); do_print (iterator()))
	 | None -> Verb.pe Verb.often "\n"
     in
       do_print (iterator ()))


let pq_recorder pkey key =
  (** Returns a recorder for a priority queue which will record elements in
      priority order.
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
  *)
  (fun info openlist ->
     Verb.pe Verb.often "%i" info.Limit.expanded;
     let iterator,reset = Picky_queue.make_iterator_unsafe openlist in
     let rec do_print node =
       match node with
	   Some thing -> (Verb.pe Verb.often " %s"
			    (pkey (key thing)); do_print (iterator()))
	 | None -> Verb.pe Verb.often "\n"
     in
       do_print (iterator ()))



let geq_focal_recorder pkey key =
  (** Returns a recorder for the focal portion of a geq  which will record
      elements in order.
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
  *)
  (fun info geq ->
     Verb.pe Verb.often "%i" info.Limit.expanded;
     Geq.ge_iter (fun n -> Verb.pe Verb.often " %s" (pkey (key n))) geq;
     Verb.pe Verb.often "\n")


let geq_all_recorder pkey key =
  (** Returns a recorder for the entirety of a geq  which will record
      elements in order.
      [pkey]       - prints the key value of a node
      [key]        - returns the key value of a node
  *)
  (fun info geq ->
     Verb.pe Verb.often "%i" info.Limit.expanded;
     Geq.in_order_iter (fun n -> Verb.pe Verb.often " %s" (pkey (key n))) geq;
     Verb.pe Verb.often "\n")


(* EOF *)
