(* A* based lss lrta* *)

type 'a node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  mutable h : float;
  g : float;          (* Cost of reaching a node *)
  mutable pos : int;  (* Position info for dpq *)
}

let make_node n sface =
  let h = sface.Search_interface.h n in
    {
      data = n;
      f = h;
      h = h;
      g = 0.;
      pos = Dpq.no_position;
    }

let clone_node n data_clone =
  { data = data_clone n.data;
    f = n.f;
    h = n.h;
    g = n.g;
    pos = n.pos; }

let wrap f =
  (** takes a function to be applied to the data payload
    such as the goal-test or the domain heuristic and
    wraps it so that it can be applied to the entire
    node *)
  (fun n -> f n.data)

let wrap2 f =
  (** takes a function to be applied to the data payload
    such as the goal-test or the domain heuristic and
    wraps it so that it can be applied to the entire
    node *)
  (fun a b -> f a.data b.data)

let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
    it in the format the domain expects it, which is domain data followed
    by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
    assuming that h is the same for both
    (hence f will be lower when g is lower). *)
  ((a.f : float) < b.f) ||
                      ((a.f = b.f) && (a.g >= b.g))

let f_then_g_rnd a b =
  if (a.f : float) = b.f && a.g = b.g
  then (Random.int 2) = 0 (* break equal f and g randomly *)
  else
    ((a.h : float) < b.h) || ((a.h = b.h) && (a.g < b.g))

let h_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
    assuming that h is the same for both
    (hence f will be lower when g is lower). *)
  ((a.h : float) < b.h) ||
                      ((a.h = b.h) && (a.g < b.g))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f


let just_g a b =
  (** Sorts nodes solely on total cost information *)
  (a.g : float) <= b.g


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
    Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand rev_expand h key hash equals =
  (** Takes the domain expand function and a heuristic calculator
    and creates an expand function which returns search nodes. *)
  let h_values = Htable.create hash equals 100 in

let update_h cost parent node =
  let state = key node.data in
    try
      let prev_h = node.h
      and new_h = cost +. parent.h in
        if prev_h > new_h
        then (Verb.pe Verb.never "%f became %f\n" node.h new_h;
              node.h <- new_h;
              Htable.replace h_values state new_h;
              true)
        else false
    with Not_found ->
      (let new_h = cost +. parent.h in
         Htable.add h_values state new_h;
         node.h <- new_h;
         true)

and node_expand  n =
  List.map (fun (d, g) ->
              let h_val = (try Htable.find h_values (key d)
                           with Not_found -> (h d)) in
                {data = d;
                 f = g +. h_val;
                 h = h_val;
                 g = g;
                 pos = Dpq.no_position;}) (expand n.data n.g)
and rev_expand  n =
  List.map (fun (d, g) ->
              let h_val = (try Htable.find h_values (key d)
                           with Not_found -> (h d)) in
                {data = d;
                 f = g +. h_val;
                 h = h_val;
                 g = g;
                 pos = Dpq.no_position;}) (rev_expand n.data n.g) in
  node_expand, update_h, rev_expand



let cost_delta a b =
  a.g -. b.g (* assume that a.g >= b.g *)


let set_h a h =
  a.h <- h


(**************************************************************************)

let make_sface ?(chan = stdout) sface =
  let def_log = Limit.make_default_logger ~ch:chan (fun n -> n.f)
                  (wrap sface.Search_interface.get_sol_length) in
  let node_expand, update_h, predecessors = make_expand
    sface.Search_interface.domain_expand
    sface.Search_interface.predecessor
    sface.Search_interface.h sface.Search_interface.key
    sface.Search_interface.hash sface.Search_interface.equals 
  in
    Search_interface.make
      ~node_expand:node_expand
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
                 ~key_print:sface.Search_interface.key_printer
              ~hash:sface.Search_interface.hash
              ~equals:sface.Search_interface.equals
              ~halt_on:sface.Search_interface.halt_on
              ~p_update:(wrap2 sface.Search_interface.parent_update)
              sface.Search_interface.domain
              {data = sface.Search_interface.initial;
               f = (sface.Search_interface.h sface.Search_interface.initial);
               h = (sface.Search_interface.h sface.Search_interface.initial);
               g = 0.;
               pos = Dpq.no_position;}
              just_f
              (fun i ->
                 sface.Search_interface.info.Limit.log
                   (Limit.unwrap_info (fun n -> n.data) i);
                 def_log i), update_h, predecessors


let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
    for domains with no duplicates. *)
  let look_ahead = Search_args.get_int "Astar_lsslrta.no_dups" args 0 in
let search_interface, update_h, predecessors = make_sface sface in
  Limit.unwrap_sol5 unwrap_sol
    (Lsslrtastar.no_dups
       search_interface
       predecessors
       look_ahead
       f_then_g
       h_then_g
       cost_delta
       set_h
       update_h
       just_g
       setpos
       getpos)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
    for domains where duplicates are frequently encountered. *)
  let look_ahead = Search_args.get_int "Astar_lsslrta.dups" args 0 in
let search_interface, update_h, predecessors = make_sface sface in
  Limit.unwrap_sol6 unwrap_sol
    (Lsslrtastar.dups
       search_interface
       predecessors
       look_ahead
       f_then_g
       h_then_g
       cost_delta
       set_h
       update_h
       just_g
       setpos
       getpos)

(** An incremental version of lss-lrta* that will allow us to
  * get each piece of the path incrementally.
  *
  * returns an Incr struct that can be used to control how the algorithm
  * is run 
  *)
let incr_lsslrtastar ?(ch = stdout) sface args = 
  let look_ahead = Search_args.get_int "Incr_lsslrtastar" args 0 in
  let search_iface, update_h, pred = make_sface ~chan:ch sface in
    Lsslrtastar.incr_lsslrtastar 
      search_iface 
      pred 
      look_ahead
      f_then_g_rnd
      h_then_g
      cost_delta
      set_h
      update_h
      just_g
      setpos
      getpos
(* EOF *)
