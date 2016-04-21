(* LSS-LRTA*)

let astar look_ahead openlist closedlist i setpos getpos sface better_p =
  (** Simple implementation of A* search, which lsslrtastar relies on.
    Technically it isn't A*, but any best first algorithm.  You just have to
    change the ordering of the openlist when you construct it in [search].
    [look_ahead] - how many steps of the bfs are we allowed to do
    [openlist] - openlist for the bfs
    [closedlist] - closed list for the bfs
    [i] - Limit.info
    [setpos] - sets position of nodes in the [openlist]
    [getpos] - returns postion of node in the [openlist]
    [sface] - search interface, has key and goal_p
    [better_p] - is a.g < b.g  *)

  let consider kid =
    (* should [kid] be inserted into open and closed? *)
    Limit.incr_gen i;
    let state = sface.Search_interface.key kid in
      try
        let prev = Htable.find closedlist state in
          Limit.incr_dups i;
          if not (better_p prev kid)
          then (Htable.replace closedlist state kid;
                let ppos = getpos prev in
                  if ppos = Dpq.no_position
                  then Dpq.insert openlist kid
                  else Dpq.swap openlist ppos kid)
      with Not_found ->
        Dpq.insert openlist kid;
        Htable.add closedlist state kid in

  let rec next_astar_step remaining =
    (* do a single step of best first search so long as the queue
     isn't empty and we aren't past our lookahead threshold. *)
    if remaining > 0 && not (Dpq.empty_p openlist)
    then
      (let next = Dpq.extract_first openlist in
         setpos next Dpq.no_position;
         if sface.Search_interface.goal_p next
         then (Printf.fprintf stderr "astar found the goal!\n%!";
               (*Limit.new_incumbent i (Limit.Incumbent (0., next));*)
               i.Limit.incumbent <- Limit.Incumbent (0., next);
               true)
         else (Limit.incr_exp i;
               List.iter consider (sface.Search_interface.node_expand next);
               next_astar_step (remaining - 1)))
    else (if Dpq.empty_p openlist then (Printf.fprintf stderr"Open is empty!"; true) else false) in

    (fun start ->
       Dpq.clear openlist;
       Htable.clear closedlist;
       Htable.add closedlist (sface.Search_interface.key start) start;
       Dpq.insert openlist start;
       next_astar_step look_ahead)


let dijkstra just_h cost_delta set_h update_h setpos getpos key expand i
      openlist closedlist =
  (** This is where lss-lrta* updates the heuristic values by doing dijkstra's
    algorithm back from the fringe.
    [just_h] - Sorts the nodes on just their heuristic values
    [cost_delta] - Calculates the transition cost between two nodes
    [set_h] - Sets the heuristic value of a node
    [update_h] - Updates the heuristic value of a node
    [setpos] - sets position of nodes in the [openlist]
    [getpos] - returns postion of node in the [openlist]
    [key] - strings things into the hashtbl
    [expand] - Generates predecessors
    [i] - Limit.info
    [openlist] - openlist for the bfs
    [closedlist] - closed list for the bfs   *)
  (fun () ->
     let h_open = (Dpq.create just_h (fun n i -> ()) (Dpq.count openlist)
                     (Dpq.peek_first openlist)) in
       Htable.iter (fun key ele -> if getpos ele = Dpq.no_position
                    then set_h ele infinity
                    else (Htable.remove closedlist key;
                          Dpq.insert h_open ele)) closedlist;
       assert ((Dpq.count h_open) = (Dpq.count openlist));
       while not (Dpq.empty_p h_open) && ((Htable.length closedlist) > 0) do
         (let s = Dpq.extract_first h_open in
          let successors = expand s in
            Htable.remove closedlist (key s);
            List.iter
              (fun sprime ->
                 try
                   let prev = Htable.find closedlist (key sprime) in
                     (if update_h (cost_delta sprime s) s prev
                      then Dpq.insert h_open prev)
                 with Not_found -> ()) successors)
       done)


let search sface predecessors look_ahead f_order h_order cost_delta set_h
      update_h better_p setpos getpos =
  let openlist = Dpq.create f_order setpos 100 sface.Search_interface.initial
  and closedlist = Htable.create sface.Search_interface.hash
                     sface.Search_interface.equals 100
  and i = sface.Search_interface.info in
  let call_astar = (astar look_ahead openlist closedlist i setpos getpos
                      sface better_p)
  and call_dijkstra = (dijkstra h_order cost_delta set_h update_h
                         setpos getpos sface.Search_interface.key
                         predecessors i openlist closedlist)
  and s_start = ref sface.Search_interface.initial in
  let rec do_step () =
    let empty_open = Dpq.empty_p openlist
    and goal_at_start = (sface.Search_interface.goal_p !s_start) in
      if not (Limit.halt_p i) && not empty_open && not goal_at_start
      then
        (if not (call_astar !s_start) (* returns true when goal is found *)
         then
           (s_start := Dpq.peek_first openlist;
            call_dijkstra ();
            do_step ()))
      else (if goal_at_start
            then (Limit.new_incumbent i (Limit.Incumbent (0., !s_start))))
  in
    Dpq.insert openlist !s_start;
    do_step ();
    i



(* used to return an Incr structure that allows us to search the LSS and
 * retrieve plans in a piecewise manner
 *)
let incr_search sface predecessors look_ahead f_order h_order cost_delta set_h
      update_h better_p setpos getpos =

  let sface = ref sface in
  let openlist = Dpq.create f_order setpos 100 !sface.Search_interface.initial
  and closedlist = Htable.create !sface.Search_interface.hash
                   !sface.Search_interface.equals 100
  and i = !sface.Search_interface.info in
  let goal' = ref None in
  let call_astar = (astar look_ahead openlist closedlist i setpos getpos
                    !sface better_p)
  and call_dijkstra = (dijkstra h_order cost_delta set_h update_h
                       setpos getpos !sface.Search_interface.key
                       predecessors i openlist closedlist)
  and s_start = ref !sface.Search_interface.initial in
  let new_start s =
    s_start := s;
    Dpq.insert openlist !s_start
  and cur_best () = !goal' in
  let exhausted () = Dpq.empty_p openlist || 
                     (!sface.Search_interface.goal_p !s_start) || 
                     (Limit.halt_p i)  in
  let rec do_step () =
    let goal_at_start = !sface.Search_interface.goal_p !s_start in
    if not (exhausted ()) then begin 
      if not (call_astar !s_start) then begin 
        s_start := Dpq.peek_first openlist;
        call_dijkstra ();
        if not (Dpq.empty_p openlist) then
          (* best node in LSS becomes the current incumbent *)
          goal' := Some (Dpq.peek_first openlist)
        else
          goal' := None
      end else begin
        match i.Limit.incumbent with
            | Limit.Nothing -> failwith 
                                 "the goal node found was not set as incumbent"
            | Limit.Incumbent (_,n) -> goal' := Some n
      end
    end else 
      if goal_at_start then begin
        i.Limit.incumbent <- Limit.Incumbent (0., !s_start);
        (*Limit.new_incumbent i (Limit.Incumbent (1., !s_start));*)
        goal' := Some !s_start
      end
  in
    Dpq.insert openlist !s_start;
    Incr.make_incr exhausted do_step cur_best new_start



let no_dups sface predecessors look_ahead f_order h_order cost_delta set_h
      update_h better_p setpos getpos =
  Limit.results5
    (search sface predecessors look_ahead f_order h_order cost_delta set_h
       update_h better_p setpos getpos)


and dups sface predecessors look_ahead f_order h_order cost_delta set_h
    update_h better_p setpos getpos =
      Limit.results6
        (search sface predecessors look_ahead f_order h_order cost_delta set_h
           update_h better_p setpos getpos)


and incr_lsslrtastar sface predecessors look_ahead f_order h_order
        cost_delta set_h update_h better_p setpos getpos =
          incr_search sface predecessors look_ahead f_order h_order cost_delta set_h
            update_h better_p setpos getpos

(* EOF *)
