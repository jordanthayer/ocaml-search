(* $Id: retro_algs.ml,v 1.1 2006/09/02 00:49:51 ruml Exp ruml $ 
   connecting algorithms up to the search space
*)

open Astar
open Grid_algs
open Logfn
open Grid
   

(**** logging in grid domain ****)

(* prepend grid headers to a list of headers *)
let grid_hdr opt_hdr () =
  ("x_pos")::
  ("y_pos")::opt_hdr

(* prepend grid node values to a list of values *)
let grid_log opt_log n =
  let (x,y) = n.pos in
  (string_of_int x)::
  (string_of_int y)::opt_log

(* grid does not wrap a domain *)
let grid_unwrap n = n



(**** logging in a-star ****)
(* this could alternatively be done by passing a list of values
   to any log function, from inside the algorithm *)

(* prepend a-star headers to a list of headers *)
let a_hdr opt_hdr () =
  ("f")::
  ("g")::opt_hdr

(* prepend a-star node values to a list of values *)
let a_log opt_log n =
  (string_of_float n.f)::
  (string_of_float n.g)::opt_log

(* unwrap a-star node to get domain node *)
let a_unwrap {data=n} = n



(**** logging in retrograde ****)

(* prepend retrograde headers to a list of headers *)
let retro_hdr opt_hdr () =
  ("h*")::
  ("d*")::
  ("h")::
  ("d")::opt_hdr

(* prepend retrograde node values to a list of values *)
let retro_log retro_search_hd domain_hd opt_log n =
  let (h_star, d_star) = retro_search_hd n
  and (h, d) = domain_hd n in
  (string_of_float h_star)::
  (string_of_int d_star)::
  (string_of_float h)::
  (string_of_float d)::opt_log

(* retrograde does not wrap a domain *)
let retro_unwrap n = n
(**** retrograde ****)


(**** find h* and d* for a node in the grid domain ****)
let retrograde_grid_hd w =
  let rec domain_start = {pos=w.start; parent=domain_start}
  and domain_goal = {pos = (List.hd w.goal) ; parent = domain_goal}
  and reverse_goal n =
    let w2 = {w with start= (List.hd w.goal); goal = [n.pos]} in
      (make_goal_p w2, get_cheapest_h w2)
  in
    Retrograde.retrograde_hd
      domain_goal reverse_goal (reverse_expand_func w) key


(* a-star where log function also logs h* and d* *)
let retro_a_star ?log_ch w lim =
  let perfect_hd = retrograde_grid_hd w
  and domain_hd = get_cheapest_hd w in
  let (hdr_fn,log_fn) =
    match log_ch with
      None ->
        Logfn.no_hdr, Logfn.no_log
    | Some ch -> begin
        make_hdr_fn a_hdr
          (make_hdr_fn retro_hdr
            (make_hdr_fn grid_hdr (log ch) )) ,
        make_log_fn a_unwrap a_log
          (make_log_fn retro_unwrap (retro_log perfect_hd domain_hd)
            (make_log_fn grid_unwrap grid_log (log ch) ))
      end
  in
    hdr_fn [] ();
    with_path6
      (Astar.a_star_dups ~logfn:log_fn ~limit:lim
        (make_root w) (make_goal_p w) (make_expand w) (key)
        (get_cheapest_h w) )


(* Logging espand in a wrapped domain : obsolete by logfn in forward search
let retro_a_star log_ch w lim =
  Retrograde.retro_headers log_ch ["x_pos";"y_pos"];
  let log_fn = Retrograde.retro_log log_ch begin
    fun n ->
      let (x,y) = n.pos in
        [string_of_int x; string_of_int y]
  end in
  let rec domain_start = {pos=w.start; parent=domain_start} in
  let rec domain_goal = {pos=w.goal; parent=domain_goal} in
  let reverse_goal n =
    let w2 = {w with start=w.goal; goal=n.pos} in
      (make_goal_p w2, get_cheapest_h w2) in
  let (retro_start,retro_expand) = Retrograde.retro_expand log_fn
    domain_start domain_goal reverse_goal (reverse_expand_func w)
    key (get_cheapest_hd w) (expand_func w)
  in
    with_path6 
      begin match
        (Astar.a_star_dups ~limit:lim
          retro_start
          (Retrograde.retro_is_goal (make_goal_p w))
          retro_expand
          (Retrograde.retro_key key)
          Retrograde.retro_h )
      with
        (None,a,b,c,d,e) -> (None,a,b,c,d,e)
      | ((Some (retro_sol,cost)),a,b,c,d,e) ->
          ((Some (Retrograde.retro_node retro_sol,cost)),a,b,c,d,e)
      end
*)
(* EOF *)
