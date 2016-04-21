(* $Id: incrdfs.ml,v 1.1 2003/07/17 19:12:57 ruml Exp $
 *
 *   data type for interface of an incrementally executed search algorithm
 *   *)


type ('node, 'key, 'cost) search = {
  exhausted_p : unit -> bool;
  do_step : unit -> unit;
  (*new_sface : ('node, 'key, 'cost) Search_interface.interface -> unit;*)
  cur_best : unit -> 'node option;
  new_start : 'node -> unit;
}


(**
  * makes an Incr.search object and returns it.
  *
  * @param [e] search.exhausted_p. Predicate for determining when the search is
  * exhausted.
  *
  * @param [d] search.do_step. Function which runs another planning phase.
  * @param [s] Search_inteface.interface. Used to update the current search
  * inteface being used. This is used for real-time searches when elements of
  * our search interface change between runs.
  * @param [c] unit -> 'node. Function that returns the next node to move to.
  *)
let make_incr e d c ns = 
  { exhausted_p = e;
    do_step = d;
    cur_best = c;
    new_start = ns;
  }

(* EOF *)
