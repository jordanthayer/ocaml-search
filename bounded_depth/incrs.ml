(* $Id: incrdfs.ml,v 1.1 2003/07/17 19:12:57 ruml Exp $
   
   data type for interface of an incrementally executed search algorithm
*)


type ('a, 'b) search = {
  exhausted_p : unit -> bool;
  do_step : unit -> unit;
  curr_best : unit -> 'a option;
  curr_stats : unit -> 'b
}
    

(* EOF *)
