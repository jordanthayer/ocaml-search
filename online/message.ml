(*
  Contains possible messages from the execution engine sent to the planner:
  1. New goals - similar to the (initial) problem in PDDL format.
  2. Toggles - change machine configurations
  3. Future events - future committments of the execution engine. Needed
  for planners that do not keep track of those by themselve.
*)

(* open Domain *)


(* 1. New goals *)
      


(* 2. Toggles 

type change = Enable | Disable


type toggle = {
  tt : float;
  switch : change;
  obj : string;
}

    
let make_toggle tt switch obj =
  { tt = tt;
    switch = switch;
    obj = obj; }
*)    


(* 3. Future events: not needed by the planner at the moment *)

(* type tl = Until | Within *)

(* an event happens at the end of an interval *)
(* type event = {
  et : float;
  fact : literal;
  ctype : tl;
} *)


(* let make_event ctype fact et =
  { et = et;
    fact = fact;
    ctype = ctype; }
*)

(* committment contains a set of events starting at a given time *)
(* type committment = {
  st : float;
  events : event list;
}
*)

(*
let make_committment st events =
  { st = st;
    events = events; }
*)    
    

(* 4. Quit *)

type message =
    NewGoals of Domain.problem
(*  | Toggles of (toggle list)
  | FutureEvents of (committment list) *)
  | Quit


      
(* EOF *)
