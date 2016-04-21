(** Search interfaces *)

open Format
open Manufact
open Manufact_inst
open Manufact_fmt

let key s = s
let hash = Hashtbl.hash
let equals a b = a = b

let domain_expand inst h s _g =
  let kids = expand inst s in
    (*
      if Verb.level Verb.debug then
      printf "@[%a@]@.@." (format_list format_state) kids;
    *)
  List.map
    (fun s' ->
      s'.h <- nan;
      s'.d <- nan;
      s'.parent <- s;
      s', float (current_plan_duration s'))
    (List.filter (fun k -> Math.finite_p (h k)) kids)

let p_update s p =
  s.parent <- p

let t inst st =
  st.mat_num

let initial_state inst h =
  let init = Manufact.initial_state inst in
  init.f <- h init;
  init

let default_interface inst limit =
  let goal_p s = is_goal inst s in
  let h, d = Manufact_heur.make_heuristic inst in
  let initial = initial_state inst h in
  let hd n = h n, d n in
  let domain_expand = domain_expand inst h in
  let t = t inst in
  Search_interface.make ~domain_expand ~key ~hash ~equals ~goal_p
    ~halt_on:limit ~p_update ~h ~d ~hd ~t
    Search_interface.Manufacture initial (fun _ _ -> false) (fun _ -> ())
