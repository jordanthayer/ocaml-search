(* $Id: line_bars.ml,v 1.1 2003/07/18 21:10:24 ruml Exp ruml $

   ?????????????
*)


(******* data processing ********)


let bar_locations num_bars_per num_pairs min max =
  let group_interval = (max -. min) /. num_bars_per in
  let group_offset = group_interval /. 2. in
  let pair_interval = group_interval /. num_pairs in
    group_interval, group_offset, pair_interval


(*let bar_pairs*)


let mean_and_bars pairs =
(*  let master, deps = normalized_data pairs in
    let mean = mean_col deps in*)
    (* sort by avg height *)
    (* select bar locations *)
    (* get bar data *)
    (*foo*) ()


(****** making plot ********)



(* make sure range is compute within each batch only, not across
   batches of pairs as well *)

let plot path data x_label y_label title =
()


(* EOF *)
