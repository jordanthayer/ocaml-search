(** Makes a  fake single step updater *)

let make_wastar_correction get_g get_h bound =

  let h_calc parent best children = ()
  and f_calc n = (get_g n +. bound *. get_h n) in
    h_calc, f_calc


(* EOF *)
