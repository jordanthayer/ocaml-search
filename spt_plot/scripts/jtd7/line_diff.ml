(**

    @author jtd7
    @since 2011-03-30

*)

let fuck_you_div a b =
  assert (b <> 0.);
  let v = if a = b then 1. else a /. b in
    if Math.finite_p v then v else 1.

let y_at_x xar yar xval =
  Verb.pe Verb.debug "Getting y at x: %f" xval;
  let ind = ref ((Array.length xar) - 1) in
    for i = 0 to ((Array.length xar) - 1)
    do
      if xval >= xar.(i) then ind := i
    done;
    Verb.pe Verb.debug "\t %f\t:\t" yar.(!ind);
    yar.(!ind)


let sing_instp xar_1 yar_1 xar_2 yar_2 =
  let get_y = y_at_x xar_1 yar_1 in
    Wrarray.map2 (fun x y ->
		    let base_y = get_y x in
		    let v = fuck_you_div y base_y in
		      assert (not (Math.nan_p v));
		      Verb.pe Verb.toplvl "%f /. %f\n%!" y base_y;
		      v) xar_2 yar_2



(* EOF *)
