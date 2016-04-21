(** Try calling BVLS

    @author eaburns
    @since 2010-09-23
*)

let _ =
  let a = [| 2.;
	     3.;
	     4.;
	  |] in
  let b = [| 1.;
	     2.;
	     3.;
	  |] in
  let bl = [| ~-.10000.; |] in
  let bu = [| 0.5; |] in
  let dims = 3, 1 in
  let coeffs = Bvls.bvls dims a b bl bu in
  let n = Array.length coeffs in
    Printf.printf "ncoeffs=%d\n" n;
    for i = 0 to n - 1 do
      Printf.printf "%f\n" coeffs.(i)
    done
