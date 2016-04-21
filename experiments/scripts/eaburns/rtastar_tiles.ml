(* Running RTA* on tiles *)

open Sliding_tiles_runs;;

List.iter
  (fun d ->
    do_batch "korf" ~nrows:4 ~ncols:4 (Printf.sprintf "srtastar %d" d)
      [ "alg", "srtastar"; "search horizon", string_of_int d ])
  (Array.to_list (Array.init 25 (fun i -> i + 1)));;
