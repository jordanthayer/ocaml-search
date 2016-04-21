open Tsp
open Tsp_instances

(************* informal testing *************)


let make_logger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f e g ->
       (** cost, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!" f e g t;
	   threshold := f *. 0.999)

let test a w =
  let res = Wrsys.with_time (fun () -> a w) in
    Tsp_algs.print_results res

let onerun alg instance =
  test alg instance

let get_instance tsp =
  let ic = Unix.openfile tsp [Unix.O_RDONLY] 775 in
    read_tsplib (Unix.in_channel_of_descr ic)

let compare_algs_on tsp =
  let instance = get_instance tsp in
    Wrutils.pr "\n--- A* ---\n\n";
    test (Tsp_algs.speedy) instance



(* EOF *)
