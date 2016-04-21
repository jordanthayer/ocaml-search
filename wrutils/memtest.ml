(*

  Returns a little function that when called returns true if the major
  major heap is smaller than main memory, false if it is bigger.

*)

let memcheck () =
  (*finds out how much memory there is*)

  let bits_per_word = float_of_int (Sys.word_size / 8) in

    let total_memory = ref 0. in
      Scanf.sscanf (Wrsys.shell_output "free -b")
	" %s %s %s %s %s %s %s %d %d %f "
	(fun _ _ _ _ _ _ _ _ _ n -> total_memory := (n
						     /. bits_per_word));

      (*function that compares the major heap's size to the total
	amount of memory*)
      (
	fun () ->
	  let gcs = Gc.quick_stat () in
	  let hs = float_of_int (gcs.Gc.heap_words) in
	    (*
	      Printf.fprintf stderr "used: %f total: %f percent %f\n"
	      hs !total_memory (hs /. !total_memory);
	      flush stderr;
	    *)
	    (hs) < !total_memory
      );;


let get_size_main_memory () =
  (*finds out how much memory there is*)
  Scanf.sscanf (Wrsys.shell_output "free -k")
    " %s %s %s %s %s %s %s %d %d %f "
    (fun _ _ _ _ _ _ _ _ _ n -> n)

