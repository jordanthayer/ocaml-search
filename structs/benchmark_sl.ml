(** Benchmark the skiplist against the red black tree.

    @author eaburns
    @since 2010-08-10
*)


let run sl_ch rb_ch num =
  let nums = ref [] in
  let sl = Skiplist.create (-) in
  let rb = Doset.make_with ~equals:(Some(=)) (<) 0 in
    for i = 1 to num do nums := (Random.int (num * 10)) :: !nums done;
  let _, rb_time =
    Wrsys.with_time (fun () -> List.iter (Doset.insert rb) !nums)
  in
  let _, sl_time =
    Wrsys.with_time
      (fun () -> List.iter (fun i -> ignore (Skiplist.insert sl i i)) !nums)
  in
  let sl_height =
    match sl.Skiplist.head with
      | Some head -> Array.length head.Skiplist.next
      | None -> ~-1
  in
    Printf.printf "%d:\n\tsl=%f (height=%d)\n\trb=%f\n%!"
      num sl_time sl_height rb_time;
    Printf.fprintf sl_ch "%d %f\n" num sl_time;
    Printf.fprintf rb_ch "%d %f\n" num rb_time


let do_run num =
  let sl_ch = open_out (Printf.sprintf "sl%d.dat" num)
  and rb_ch = open_out (Printf.sprintf "rb%d.dat" num) in
  let i = ref 100_000 in
    while !i <= 1_000_000 do
      run sl_ch rb_ch !i;
      i := !i + 100_000;
    done;
    close_out sl_ch;
    close_out rb_ch


let main () =
  for i = 0 to 4 do do_run i done


let _ = main ()
