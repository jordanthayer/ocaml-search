(**
   Run-length encoding.
*)

let max = 127
  (** Maximum number of characters in a run or non-run. *)
let run_min = 3
  (** Minimum number of chacacters in a run. *)

let has_run buf =
  (** Check if there is a run of at least [run_min] length at the end
      of the buffer. *)
  let len = Buffer.length buf in
    if len < run_min then None
    else
      let run = ref true
      and str = Buffer.sub buf (len - run_min) run_min in
	for i = 1 to (String.length str) - 1 do
	  if (String.get str i) <> (String.get str 0) then run := false
	done;
	if not !run then None
	else
	  let front = Buffer.sub buf 0 (len - run_min) in
	    Some (front, run_min, Buffer.nth buf (len - 1))


let encode data =
  (** Run-length encode the data stream.  Returns a buffer of the
      encoded data. *)
  let output = Buffer.create run_min in

  let rec output_run num chr =
    (* Output a run of num chr's. *)
    if num > max
    then begin
      output_run max chr;
      output_run (num- max) chr
    end else begin
      Buffer.add_char output (char_of_int ((1 - num) land 0xff));
      Buffer.add_char output chr
    end
  and output_chars chrs =
    (* Output a string of characters. *)
    let len = String.length chrs in
      if len > max then begin
	output_chars (Str.string_before chrs max);
	output_chars (Str.string_after chrs max);
      end else begin
	Buffer.add_char output (char_of_int ((len - 1) land 0xff));
	Buffer.add_string output chrs
      end
  in

  let buf = Buffer.create run_min in
    while (Stream.peek data) <> None do
      let c = Stream.next data in
	Buffer.add_char buf c;
	match has_run buf with
	  | Some (front, count, char) ->
	      (* Run-found *)
	      begin
		if (String.length front) > 0 then output_chars front;
		Buffer.reset buf;
		let run_len = ref count
		and finish = ref false in
		  while not !finish && (Stream.peek data) <> None do
		    let c = Stream.next data in
		      if c = char then incr run_len
		      else begin
			Buffer.add_char buf c;
			finish := true
		      end
		  done;
		  output_run !run_len char
	      end
	  | None -> ()
    done;
    if (Buffer.length buf) > 0 then
      output_chars (Buffer.contents buf);
    output
