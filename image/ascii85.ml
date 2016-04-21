(**
   ASCII85 encode a data stream.
*)


let max_width = 72
  (* The maximum width of the data. *)

let encode data =
  (** ASCII85 encoding of the given data. *)
  let init_size = 100 in
  let out = Buffer.create init_size in

  let do_tuple ?(width=0) tuple count =
    (* Encode a single tuple.  Result in the new width of the text. *)
    let eighty_five = Int64.of_int 85 in
    let rec do_div ?(i=5) ?(accum=[]) tuple =
      if i = 0 then accum
      else
	let offset = int_of_char '!' in
	let ival = Int64.to_int (Int64.rem tuple eighty_five) in
	let as_char = char_of_int (ival + offset) in
	  do_div ~i:(i - 1) ~accum:(as_char::accum)
	    (Int64.div tuple eighty_five)
    in
      if tuple = Int64.zero then begin
	Buffer.add_char out 'z';
	0
      end else
	let n = do_div tuple in
	let rec add_chars count chars =
	  if count >= 0 then begin
	    Buffer.add_char out (List.hd chars);
	    add_chars (count - 1) (List.tl chars)
	  end else () in
	  add_chars count n;
	  if (List.length n) + width > max_width then begin
	    Buffer.add_char out '\n';
	    0
	  end else width + (List.length n)
  in

  let rec do_encode ?(width=0) ?(tuple=Int64.zero) ?(count=0) () =
    (* Encode the data stream. *)
    if Stream.peek data <> None then
      let byte = Int64.of_int (int_of_char (Stream.next data)) in
	if count = 3 then
	  let width = do_tuple ~width:width (Int64.logor tuple byte)
	    (count + 1) in
	    do_encode ~width:width ()
	else
	  let shift =
	    match count with
	      | 0 -> 24
	      | 1 -> 16
	      | 2 -> 8
	      | _ -> assert false in
	    do_encode
	      ~width:width
	      ~tuple:(Int64.logor tuple (Int64.shift_left byte shift))
	      ~count:(count + 1) ()
    else
      do_tuple tuple count
  in

    ignore (do_encode ());
    Buffer.add_string out "~>";
    out
