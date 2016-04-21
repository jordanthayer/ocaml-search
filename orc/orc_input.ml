(**

   reads orc bombing files.

*)


let read_from instream = 
  ignore (Wrio.read_token instream);
  let x_size = Wrio.input_int instream in
  let y_size = Wrio.input_int instream in
    ignore (Wrio.read_token instream);
    let min_cost = Wrio.input_float instream in
    let max_cost = Wrio.input_float instream in
      ignore (Wrio.read_token instream);
      let p_blocked = Wrio.input_float instream in
      let board = 
	Array.init y_size
	  (fun _ ->
	     let to_return = Vector.read_from_line instream in
	       assert (Array.length to_return = x_size);
	       to_return;
	  )
      in
	{
	  Orc.cells = board;
	  Orc.x_size = x_size;
	  Orc.y_size = y_size;
	  Orc.min_cost = min_cost;
	  Orc.max_cost = max_cost;
	  Orc.p_blocked = p_blocked;
	}


let read_file fn = 
  let is = open_in fn in
  let new_instance = read_from is in
    close_in is;
    new_instance
