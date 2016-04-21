


let write_problem ch prob = 
  Wrio.output_int ch prob.Topspin.n_disks;
  Printf.fprintf ch " ";
  Wrio.output_int ch prob.Topspin.k_flipper;
  Printf.fprintf ch "\n";
  Wrarray.write_ints ch prob.Topspin.initial_configuration.Topspin.disks;
  Printf.fprintf ch "\n"

let read_problem ch = 
  let n_disks = Wrio.input_int ch in
  let k_flipper = Wrio.input_int ch in
  let initial = Wrarray.read_ints ch n_disks in
    {
      Topspin.n_disks = n_disks;
      Topspin.k_flipper = k_flipper;
      Topspin.initial_configuration = (Topspin.make_initial initial);
    }


let read_from_file fn = 
  let inch = open_in fn in
  let to_return = read_problem inch in
    close_in inch;
    to_return

