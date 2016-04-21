let is_monotonic arr =
  (** checks if the array of floats (arr) is monotonicly increasing.*)
  let all_okay = ref true in
    for i = 0 to ((Array.length arr) - 2) do
      if(!all_okay) then
	all_okay := arr.(i) < arr.(i+1);
    done;
    !all_okay

let weighted_average ep1 ep2 ep1v ep2v pt =
  (** Given 2 end points (ep1 and ep2) and their associated values
      (ep1v and ep2v) calculates what pt's value is supposed to be. *)
  assert (pt >= ep1);
  assert (pt <= ep2);
  ep1v +. ((pt -. ep1) /. (ep2 -. ep1)) *. (ep2v -. ep1v)

let find_index_1d arr vl =
  (** given an array and a value, finds the index into the array such
      that the value is greater than the index but greater than
      index +1. *)
  let rec helper_find arr vl start_at =
    if(start_at >= ((Array.length arr) - 1)) then
      failwith "value not found in array"
    else if (arr.(start_at) <= vl && arr.(start_at + 1) >= vl) then
      start_at
    else
      helper_find arr vl (start_at + 1)
  in
    helper_find arr vl 0

let interp1 index_values arr value =
  assert ((Array.length index_values) = (Array.length arr));
  assert (is_monotonic index_values);
  let x_base = find_index_1d index_values value in
    weighted_average index_values.(x_base) index_values.(x_base+1)
      arr.(x_base) arr.(x_base + 1) value

let interp2 x_index y_index arr x_val y_val =
  assert ((Array.length x_index) = (Array.length arr.(0)));
  assert ((Array.length y_index) = (Array.length arr));
  assert (is_monotonic x_index);
  assert (is_monotonic y_index);
  let y_base = find_index_1d y_index y_val in

  let top_y = interp1 x_index arr.(y_base) x_val in
  let bottom_y = interp1 x_index arr.(y_base + 1) x_val in

    weighted_average y_index.(y_base) y_index.(y_base + 1) top_y bottom_y
    y_val

(*
let test_array =
[|
  [|1.;3.;4.;2.;1.;|];
  [|2.;2.;5.;7.;2.;|];
  [|8.;1.;2.;3.;6.;|];
|]

let x_index = [|4.;5.;6.;7.;8.;|]
let y_index = [|2.;3.;4.;|]
*)
