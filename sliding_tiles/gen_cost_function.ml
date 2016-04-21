(**

   Functions for normalizing costs.

   Christopher Wilt

   April 5, 2011

*)


let make_cost_function ~max_cost_out ~min_cost_out ~max_cost_in ~min_cost_in =
  (** given the desired range, maps the specificed range linearly to
     the desired output range.  *)
  assert (max_cost_out >= min_cost_out);
  assert (max_cost_in > min_cost_in);
  let slope = (max_cost_out -. min_cost_out) /. (max_cost_in -. min_cost_in) in
    (fun f ->
       (f -. min_cost_in) *. slope +. min_cost_out)


let make_special sss = 
  let params = Wrstr.split_bag "_" sss in
    if(List.length params != 2) then
      invalid_arg sss;
    let max_cost = List.hd params in
    let n_cheaps = List.nth params 1 in
    let max_cost = float_of_string max_cost in
    let n_cheaps = int_of_string n_cheaps in
      (fun f -> if f <= n_cheaps then 1.0 else max_cost)

