(**

    @author jordan
   @since 2011-11-26
   maintains a rolling average of the last seen elements
*)

type rb = {
  tot_values : int;
  mutable last_index : int;
  values : float array;
  mutable mean : float;
  mutable inserted : int;
}


let init ?(pholder = infinity) num_values =
  { tot_values = num_values;
    last_index = 0;
    values = Array.create num_values pholder;
    mean = if pholder == infinity then 0. else pholder *. (float num_values);
    inserted = if pholder == infinity then 0 else num_values; }


let insert rb value =
  if Math.finite_p value then
    (let ind = rb.last_index + 1 in
     let wrapped = ind >= rb.tot_values in
     let ind' = if wrapped then 0 else ind in
     let inserted' = rb.inserted + 1 in
     let mean' =
       if rb.inserted >= rb.tot_values
       then rb.mean -. (rb.values.(rb.last_index)) +. value
       else rb.mean +. value in
       rb.inserted <- inserted';
       rb.mean <- mean';
       rb.values.(rb.last_index) <- value;
       rb.last_index <- ind')


let get_mean rb =
  if rb.inserted < rb.tot_values
  then rb.mean /. (float rb.inserted)
  else rb.mean /. (float rb.tot_values)


