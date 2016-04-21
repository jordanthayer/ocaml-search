(** An array that is initialized on-demand.  This is implemented with
    a hash table so you can use negative integers for the keys too...

    This is keyed with int arrays to simulate multi dimensional
    arrays.

    @author eaburns
    @since 2010-03-23
*)

type 'data t = {
  dim : int;
  tbl : (int array, 'data) Hashtbl.t;
  init_fun : int array -> 'data;
}

let create ~dim ~init_size init_fun =
  {
    dim = dim;
    tbl = Hashtbl.create init_size;
    init_fun = init_fun;
  }


let get tbl index =
  (** [get tbl index] gets the value at the given index.  If the value
      was not set then it is lazily created using the initialization
      function. *)
  try Hashtbl.find tbl.tbl index
  with Not_found ->
    let v = tbl.init_fun index in
      Hashtbl.add tbl.tbl index v;
      v


let set tbl index v =
  (** [set tbl index] sets the value for [index] in the table to be
      [v]. *)
  Hashtbl.replace tbl.tbl index v


let iter tbl f =
  (** [iter tbl f] iterates [f] over each element of [tbl]. *)
  Hashtbl.iter f tbl.tbl
