(** Holding sets of integers with constant insert and amortized
    constant lookup and delete if that the integers in the set are
    distributed uniformly.

    @author eaburns
    @since 2010-05-29
*)

type t = {
  mutable bins : int list array;
  mutable nbins : int;
  mutable fill : int;
}

let create nbins =
  (** [create nbins] makes a new intset. *)
  {
    bins = Array.create nbins [];
    nbins = nbins;
    fill = 0;
  }


let consider_grow t =
  (** [consider_grow t] considers growing the number of bins. *)
  if t.fill > t.nbins / 2
  then begin
    let bins = t.bins in
    let nbins' = t.nbins * 2 in
    let bins' = Array.create nbins' [] in
      for i = 0 to t.nbins - 1 do
	List.iter (fun elm ->
		     let i' = elm mod nbins' in
		       bins'.(i') <- elm :: bins'.(i'))
	  bins.(i)
      done;
      t.bins <- bins';
      t.nbins <- nbins';
  end


let insert t elm =
  (** [insert t elm] inserts an integer into the set. *)
  let bins = t.bins in
  let i = elm mod t.nbins in
    bins.(i) <- elm :: bins.(i);
    t.fill <- t.fill + 1;
    consider_grow t


let remove t elm =
  (** [remove t elm] removes an element from the bins. *)
  let bins = t.bins in
  let i = elm mod t.nbins in
    bins.(i) <- List.filter ((=) elm) bins.(i);
    t.fill <- t.fill - 1


let mem t elm =
  (** [mem t elm] tests for membership. *)
  let i = elm mod t.nbins in
  let rec scan_bin elm = function
    | [] -> false
    | hd :: _ when hd = elm -> true
    | _ :: tl -> scan_bin elm tl
  in scan_bin elm t.bins.(i)
