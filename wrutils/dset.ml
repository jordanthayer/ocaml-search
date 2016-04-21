(* destructive sets

*)


type 'a t = ('a, bool) Hashtbl.t


let create = Hashtbl.create


let add x ds =
  Hashtbl.replace ds x true


let count ds =
  Hashtbl.length ds
    

let mem x ds =
  Hashtbl.mem ds x


let add_new_p x ds =
  let n = not (mem x ds) in
    if n then Hashtbl.replace ds x true;
    n


let fold f init ds =
  Hashtbl.fold (fun k _ accum ->
		  f accum k)
    ds init


let iter f ds =
  Hashtbl.iter (fun k _ ->
		  f k)
    ds

    
let to_list ht =
  fold (fun accum x ->
	  x::accum)
    [] ht


(* EOF *)
