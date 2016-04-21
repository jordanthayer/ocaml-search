(* $Id: coatcheck.ml,v 1.1 2005/04/03 19:17:40 ruml Exp ruml $

*)


type 'a t = {
  tbl : ('a, int) Hashtbl.t;
  mutable objs : 'a array;
}


let count t =
  Hashtbl.length t.tbl


let create approx_num =
  { tbl = Hashtbl.create approx_num;
    objs = [||] }


let copy t =
  { tbl = Hashtbl.copy t.tbl;
    objs = Array.copy t.objs; }


let intern t x =
  let tbl = t.tbl in
  try
    Hashtbl.find tbl x
  with Not_found ->
    let fp = Hashtbl.length tbl in
    let a = t.objs in
    let max = (Array.length a) - 1 in
    if fp >= max then begin
      let size = if max <= 0 then 2 else max * 2 in
      t.objs <- Wrarray.extend a size x;
    end;
    t.objs.(fp) <- x;
    Hashtbl.add tbl x fp;
    fp


let retrieve t tag =
  t.objs.(tag)


let iter f t =
  let a = t.objs in
    for i = 0 to (Hashtbl.length t.tbl) - 1 do
      f i a.(i)
    done


let fold f a t =
  let curr = ref a in
    iter (fun i o ->
	    curr := f !curr i o)
      t;
    !curr


(************ debugging *************)


let check t =
  Hashtbl.iter (fun k v ->
                  assert (t.objs.(v) = k))
    t.tbl;
  assert (not (Wrarray.duplicates_p t.objs))


module Str = struct
(* A string intern table.  This is similar to Coatcheck but optimized
   for strings. *)

  module Strhtbl = Hashtbl.Make(struct
    type t = string
    let equal a b = String.compare a b = 0
    let hash (s:string) = Hashtbl.hash s
  end)

  type t = {
    tbl : (int * string) Strhtbl.t;
    mutable ary : string array;
    mutable nxt : int;
  }

  let empty_str = ""

  let create init_size =
    { tbl = Strhtbl.create init_size;
      ary = Array.create init_size empty_str;
      nxt = 0; }

  let consider_resize t =
    let len = Array.length t.ary in
    if t.nxt >= len then begin
      let len' = max (len * 2) 1 in
      assert (t.nxt < len');
      let ary = t.ary in
      let init_elm = function
	| i when i < len -> ary.(i)
	| _ -> empty_str in
      t.ary <- Array.init len' init_elm;
    end

  let next_id t =
    consider_resize t;
    let id = t.nxt in
    t.nxt <- t.nxt + 1;
    id

  let get t str =
    try
      Strhtbl.find t.tbl str
    with Not_found ->
      let id = next_id t in
      let ent = id , str in
      t.ary.(id) <- str;
      Strhtbl.add t.tbl str ent;
      ent

  let length t =
    Strhtbl.length t.tbl

  let insert t str =
    ignore (get t str)

  let intern t str =
    snd (get t str)

  let id t str =
    fst (get t str)

  let lookup t id =
    t.ary.(id)

  let iter f t =
    let ary = t.ary in
    for i = 0 to t.nxt - 1 do
      f i ary.(i)
    done

  let fold f init t =
    let accum = ref init in
    let ary = t.ary in
    for i = 0 to t.nxt - 1 do
      accum := f !accum i ary.(i)
    done;
    !accum
end
