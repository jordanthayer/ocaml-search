(* hhat calculations *)
open Recorded_run


(* Single value models *)
let d_hhat wts =
  let wd = List.nth wts 0 in
  (fun exp -> 
     (get_h exp) +. wd *. (get_d exp))


let h_hhat wts =
  let wh = List.nth wts 0 in
    (fun exp -> 
       (wh +. 1.) *. (get_h exp))

let g_hhat wts =
  let wg = List.nth wts 0 in
    (fun exp ->
       wg *. exp.g +. (get_h exp))

let dep_hhat wts =
  let wD = List.nth wts 0 in
    (fun exp ->
       wD *. exp.depth +. (get_h exp))

(*******************************************************)

(* Linear combinations - 2 items *)

let hd_hhat wts =
  let wh = List.nth wts 0
  and wd = List.nth wts 1 in
  (fun exp -> 
     let ret =
       (wh +. 1.) *. (get_h exp) +. 
	 wd *. (get_d exp) in
       assert (ret <> infinity);
       ret)

let hg_hhat wts =
  let wh = List.nth wts 0
  and wg = List.nth wts 1 in
    (fun exp ->
       (wh +. 1.) *. (get_h exp) +.
	 wg *. exp.g)

let hD_hhat wts =
  let wh = List.nth wts 0
  and wD = List.nth wts 1 in
    (fun exp ->
       (wh +. 1.) *. (get_h exp) +.
	 wD *. exp.depth)

let dg_hhat wts =
  let wd = List.nth wts 0
  and wg = List.nth wts 1 in
    (fun exp ->
       wd *. (get_d exp) +.
	 wg *. exp.g +.
	 (get_h exp))

let dD_hhat wts =
  let wd = List.nth wts 0
  and wD = List.nth wts 1 in
    (fun exp ->
       wd *. (get_d exp) +.
	 wD *. exp.depth +.
	 (get_h exp))

let gD_hhat wts =
  let wg = List.nth wts 0
  and wD = List.nth wts 1 in
    (fun exp ->
       wg *. exp.g +.
	 wD *. exp.depth +.
	 (get_h exp))


(* Linear combinations - 3 items *)
let hdg_hhat wts =
  let wh = List.nth wts 0
  and wd = List.nth wts 1
  and wg = List.nth wts 2 in
    (fun exp -> 
       wg *. exp.g +. 
	 (wh +. 1.) *. (get_h exp) +. 
	 wd *. (get_d exp))

let hdD_hhat wts =
  let wh = List.nth wts 0
  and wd = List.nth wts 1
  and wD = List.nth wts 2 in
    (fun exp -> 
       wD  *. exp.depth +. 
	 (wh +. 1.) *. (get_h exp) +. 
	 wd *. (get_d exp))

let dgD_hhat wts =
  let wd = List.nth wts 0
  and wg = List.nth wts 1
  and wD = List.nth wts 2 in
    (fun exp -> 
       wg  *. exp.g +. 
	 wD *. exp.depth +. 
	 wd *. (get_d exp) +. (get_h exp))

let hgD_hhat wts =
  let wh = List.nth wts 0
  and wg = List.nth wts 1
  and wD = List.nth wts 2 in
    (fun exp -> 
       wg  *. exp.g +. 
	 (wh +. 1.) *. (get_h exp) +. 
	 wD *. exp.depth)


(* Line combination - all items *)
let hdgD_hhat wts =
  let wh = List.nth wts 0
  and wd = List.nth wts 1
  and wg = List.nth wts 2
  and wD = List.nth wts 3 in
  (fun exp -> 
     (wg -. 1.) *. exp.g +. 
       (wh +. 1.) *. (get_h exp) +. 
       wd *. (get_d exp) +. 
       wD *. exp.depth)

(* More complicated models *)

(* EOF *)
