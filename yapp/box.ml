(* $Id: problem.ml,v 1.2 2003/07/07 21:20:50 ruml Exp ruml $
   
   boxes
*)


type t = {
  llx : float;
  lly : float;
  urx : float;
  ury : float;
}


let pts_of_in x =
  x *. 72.
    
let in_of_pts x =
  x /. 72.


let letter_width = pts_of_in 8.5

let letter_length = pts_of_in 11.


let of_coords llx lly urx ury =
  { llx = llx; lly = lly;
    urx = urx; ury = ury; }


let of_size llx lly width height =
  { llx = llx; lly = lly;
    urx = llx +. width;
    ury = lly +. height; }


(* EOF *)
