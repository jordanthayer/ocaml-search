(** Dealing with lengths in different units.

    @author eaburns
    @since 2010-05-05
*)

open Printf

let cm_per_in = 2.54
let px_per_in = 96.
let pt_per_in = 72.
let px_per_cm = px_per_in /. cm_per_in
let pt_per_cm = pt_per_in /. cm_per_in
let px_per_pt = px_per_in /. pt_per_in

type t =
  | In of float
  | Cm of float
  | Px of int
  | Pt of float

type units =
  | Inches
  | Centimeters
  | Pixels
  | Points

let as_in = function
  | In i -> i
  | Cm c -> c /. cm_per_in
  | Px p -> (float p) /. px_per_in
  | Pt p -> p /. pt_per_in

let as_cm = function
  | In i -> i *. cm_per_in
  | Cm c -> c
  | Px p -> (float p) /. px_per_cm
  | Pt p -> p /. pt_per_cm

let as_px_float = function
  | In i -> i *. px_per_in
  | Cm c -> c *. px_per_cm
  | Px p -> (float p)
  | Pt p -> p *. px_per_pt

let as_px l = truncate (as_px_float l)

let as_pt = function
  | In i -> i *. pt_per_in
  | Cm c -> c *. pt_per_cm
  | Px p -> (float p) /. px_per_pt
  | Pt p -> p


let to_string = function
  | In i -> sprintf "%f in" i
  | Cm c -> sprintf "%f cm" c
  | Px p -> sprintf "%d px" p
  | Pt p -> sprintf "%f pt" p


let of_string str =
  let strs = Str.split (Str.regexp " ") str in
    if List.length strs > 2
    then failwith (sprintf "Bad parse of length %s" str)
    else (match strs with
	      [vl;unit] -> (match unit with
			      | "in" -> In (float_of_string vl)
			      | "cm" -> Cm (float_of_string vl)
			      | "px" -> Px (int_of_string vl)
			      | "pt" -> Pt (float_of_string vl)
			      | _ -> failwith
				  (sprintf "Bad parse of length %s" str))
	    | _ -> failwith (sprintf "Bad parse of length %s" str))


let convert t unit =
  match unit with
    | Inches -> as_in t
    | Centimeters -> as_cm t
    | Pixels -> as_px_float t
    | Points -> as_pt t


let get_type t =
  match t with
    | In _ -> Inches
    | Cm _ -> Centimeters
    | Px _ -> Pixels
    | Pt _ -> Points

(* EOF *)
