(** Action Listener for the visualization tool *)

type events =
  | StepForward
  | StepBack
  | CycleSelected
  | CycleContext
  | Kill
  | StoreImage
  | NotRecognized


let keyToEvent k =
  (** converts the keystroke [k] into the appropriate event.*)
    match k with
      | 'k' -> Kill
      | 'e' -> StepForward
      | 'a' -> StepBack
      | 'o' -> CycleContext
      | _ -> (Verb.pe Verb.always "%s" (Wrutils.str "|%c| not recognized\n%!" k);
	      NotRecognized)

(*
let get_event () =
  (** Returns the current keystroke as an event *)
  keyToEvent (Graphics.read_key())
*)
(* EOF *)
