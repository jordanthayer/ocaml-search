(** A playerc simulation proxy interface.

    @author Kevin Rose
    @since 2010-08-04
*)

type t

external create : Client.t -> int -> t = "simulation_create_stub"

external destroy : t -> unit = "simulation_destroy_stub"
(* This is called automatically by the garbage collector.  The
   function call is just here for library debugging purposes. *)

external subscribe :
  t -> Common.accessmode -> unit = "simulation_subscribe_stub"

external unsubscribe : t -> unit = "simulation_unsubscribe_stub"

external set_pose2d : t -> string -> float -> float -> float -> unit = "simulation_set_pose2d_stub"

external get_pose2d : t -> string -> float * float * float = "simulation_get_pose2d_stub"

(* external set_pose3d : t -> string -> float -> float -> float -> float -> float -> float -> unit = "simulation_set_pose3d_stub" *)

external get_pose3d : t -> string -> float * float * float * float * float * float * float = "simulation_get_pose3d_stub"

external pause : t -> unit = "simulation_pause_stub"                                                                                               

(* get_property and set_property not implemented *)


(* EOF *)
