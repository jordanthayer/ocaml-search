(** A playerc2d proxy interface.

    @author eaburns
    @since 2010-01-30
*)

type t

external create : Client.t -> int -> t = "position2d_create_stub"

external destroy : t -> unit = "position2d_destroy_stub"
(* This is called automatically by the garbage collector.  The
   function call is just here for library debugging purposes. *)

external subscribe :
  t -> Common.accessmode -> unit = "position2d_subscribe_stub"

external unsubscribe : t -> unit = "position2d_unsubscribe_stub"

external enable : t -> bool -> unit = "position2d_enable_stub"

external device : t -> Device.t = "position2d_device_stub"

type position = {
  px : float;
  (* meters *)
  py : float;
  (* meters *)
  pa : float;
  (* radians *)
  vx : float;
  (* meters *)
  vy : float;
  (* meters *)
  va : float;
  (* radians *)
}

external position : t -> position = "position2d_position_stub"

external set_cmd_pose :
  t -> float -> float -> float -> int -> unit = "position2d_set_cmd_pose_stub"

external set_cmd_vel :
  t -> float -> float -> float -> int -> unit = "position2d_set_cmd_vel_stub"

external set_odom :
  t -> float -> float -> float -> unit = "position2d_set_odom_stub"

external get_size : t -> float * float = "position2d_get_size_stub"
(* this returns the size of the robot (width , height) in meters *)
