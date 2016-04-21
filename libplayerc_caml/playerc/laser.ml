(** A laser range finder.

    @author eaburns
    @since 2010-02-02
*)

type t

type config = {
  (* If the order of these values changes then it must change in
     laser_stubs.c too. *)
  min_angle : float;
  max_angle : float;
  resolution : float;
  range_res : float;
  intensity : float;
  (* [intensity] is really just needs to be an 8-bit integer, but
     having all fields of this record makes it a lot more memory
     efficient because no field will be boxed. *)
 scanning_frequency : float;
}

external create : Client.t -> int -> t = "laser_create_stub"

external subscribe : t -> Common.accessmode -> unit = "laser_subscribe_stub"

external unsubscribe : t -> unit = "laser_unsubscribe_stub"

external set_config : t -> config -> unit = "laser_set_config_stub"

external get_config : t -> config = "laser_get_config_stub"

external points : t -> (float array * float array) = "laser_points_stub"

external scan : t -> (float array * float array) = "laser_scan_stub"

external intensities : t -> int array = "laser_intensities_stub"

external max_range : t -> float = "laser_max_range_stub"

external device : t -> Device.t = "laser_device_stub"
