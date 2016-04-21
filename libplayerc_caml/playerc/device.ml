(** Accessing device information.

    @author eaburns
    @since 2010-01-30
*)

type t

type address = {
  host : int;
  robot : int;
  interf : int;
  index : int;
}

external address : t -> address = "device_address_stub"

external drivername : t -> string = "device_drivername_stub"

external subscribed : t -> bool = "device_subscribed_stub"

external datatime : t -> float = "device_datatime_stub"

external lasttime : t -> float = "device_lasttime_stub"
