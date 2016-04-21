(** Functions for playerc clients.

    Each function here corresponds (nearly exactly) to a playerc_XXX
    function from libplayerc.

    @author eaburns
    @since 2010-01-28
*)

type t

external create : Mclient.t option -> string -> int -> t = "client_create_stub"

external set_transport :
  t -> Common.transport -> unit = "client_set_transport_stub"

external connect : t -> unit = "client_connect_stub"

external disconnect : t -> unit = "client_disconnect_stub"

external disconnect_retry : t -> unit = "client_disconnect_retry_stub"

external datamode : t -> Common.datamode -> unit = "client_datamode_stub"

external requestdata : t -> unit = "client_requestdata_stub"

external peek : t -> int -> unit = "client_peek_stub"

external internal_peek : t -> int -> unit = "client_internal_peek_stub"

external read : t -> unit = "client_read_stub"

external read_nonblock : t -> unit = "client_read_nonblock_stub"

external set_request_timeout :
  t -> int -> unit = "client_set_request_timeout_stub"

external set_retry_limit : t -> int -> unit = "client_set_retry_limit_stub"

external set_retry_time : t -> float -> unit = "client_set_retry_time_stub"
