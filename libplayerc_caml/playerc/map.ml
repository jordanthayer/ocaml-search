(** Map

    @author Kevin Rose
    @since 2010-06-09
*)

type t

external create : Client.t -> int -> t = "map_create_stub"

external subscribe : t -> Common.accessmode -> unit = "map_subscribe_stub"

external unsubscribe : t -> unit = "map_unsubscribe_stub"

external get_map : t -> unit = "map_get_map_stub"

external get_resolution : t -> float = "map_get_resolution_stub"

external get_width : t -> int = "map_get_width_stub"

external get_height : t -> int = "map_get_height_stub"

external get_origin : t -> float * float = "map_get_origin_stub"

external get_cells : t -> int array = "map_get_cells_stub"

(* NOT IMPLEMENTED *)
(* external get_vector : t -> unit = "map_get_vector_stub" *)

(* EOF *)
