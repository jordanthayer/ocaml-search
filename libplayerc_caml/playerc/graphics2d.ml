(** For drawing 2D graphics on Stage

    @author Kevin Rose
    @since 2010-06-09
*)

type t

type color = {
  alpha : int;
  red   : int;
  green : int;
  blue  : int;
}

type point = {
  px : float;
  py : float;
}

external create : Client.t -> int -> t = "graphics2d_create_stub"

external subscribe : t -> Common.accessmode -> unit = "graphics2d_subscribe_stub"

external unsubscribe : t -> unit = "graphics2d_unsubscribe_stub"

external setcolor : t -> color -> unit = "graphics2d_setcolor_stub"

external draw_points : t -> point array -> int -> unit = "graphics2d_draw_points_stub"

external draw_polyline : t -> point array -> int -> unit = "graphics2d_draw_polyline_stub"

external draw_polygon : t -> point array -> int -> int -> color -> unit = "graphics2d_draw_polygon_stub"

external clear : t -> unit = "graphics2d_clear_stub"
