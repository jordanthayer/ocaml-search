(** The plot hierarchy.

    @author eaburns
    @since 2010-04-23
*)

open Geometry
open Drawing
open Verbosity

(** The default font used by SPT plots.  Make sure this font is very
    common (preferably a postscript or PDF required font). *)
let default_font = "New Century Schoolbook"


(** The default style for the text associated with tick marks on a
    numeric axis. *)
let default_tick_style =
  {
    text_font = default_font;
    text_size = Length.Pt 10.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


(** The default style for legend text. *)
let default_legend_style =
  {
    text_font = default_font;
    text_size = Length.Pt 12.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }

(** The default style for the x and y axis labels and the title
    text. *)
let default_label_style =
  {
    text_font = default_font;
    text_size = Length.Pt 12.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


(** Padding around text *)
let text_padding = Length.Pt 2.


(** The default plot width. *)
let default_width = Length.In 3.


(** The default plot height. *)
let default_height = Length.In 3.


let set_label_size lbl_sty text_size =
  { lbl_sty with text_size = text_size }

class type plot_type =
object

  method width : Length.t

  method height : Length.t


  (** [set_size ~w ~h] resizes the plot. *)
  method set_size : w:Length.t -> h:Length.t -> unit

  (** [display] opens a lablgtk window showing the plot. *)
  method display : unit


  method output : string -> unit

  (** [draw ctx] displays the plot to the given drawing context with
      the given aspect ratio.  It is up to the caller to scale this
      to the appropriate width/height. *)
  method draw : context -> unit
end

(* EOF *)

(** [plot title] a plot has a method for drawing. *)
class virtual plot title =
object (self)

  val mutable width = default_width

  method width = width

  val mutable height = default_height

  method height = height


  (** [size ctx] gets the size of the plot in the units of the
      context. *)
  method private size ctx =
    (ctx.units width), (ctx.units height)


  (** [set_size ~w ~h] resizes the plot. *)
  method set_size ~w ~h =
    vprintf verb_debug "setting size to %s by %s\n"
      (Length.to_string w) (Length.to_string h);
    width <- w;
    height <- h


  method private title = match title with
    | Some t -> t
    | None -> "<no title>"


  (** [fill_background ctx] fill the background white. *)
  method private fill_background ctx =
    Drawing.fill_rectangle ctx ~color:white
      (rectangle 0. (ctx.units width) 0. (ctx.units height))


  (** [display] opens a lablgtk window showing the plot. *)
  method display =
    Spt_gtk.create_display self self#title


  (** [output filename] saves the plot to a filename.  The type is
      pulled from the name, so you must include an extension *)
  method output filename =
    vprintf verb_normal "outputting to %s\n" filename;
    Spt_cairo.save self filename


  (** [draw ctx] displays the plot to the given drawing context with
      the given aspect ratio.  It is up to the caller to scale this
      to the appropriate width/height. *)
  method virtual draw : context -> unit
end

(* EOF *)
