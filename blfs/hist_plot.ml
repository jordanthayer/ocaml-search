(** Plots the histogram.

    This is a separate module becasue the histogram is now in the
    structs system which can't use Ps_plot (circular dependency).

    @author eaburns
    @since 2009-10-06
*)

open Hist

let plot ?(title = "") path t =
(*
  let data, n, l = get_plot_data t in
    Ps_plot.line path [data, (Wrutils.str "%d %s" n l)]
      ~zero_line:true title "Value" "Weight"
*)
  ()
