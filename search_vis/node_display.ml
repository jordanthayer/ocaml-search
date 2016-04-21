(** Draws info about the current node *)


open StdLabels
open Gaux
open Gtk
open GObj
open GMain

let node_window = ignore(GtkMain.Main.init());
  GWindow.window ~border_width:10 ~title:"Current Node"
    ~deletable:false ~width:200 ~height:300 ()

let val_table = GPack.table ~columns:2 ~rows:11 ~row_spacings:10
  ~col_spacings:50 ~packing:node_window#add ()


let data_lbls =
  ignore(GMisc.label ~text:"Expanding"
	   ~packing:(val_table#attach ~left:0 ~top:0) ());
  ignore(GMisc.label ~text:"g" ~packing:(val_table#attach ~left:0 ~top:2) ());
  ignore(GMisc.label ~text:"depth"
	   ~packing:(val_table#attach ~left:0 ~top:3) ());
  ignore(GMisc.label ~text:"h" ~packing:(val_table#attach ~left:0 ~top:4) ());
  ignore(GMisc.label ~text:"h*" ~packing:(val_table#attach ~left:0 ~top:5) ());
  ignore(GMisc.label ~text:"d" ~packing:(val_table#attach ~left:0 ~top:6) ());
  ignore(GMisc.label ~text:"d*" ~packing:(val_table#attach ~left:0 ~top:7) ());
  ignore(GMisc.label ~text:"f" ~packing:(val_table#attach ~left:0 ~top:8) ());
  ignore(GMisc.label ~text:"f*" ~packing:(val_table#attach ~left:0 ~top:9) ());
  ignore(GMisc.label ~text:"cost"
	   ~packing:(val_table#attach ~left:0 ~top:10) ());
  [|GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:0) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:2) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:3) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:4) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:5) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:6) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:7) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:8) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:9) ();
    GMisc.label ~justify:`RIGHT ~text:"" ~packing:(val_table#attach ~left:1 ~top:10) ();|]


let print_node key_printer exp getters step =
  (** Renders the node to standard error *)
  Verb.pe Verb.toplvl "%s\n" (key_printer exp.Recorded_run.key);
  for i = 0 to ((Array.length getters) - 1)
  do
    let fn,nm = getters.(i) in
      match fn exp with
	  Render.Scaler v ->
	    Verb.pe Verb.toplvl "%s" (Wrutils.str ("%s:\t\t%f\n") nm v)
	| _ -> ()
  done;
  Verb.pe Verb.toplvl "\n%!"


let display_node key_printer exp getters step =
  (** Renders the currently being expanded node to the graphics area *)
  data_lbls.(0)#set_text (key_printer exp.Recorded_run.key);
  for i = 0 to ((Array.length getters) - 1)
  do
    (let fn,nm = getters.(i) in
       match fn exp step with
	   Render.Scaler v ->
	     (data_lbls.(i+1)#set_text (Wrutils.str "%06.2f" v);
	      data_lbls.(i+1)#set_justify `RIGHT)
	 | Render.Fixed (r,g,b) -> ())
  done

(* EOF *)
