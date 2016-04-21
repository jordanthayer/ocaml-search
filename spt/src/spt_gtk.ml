(** Holds all of the gtk display functionality for easy removal -
    Jordan*)

open Verbosity

let gtk_initialized = ref false
let dir = ref None

let _ =
  (* Need to set up gtk for use *)
  try
    ignore (GtkMain.Main.init());
    gtk_initialized := true;
  with _ -> vprintf verb_optional "Failed to initialize GTK, is X11 running?\n"

let init_size = 400 (* default window size *)

let file_dialog ~title ~callback () =
  let sel =
    GWindow.file_selection ~title ~modal:true ?filename:!dir () in
  ignore (sel#cancel_button#connect#clicked ~callback:sel#destroy);
  ignore (sel#ok_button#connect#clicked ~callback:
	    begin fun () ->
	      let name = sel#filename in
		dir := Some name;
		sel#destroy ();
		callback name
	    end);
  sel#show ()


let dimension_dialog ~title ~plot ~window () =
  let sel = GWindow.dialog ~title ~modal:true ~width:200 ~height:200 () in
  let vbox = sel#vbox in
  let h_dim = GBroken.text ~editable:true ~packing:vbox#add ~show:true ()
  and v_dim = GBroken.text ~editable:true ~packing:vbox#add ~show:true ()
  and accept = GButton.button ~label:"Resize" ~packing:sel#action_area#add ()in
    (* prefill v_dim, h_dim, and put the teeth into accept *)
    v_dim#insert (Length.to_string plot#height);
    h_dim#insert (Length.to_string plot#width);
    let resize () =
      try
	(let new_v = Length.of_string (v_dim#get_chars ~start:0
					 ~stop:v_dim#length)
	 and new_h = Length.of_string (h_dim#get_chars ~start:0
					 ~stop:h_dim#length) in
	   window#resize ~width:(Length.as_px new_h)
	     ~height:((Length.as_px new_v) + 40))
      with _ -> () in
      ignore (accept#connect#clicked ~callback:resize);
      sel#show ()


let save_dialog plot =
  file_dialog ~title:"Save"
    ~callback:(fun file ->
		 try
		   plot#output file
		 with Failure str ->
		   (let mwindow = GWindow.message_dialog
		      ~message:str
		      ~message_type:`ERROR
		      ~buttons:GWindow.Buttons.ok () in
		    ignore (mwindow#run ());
		      mwindow#destroy ()))()


let edit_dialog window plot =
  dimension_dialog ~title:"Edit Dimensions" ~plot ~window ()

let draw_plot_to_gtk_area plot area =
  (** [draw_plot plot area] draws the plot to a GTK drawing area. *)
  let { Gtk.width = width ; Gtk.height = height } = area#misc#allocation in
  let widthf = float width and heightf = float height in
  let gtk_ctx = Cairo_lablgtk.create area#misc#window in
  let ctx =
    Drawing.drawing_context gtk_ctx Length.as_px_float
      ~w:(Length.Px width) ~h:(Length.Px height)
  in
    plot#set_size ~w:(Length.Px width) ~h:(Length.Px height);
      Drawing.fill_rectangle ctx ~color:Drawing.white
	(Geometry.rectangle 0. widthf 0. heightf);
      (* Scale so that drawing can take place in a normalized aspect
	 ratio. *)
(*
      Drawing.scale ctx (widthf /. x_ratio) (heightf /. y_ratio);
*)
      plot#draw ctx


open GdkKeysyms

(** [create_display plot title] spawns a window with the given title
    showing the specified plot *)
let create_display plot title =
  if not !gtk_initialized
  then vprintf verb_critical "GTK not initialized\n"
  else begin
    let width = Length.as_px plot#width
    and height = Length.as_px plot#height in
    let w = GWindow.window ~title ~allow_grow:true ~allow_shrink:true () in
    let vbox = GPack.vbox ~packing:w#add () in
    let menu_bar = GMenu.menu_bar ~packing:vbox#pack () in
    let factory = new GMenu.factory ~accel_path:"<MLPLOT>/" menu_bar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu "File" in
    let edit_menu = factory#add_submenu "Edit" in
    let factory = new GMenu.factory ~accel_path:"<MLPLOT File>///" file_menu
      ~accel_group in
    let efactory = new GMenu.factory ~accel_path:"<MLPLOT Edit>///" edit_menu
      ~accel_group in
    let area = GMisc.drawing_area ~width ~height
      ~packing:(vbox#pack ~expand:true) () in
    let draw = draw_plot_to_gtk_area plot in
      w#resize ~width ~height:(height + 40);
      ignore (factory#add_item "Save as..." ~key:_S ~callback:
		(fun _ -> (save_dialog plot)));
      ignore (efactory#add_item "Dimensions" ~key:_D ~callback:
		(fun _ -> (edit_dialog w plot)));
      area#misc#realize ();
      ignore (factory#add_item "Quit" ~key:_Q ~callback:w#destroy);
      ignore (area#event#connect#expose (fun _ -> draw area; true));
      ignore (area#event#connect#button_press
		~callback:(fun ev ->
			     let button = GdkEvent.Button.button ev in
			       if button = 3 then begin
				 file_menu#popup ~button
				   ~time:(GdkEvent.Button.time ev); true
			       end else false));
      w#add_accel_group accel_group; (* turns on the button listening *)
      ignore (w#connect#destroy GMain.quit);
      ignore (draw area);
      w#show();
      GMain.main ()
  end

(* EOF *)
