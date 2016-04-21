(** Simple connection and communication.

    @author eaburns
    @since 2010-03-05
*)

type t = {
  to_server : out_channel;
  from_server : in_channel;
}


let echo = ref false
  (** [echo] is set to true if the sending and receiving data should
      be echoed. *)


let inet_addr_of_string str =
  (** [sock_addr_of_string str] gets an inet_addr from a string. *)
  let host =
    try Unix.gethostbyaddr (Unix.inet_addr_of_string str)
    with Failure _ ->
      try Unix.gethostbyname str
      with Failure _ -> failwith ("No host " ^ str)
  in
    if Array.length (host.Unix.h_addr_list) = 0
    then failwith ("No addresses on host " ^ str)
    else host.Unix.h_addr_list.(0)


let create server port =
  (** [create server port] creates a new client connection to the
      given server. *)
  let addr = inet_addr_of_string server in
  let from_server, to_server =
    Unix.open_connection (Unix.ADDR_INET (addr, port))
  in
    Printf.printf "Connection is open\n%!";
    {
      to_server = to_server;
      from_server = from_server;
    }


let close con =
  (** [close con] closes the given connection. *)
  Unix.shutdown_connection con.from_server


let send_string con data =
  (** [send_string con data] sends a string to the server. *)
  if !echo then Printf.printf "send_string: [%s]\n" data;
  output_string con.to_server data;
  flush con.to_server


let receive_string =
  (** [receive_string con] receives a line of data from the
      server. *)
  let buf_size = 4096 in
  let str_buf = String.create buf_size in
    (fun con ->
       let nread = input con.from_server str_buf 0 buf_size in
       let str = String.sub str_buf 0 nread in
	 if !echo then Printf.printf "receive_string: [%s]\n" str;
	 str)
