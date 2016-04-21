(* $Id: udp.mli,v 1.1 2003/12/01 21:22:10 ruml Exp ruml $
   
   communicating over UDP sockets.  This is a packet-based semantics,
   rather than a connection-based stream/channel like TCP. Packet
   delivery is unreliable and possibly out of order.  Packets that are
   received are error-free (via CRC).

   UDP sockets can be used without setting up a connection.  One
   specifies a destination with each send and is told the sender with
   each receipt.  It is also possible to set a default address which
   is used for sending and for filtering incoming packets.

   These functions separate the two types of sockets into different
   types to prevent mixing calls.  This removes some flexibility (UDP
   sockets can in fact be switched between these states using
   additional calls) but presents a simpler interface.

   It would be nice to know what the minimum Maximum Transmission Unit is
   to some host, so that one could ensure a packet wouldn't be broken up
   along the way.
*)


type connected
type unconnected

  
val make : int -> unconnected
  (** Returns an unconnected UDP socket bound to the given local port *)

val sendto : unconnected -> Unix.sockaddr -> string -> unit
  (** UDP will fragment packet if necessary! *)

val recvfrom : unconnected -> int -> string * Unix.sockaddr
  (** Reads at most the first [len] bytes of the next packet at
    [sock].  The rest of the packet is discarded.  Also returns the addr
    of the sender. *)

val input_waiting : unconnected -> bool

val shut : unconnected -> unit

  
val make_connected : int -> Unix.sockaddr -> connected
  (** Makes a new UDP socket bound the given local port and connects
    it to [addr].  This sets the default destination for send and
    recv. *)

val send : connected -> string -> unit

val in_waiting : connected -> bool

val recv : connected -> int -> string
  (** Returns the first [len] characters at most.  Blocks if necessary. *)

val shut_connected : connected -> unit

  
(* EOF *)
