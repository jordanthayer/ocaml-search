(* $Id: udp.ml,v 1.1 2003/12/01 21:22:14 ruml Exp ruml $

   communicating over UDP sockets
*)


type connected = Unix.file_descr
type unconnected = Unix.file_descr


let make port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
    Unix.bind sock (Wrsys.sock_addr (Unix.gethostname ()) port);
    sock


let sendto sock addr s =
  let n = String.length s in
  let sent = Unix.sendto sock s 0 n [] addr in
    if sent <> n then
      failwith (Wrutils.str "Udp.sendto sent only %d bytes not %d" sent n)


let recvfrom sock len =
  let s = String.make len '-' in
  let len,addr = Unix.recvfrom sock s 0 len [] in
    (String.sub s 0 len),addr


let input_waiting sock =
  Wrsys.fd_waiting sock


let shut sock =
  (* Unix.shutdown sock Unix.SHUTDOWN_ALL *)
  Unix.close sock


let make_connected port addr =
  let s = make port in
    Unix.connect s addr;
    s


let send sock s =
  let n = String.length s in
  let sent = Unix.send sock s 0 n [] in
    if sent <> n then
      failwith (Wrutils.str "Udp.send sent only %d bytes not %d" sent n)


let recv sock len =
  let s = String.make len '-' in
  let len = Unix.recv sock s 0 len [] in
    String.sub s 0 len


let in_waiting sock =
  Wrsys.fd_waiting sock


let shut_connected sock =
  (* Unix.shutdown sock Unix.SHUTDOWN_ALL; *)
  Unix.close sock


(* EOF *)
