(************************************************************************)
(* This file is part of SKS.  SKS is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA *)
(***********************************************************************)

(** simple command-line tool for sending actions directly to 
  sks_db and sks_recon processes 
*)

open StdLabels
open MoreLabels
open Printf
open Common
open Packet
open DbMessages
module Unix = UnixLabels
module PTree = PrefixTree
module Map = PMap.Map

let fail reason =
  printf "%s\n" reason;
  flush stdout;
  exit (-1)

let send_dbmsg msg = 
  let ctr = ref 0 in
  let s = Unix.socket 
	    ~domain:Unix.PF_UNIX 
	    ~kind:Unix.SOCK_STREAM 
	    ~protocol:0 in
  protect ~f:(fun () ->
		Unix.connect s ~addr:db_command_addr;
		let cin = Channel.sys_in_from_fd s in
		let cout = Channel.sys_out_from_fd s in
		marshal cout msg;
		let reply = (unmarshal cin).msg in
		reply
	     )
    ~finally:(fun () -> Unix.close s)


let drop () = 	  
  match !Settings.anonlist with
    | [hash_string] -> 
	if String.length hash_string <> 32 then 
	  fail "hash should be exactly 32 characters long";
	let hash = KeyHash.dehexify hash_string in
	ignore (send_dbmsg (DeleteKey hash))	  
    | _ -> fail "Wrong number of arguments: must specify exactly 1 hash"
	  
