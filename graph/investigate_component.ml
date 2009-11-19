open Batteries
open Unix

let format_time_option = function
  | Some t -> 
      let ts = gmtime t in
	Printf.sprintf "%d.%d.%d" ts.tm_mday ts.tm_mon (1900 + ts.tm_year)
  | None -> "void"

let print_key_records l =
  let print r =
    let (keyid, puid, ctime, exptime) = r in
    let ctime_string = format_time_option ctime in
    let exptime_string = format_time_option exptime in
    let s = Printf.sprintf 
      "%s %s %s %s" 
      keyid puid ctime_string exptime_string
    in
      print_endline s
  in
    List.iter print l

let get_key_records dbh keyids =
  let keyids = List.map Misc.keyid_to_string keyids in
  PGSQL(dbh) "select keyid, puid, ctime, exptime from keys where keyid in $@keyids"

let _ =
  if Array.length Sys.argv <> 4 then (
    print_endline "investigate_components vertex.sexp edge.sexp min_size";
    exit 1)

let main () =
  let minsize = int_of_string Sys.argv.(3) in
    print_endline ("investigate smaller components down to size " ^ Sys.argv.(3));
    let dbh = PGOCaml.connect ~database:"wot" () in
    let (_, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1) Sys.argv.(2) in
    let rec loop l =
      match l with
	| hd :: tl when (List.length hd) > 30000 -> 
	    loop tl
	| hd :: tl when (List.length hd) > minsize -> 
	    let records = get_key_records dbh hd in
	      Printf.printf "\nmembers of scc %d\n" (List.length hd);
	      print_key_records records;
	      loop tl
	| hd :: tl -> ()
	| [] -> ()
    in
      loop scc_list_sorted
      
let _ =
  try main () with
    | e -> prerr_endline (Printexc.to_string e)