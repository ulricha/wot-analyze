open Batteries
open Printf
open Graph

open Ekey
open Misc

module V = struct
  type t = epki
  let compare = (fun k1 k2 -> compare k1.key_keyid k2.key_keyid)
  let hash = (fun k1 -> hash_keyid k1.key_keyid)
  let equal = (fun k1 k2 -> k1.key_keyid = k2.key_keyid)
end

module E = struct
  type t = string * string
  let compare = (fun (v1, v2) (v3, v4) ->
		   compare (v1 ^ v2) (v3 ^ v4))
end

module G = Imperative.Digraph.ConcreteBidirectional(V)

module Key_map = Map.Make(V)

let load_storeable_graph_from_files vertex_filename edge_filename =
  let vertices = List.map epki_of_sexp (load_sexps vertex_filename) in
  let edges = List.map sig_list_per_signee_of_sexp (load_rev_sexps edge_filename) in
    (vertices, edges)

let graph_to_storeable_graph g edgeinfo_tbl=
  let vertex_list = G.fold_vertex (fun v l -> v :: l) g [] in
  let edge_map = G.fold_edges
    (fun signer signee map -> 
       let siginfo = Hashtbl.find edgeinfo_tbl (signer, signee) in
       if Key_map.mem signee map then
	 let old_entry = Key_map.find signee map in
	 let (_, l) = old_entry in
	 let new_entry = (signee, (signer, siginfo) :: l) in
	   Key_map.add signee new_entry map
       else
	 let entry = (signee, [(signer, siginfo)]) in
	 Key_map.add signee entry map
    )
    g
    Key_map.empty
  in
    (vertex_list, List.of_enum (Key_map.values edge_map))
	 
let add_edges g vertex_tbl edge_list =
  let siginfos = Hashtbl.create 700000 in
  let add_edges_from_one_vertex edges =
    let (signee, siglist) = edges in
      try
	let signee_vertex = Hashtbl.find vertex_tbl signee in
	  List.iter 
	    (fun s ->
	       let (signer, siginfo) = s in
		 try
		   let signer_vertex = Hashtbl.find vertex_tbl signer in
		     Hashtbl.add siginfos (signer, signee) siginfo;
		     G.add_edge g signer_vertex signee_vertex
		 with Not_found -> print_endline ("key not in hashtbl: " ^ (keyid_to_string signer)))
	    siglist
      with Not_found -> ()
  in
    List.iter (fun edges -> add_edges_from_one_vertex edges) edge_list

let graph_from_storeable_graph storeable_graph =
  let vertex_tbl = Hashtbl.create 320000 in
  let (vertices, edges) = storeable_graph in
  let g = G.create ~size:300000 () in
    List.iter (fun v -> G.add_vertex g v; Hashtbl.add vertex_tbl v.key_keyid v) vertices;
    add_edges g vertex_tbl edges;
    g
