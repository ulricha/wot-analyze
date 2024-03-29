open Batteries
open Printf
open Graph
open Misc
open Graph_misc
open Wot_graph

module Make(G : Sig.I) = struct
  module C = Components.Make(G)

  let graph_from_node_list nodes original_graph =
    let g = G.create () in
      List.iter (fun v -> G.add_vertex g v) nodes;
      let add_edge v1 v2 =
	if G.mem_vertex g v1 && G.mem_vertex g v2 then 
	  G.add_edge g v1 v2
      in
	G.iter_edges add_edge original_graph;
	g

  let largest_component_as_graph scc_list original_graph =
    let compare_scc_length = 
      compare_reverse (fun l1 l2 -> compare (List.length l1) (List.length l2)) in
    let sorted_list = List.sort ~cmp:compare_scc_length scc_list in
    let largest = List.hd sorted_list in
      graph_from_node_list largest original_graph

  let overall_component_properties scc_list =
    let l = List.map (fun scc -> List.length scc) scc_list in
    let size_map = List.fold_left 
      (fun m s ->
	 try 
	   Map.IntMap.add s ((Map.IntMap.find s m) + 1) m
	 with Not_found -> Map.IntMap.add s 1 m)
      Map.IntMap.empty
      l
    in
    let cmp_pair = compare_reverse (fun (s1, nr1) (s2, nr2) -> compare s1 s2) in
    let size_number_list = List.sort ~cmp:cmp_pair (List.of_enum (Map.IntMap.enum size_map)) in
      printf "largest component %d\n" (fst (List.hd size_number_list));
      printf "number of components %d\n" (List.length scc_list);
      List.iter (fun (size, number) -> printf "%d: %d " size number) size_number_list;
      write_distribution_to_file "%d %d\n" (Map.IntMap.enum size_map) "component_size.plot"; 
      write_int_values_to_file (List.enum l) "component_size_values.dat";
      print_endline ""

  let scc_list_to_graph_list scc_list original_graph original_siginfo =
    List.map (fun scc -> graph_from_node_list scc original_graph) scc_list

  let scc_list g = C.scc_list g

  let graph_list g = scc_list_to_graph_list (scc_list g)

end

module C = Make(G)

let load_scc_list e_fname =
  let g = load_graph_from_file e_fname in
  let scc_list = C.scc_list g in
  let scc_list_sorted = list_list_sort_reverse scc_list in
    (g, scc_list_sorted)

let load_mscc e_fname =
  let (g, scc_list_sorted) = load_scc_list e_fname in
    (g, List.hd scc_list_sorted)

let component_ids scc_list =
  let scc_list = List.filter (fun c -> (List.length c) > 1) scc_list in
    List.mapi (fun i c -> (c, i)) scc_list

let canonical_component_name keyids =
  let sorted = List.sort ~cmp:Pervasives.compare keyids in
    Digest.to_hex (Digest.string (String.concat "" sorted))
    
