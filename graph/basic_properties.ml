open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph
open Network_statistics

module Wot_components = Components.Make(G)
module Statistics = Network_statistics.Make(G)

(* creating a new siginfo table for the new graph from the original one 
   is not necesarry because the original one can still be used *)
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
    write_distribution_to_file (Map.IntMap.enum size_map) "component_size.plot"; 
    print_endline ""

let scc_list_to_graph_list scc_list original_graph original_siginfo =
  List.map (fun scc -> graph_from_node_list scc original_graph) scc_list

(* mscc = maximum strongly connected component *)
let () =
  if (Array.length Sys.argv) <> 3 then
    begin
      print_endline "usage: basic_properties vertex.mar edges.mar";
      exit (-1)
    end
  else
    begin
      print_endline "compute basic properties of wot graph";
      let vertex_fname = Sys.argv.(1) in
      let edge_fname = Sys.argv.(2) in
      let l = fun () -> load_storeable_graph_from_files vertex_fname edge_fname in
      let storeable_g = time_evaluation l "load_storeable_graph" in
      let c = fun () -> graph_from_storeable_graph storeable_g in
      let g = time_evaluation c "graph_from_storeable_graph" in
      let scc_list = time_evaluation (fun () -> Wot_components.scc_list g) "scc_list" in
      let mscc = largest_component_as_graph scc_list g in
	Statistics.basic_network_statistics g "complete_graph";
	overall_component_properties scc_list;
	Statistics.complete_statistics mscc "mscc";
    end
