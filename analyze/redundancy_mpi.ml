open Batteries

open Printf
open Graph
open Misc
open Graph_misc
open Ekey
open Wot_graph

module C = Component_helpers.Make(G)

module Mpi_redundancy = Mpi_framework.Make(Redundancy.Redundancy_job)

(* mscc = maximum strongly connected component *)
let _ =
  if (Array.length Sys.argv) <> 2 then
    begin
      print_endline "usage: redundancy_mpi edge_filename";
      exit (-1)
    end

let main () =
  Random.init 2;
  let rank = Mpi.comm_rank Mpi.comm_world in
  let (g, scc_list_sorted) = Component_helpers.load_scc_list Sys.argv.(1) in
  let mscc_nodelist = List.hd scc_list_sorted in
  let mscc = C.graph_from_node_list mscc_nodelist g in
    if rank = 0 then
      begin
	print_endline "server started";
	let res = Mpi_redundancy.server 0 mscc in
	  print_endline "server finished";
	  (* let fname = sprintf "scc-%d_%d_maxflow_median.values" (G.nb_vertex mscc) 0 in
	  let proj r = 
	    let s = r.Redundancy.maxflow in
	      s.Redundancy.median
	  in *)
	    (* write_int_values_to_file (List.enum (List.map proj res)) fname *)
	    ignore res
      end
    else
      begin
	printf "worker %d started\n" rank;
	flush stdout;
	Mpi_redundancy.worker [mscc]
      end;
    Mpi.barrier Mpi.comm_world;
    if rank = 0 then
      print_endline "computation finished"

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
