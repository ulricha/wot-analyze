open Batteries
open Graph
open Graph_misc
open Misc

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val nb_vertex : t -> int
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

module Make(G : G) = struct
  module H = Hashtbl.Make(G.V)

  type vertex_info = {
    mutable d : int;
    mutable sigma : float;
    mutable pred : G.V.t Ref_list.t;
    mutable delta : float
  }

  let lookup_vertex_info_or_create tbl v =
    try
      H.find tbl v
    with Not_found ->
      let i = { d = (-1); sigma = 0.0; pred = Ref_list.empty (); delta = 0.0 } in
	H.add tbl v i;
	i

  (* update betweeness centrality values as part of the round
     function. stack is the stack accumulated during BFS traversal; s
     is the start vertex of the round and n = |V| *)
  let update_betweeness b_tbl lookup stack s n =
    while not (Stack.is_empty stack) do
      let w = Stack.pop stack in
      let w_info = lookup w in
      let compute_delta v =
	let v_info = lookup v in
	let div = v_info.sigma /. w_info.sigma in
	let t = v_info.delta +. div *. (1.0 +. w_info.delta) in
	  v_info.delta <- t
      in
	Enum.iter compute_delta (Ref_list.backwards w_info.pred);
	if not (w = s) then
	  let tbl_add_or_create key increment =
	    if H.mem b_tbl key then
	      let prev_val = H.find b_tbl key in
		H.replace b_tbl key (prev_val +. increment)
	    else
	      H.add b_tbl key increment
	  in
	    tbl_add_or_create w (w_info.delta)
    done

  (* compute the round function of the Betweeness Centrality algorithm: do a 
     BFS traversal beginning on s, compute the vertex dependencies and use them
     to update the betweeness values in b_tbl. *)
  let betweeness_round g s b_tbl =
    let stack = Stack.create () in
    let n = G.nb_vertex g in
    let info_tbl = H.create n in
    let lookup = lookup_vertex_info_or_create info_tbl in
    let q = Queue.create () in
    let s_info = lookup s in
      s_info.sigma <- 1.0;
      s_info.d <- 0;
      Queue.add s q;
      while not (Queue.is_empty q) do
	let v = Queue.take q in
	let v_info = lookup v in
	let push w =
	  let w_info = lookup w in
	  if w_info.d < 0 then
	    begin
	      w_info.d <- (v_info.d + 1);
	      Queue.add w q;
	    end;
	    if w_info.d = v_info.d + 1 then
	      begin
		w_info.sigma <- w_info.sigma +. v_info.sigma;
		Ref_list.push w_info.pred v
	      end
	in
	  Stack.push v stack;
	  G.iter_succ push g v
      done;
      update_betweeness b_tbl lookup stack s n 

  (* iteratively compute the betweeness centrality values for the graph g. *)
  let betweeness_centrality_iterative g bench =
    let n = G.nb_vertex g in
    let b_tbl = H.create n in
    let f v = 
      bench ();
      betweeness_round g v b_tbl
    in
      G.iter_vertex f g;
      b_tbl
end