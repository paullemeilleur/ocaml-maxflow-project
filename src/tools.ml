(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr: 'a graph) =
  n_fold gr (fun gr' id -> new_node gr' id) empty_graph


let gmap (gr: 'a graph) (f: 'a -> 'b) = 
  let new_graph = clone_nodes gr in
  e_fold gr (fun g arc -> new_arc g {src=arc.src; tgt=arc.tgt; lbl = (f arc.lbl)}) new_graph

(* Replace _gr and _f by gr and f when you start writing the real function. *)

let add_arc (g : int graph) (id1 : id) (id2 : id) (n : int) =
  new_arc g {src=id1; tgt=id2; lbl=n}

