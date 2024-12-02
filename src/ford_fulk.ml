open Graph
open Tools

type flot =
  { 
    (* Capacité *)
    capa: int ;

    (* Flot *)  
    flot: int}

(* Création d'un graphe flot *)
let create_flot_graph (graph: int graph) = 
  gmap graph (fun lbl -> {capa = lbl; flot = 0});;

(* Ecart *)
let arcs_ecart (arc : flot arc) =
  let capa = arc.lbl.capa in
  let flot = arc.lbl.flot in
  let ecart = capa - flot in
  ({src = arc.src; tgt = arc.tgt; lbl = ecart},
   {src = arc.tgt; tgt = arc.src; lbl = flot});;



(*Graph ecart*)
let graph_ecart (graph : flot graph) =
  let new_graph = clone_nodes graph in
  e_fold graph (fun g arc -> let (arc1, arc2) = arcs_ecart arc in
                             new_arc (new_arc g arc1) arc2) new_graph;;

                             (*
(* Ford-Fulkerson *)
let ford_fulkerson graphe source destination = graphe;;*)
