open Graph
open Tools

(* Création du type Flot *)
type flot =
  { 
    (* Capacité *)
    capa: int ;

    (* Flot *)  
    flot: int}


(* Création d'un graphe flot *)
let create_flot_graph (graph: int graph) = 
  gmap graph (fun lbl -> {capa = lbl ; flot= 0});;


let write_flot (init : int graph) ecart_graph = 
  let aux graph arc =
    let retour = find_arc ecart_graph arc.tgt arc.src in
    match retour with
    |None -> new_arc graph {src = arc.src ; tgt = arc.tgt ; lbl = {capa = arc.lbl; flot = 0}}
    |Some x -> new_arc graph {src = arc.src ; tgt = arc.tgt ; lbl = {capa = arc.lbl; flot = x.lbl}}
  in 
  e_fold init aux;;


(* Ecart *)
let arcs_ecart (arc : flot arc) graph =
  let capa = arc.lbl.capa in
  let flot = arc.lbl.flot in
  let ecart = capa - flot in
  match (find_arc graph arc.tgt arc.src) with
  |None -> ({src = arc.src; tgt = arc.tgt; lbl = ecart},{src = arc.tgt; tgt = arc.src; lbl = flot})
  |Some x -> ({src = arc.src; tgt = arc.tgt; lbl = ecart+x.lbl.flot},{src = arc.tgt; tgt = arc.src; lbl = flot + (x.lbl.capa - x.lbl.flot)});;


(*Graph ecart*)
let graph_ecart (graph : flot graph) =
  let new_graph = clone_nodes graph in
  e_fold graph (fun g arc -> let (arc1, arc2) = arcs_ecart arc graph in
                             new_arc (new_arc g arc1) arc2) new_graph;;


(* Check if the arc is equal to 0*)
let arc_valid arc =
  match arc.lbl with
  |0 -> false
  |_-> true;;

(* Find a path in the arc *)
let find_path (graph : int graph) source destination =
  let rec find_path_aux graphe src dest visited =
    if src = dest then [src]
    else
      let out_arcs = out_arcs graphe src in
      let rec find_path_aux2 out_arcs visited =
        match out_arcs with
        |[] -> []
        |x::rest-> if (List.mem x.tgt visited || not(arc_valid x)) then find_path_aux2 rest visited
                 else let path = find_path_aux graphe x.tgt dest (x.tgt::visited) in
                      if path = [] then find_path_aux2 rest visited
                      else src::path
      in
      find_path_aux2 out_arcs visited
  in
  find_path_aux graph source destination [source];;


(* Find minimum value of the path*)
let rec min_path_value graph path =
  match path with
  |[] -> max_int
  |[_] -> max_int
  |x::y::rest -> let arc = find_arc graph x y in
                 match arc with
                 |None -> max_int
                 |Some z -> min z.lbl (min_path_value graph (y::rest));;


(* Update the path *)
let rec update_path graph path value =
    match path with
    |[] -> graph
    |[_] -> graph
    |x::y::rest -> 
      let new_graph = add_arc graph x y (-value) in
      let new_graph_bis = add_arc new_graph y x (value) in
      update_path new_graph_bis (y::rest) value;;


(* Check if a path exist *)
let path_exist path =
  match path with
  |[] -> false
  |[_] -> false
  |_::_ -> true;;

(* Boucle Princiaple de l'algorithme de Ford-Fulkerson *)
let rec ford_fulkerson_boucle graphe source destination = 
  let path = find_path graphe source destination in
  if path_exist path then
    let min_path_value = min_path_value graphe path in
    let new_graph = update_path graphe path min_path_value in
    ford_fulkerson_boucle new_graph source destination
  else graphe;;

(* Alogithme de Ford-Fulkerson *)
let ford_fulkerson graphe source destination = 
  let flot_graphe = create_flot_graph graphe in
  let ecart_graphe = graph_ecart flot_graphe in
  let graphe_ecart_final = ford_fulkerson_boucle ecart_graphe source destination in
  write_flot graphe graphe_ecart_final;;