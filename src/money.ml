open Graph
open Ford_fulk

let create_liste liste line  =
  try Scanf.sscanf line "p %s %d" (fun personne somme -> (personne, somme) :: liste)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let rec add_id liste compteur (map: (int, 'a *'b) Hashtbl.t)  =
  match liste with
  | [] -> map
  | (personne, somme) :: rest -> (Hashtbl.add map compteur (personne,somme)); add_id rest (compteur + 1) map;;


let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

    
let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'p' -> create_liste graph line 

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop [] in
  
  close_in infile ;
  final_graph

let create_hashtable liste =
  let map = Hashtbl.create (List.length liste) in
  add_id liste 0 map;;

let create_all_nodes graphe (hastable: (int, 'a *'b) Hashtbl.t) =
  let graph = ref graphe in
  Hashtbl.iter (fun id _ -> graph := new_node !graph id) hastable;
  !graph

let add_source_node graph = (* Ajoute le noeud source, pour les personnes en négatif *)
  new_node graph (-1)

let add_destination_node graph = (* Ajoute le noeud destination, pour les personnes en positif *)
  new_node graph (-2)

(* Fonction qui ajoute les arcs vers la source et la destination *)
let add_arcs graph hastable moyenne = 
  let graph = ref graph in
  Hashtbl.iter (fun id (_, somme) -> 
    Printf.printf "Somme = %d\n" somme;
    if somme > moyenne then
      graph := new_arc !graph {src = id; tgt = (-2); lbl = (somme - moyenne)}
    else
      graph := new_arc !graph {src = (-1); tgt = id; lbl = (moyenne - somme)}
  ) hastable;
  !graph

let add_arcs_between_nodes graph hastable = 
  let graph = ref graph in
  Hashtbl.iter (fun id (_, _) -> 
    Hashtbl.iter (fun id2 (_, _) -> 
      if id <> id2 then
        graph := new_arc !graph {src = id; tgt = id2; lbl = 100000}
    ) hastable
  ) hastable;
  !graph

let calculate_moyenne liste =
  let rec aux liste somme nb_personnes =
    match liste with
    | [] -> somme / nb_personnes
    | (_, s) :: rest -> aux rest (somme + s) (nb_personnes + 1)
  in
  aux liste 0 0

let export_Money hashtable graph path =
  let ff = open_out path in

  Printf.fprintf ff "
  digraph G {
  fontname=\"Helvetica,Arial,sans-serif\" 
  node [fontname=\"Helvetica,Arial,sans-serif\"] 
  edge [fontname=\"Helvetica,Arial,sans-serif\"]
  rankdir=LR;
  node [shape = circle];";

  (* Write all arcs *)
  Hashtbl.iter (fun id (personne, _) ->
    Hashtbl.iter (fun id2 (personne2, _) ->
      if id <> id2 then
        e_iter graph (fun arc ->
          if arc.src = id && arc.tgt = id2 then
            Printf.fprintf ff "%s -> %s [label=\"%s\"];\n" personne personne2 arc.lbl
        )
    ) hashtable
  ) hashtable;

  Printf.fprintf ff "}\n" ;

  close_out ff ;
  ();;

let tricount =
  let list = from_file "graphs/tricount.txt" in
  let hashtable = create_hashtable list in
  let graph = create_all_nodes empty_graph hashtable in
  let moyenne = calculate_moyenne list in
  let graph = add_source_node graph in
  let graph = add_destination_node graph in
  let graph = add_arcs graph hashtable moyenne in
  let graph = add_arcs_between_nodes graph hashtable in
  let exportation = export_Money hashtable in
  ford_fulkerson graph (-1) (-2) exportation;
  Printf.printf "Moyenne = %d\n" moyenne  ;
  Hashtbl.iter (fun id (personne, somme) -> Printf.printf "ID: %d, Personne: %s, Somme: %d\n" id personne somme) hashtable;;



