open Gfile
open Tools
open Ford_fulk
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)
  let () = write_file (outfile^"0") graph in

  (*Test Tools*)
  let () = write_file (outfile^"1") (clone_nodes graph) in
  let tout_ca = add_arc (gmap graph (fun x -> int_of_string x)) 1 2 3 in 
  let () = write_file (outfile^"2") (gmap tout_ca (fun x -> string_of_int x)) in

  (* Export the graph to a file *)
  export graph (outfile^"3") ;

  (* Create a flot graph *)
  let flot_graph = create_flot_graph (gmap graph (fun x -> int_of_string x)) in
  let graph_ecart = graph_ecart flot_graph in
  export (gmap graph_ecart (fun x -> string_of_int x)) (outfile^"4");
  
    (* Find a path *)
  let path = (find_path graph_ecart 0 4) in
  let rec string_path path = 
    match path with
    | [] -> ""
    | x:: rest -> string_of_int x ^ " " ^ string_path rest in
  Printf.printf "Path: %s\n" (string_path path);
  let min_path = min_path_value graph_ecart path in
  Printf.printf "Min path: %d\n" min_path;
  let new_graph = update_path graph_ecart path min_path in
  export (gmap new_graph (fun x -> string_of_int x)) (outfile^"5");;



  ()

