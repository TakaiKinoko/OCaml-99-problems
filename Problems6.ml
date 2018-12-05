(** representation 1: ('a * 'a) list, where isolated nodes cannot be representd *)

(** representation 2:  *)
type 'a graph_term = { nodes: 'a list; edges: ('a * 'a) list}

(** representation 3: *)
type 'a adj_list = ('a * 'a list) list

(** representation 4: human friendly*)


(** 80. Conversions. (easy) *)
(** I don't think you can convert 1 to others because isolated nodes gets left out! *)

(** WRONG!!! *)
(*let graph_to_adj graph = 
    let nodes = List.map (fun x -> (x, [])) graph.nodes  (** this will be the return list*)
    and edges = graph.edges
    in  (** iterate over the edges, add nodes to their correspoinding nodes*)
    List.map (fun (x,y) -> 
                let x_entry = List.assoc x nodes 
                and y_entry = List.assoc y nodes 
                in 
                (x, (y::x_entry))::(y, (x::y_entry))::(List.(remove_assoc y (remove_assoc x nodes)))) edges;  *)

(**81. Path from one node to another one. (medium) 
The function should return the list of all paths via backtracking.*)
let paths g a b = 
    let get_adj g a = List.filter (fun (x, y) -> x = a || y = a ) g.edges in
    let rec f acc item = 
        if 