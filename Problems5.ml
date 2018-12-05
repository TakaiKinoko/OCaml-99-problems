(** MULTI WAY TREES *)
type 'a mult_tree = T of 'a * 'a mult_tree list

(** 70C. Count the nodes of a multiway tree. (easy) *)
let rec count_nodes mt = 
    let rec aux t_lst = 
    match t_lst with 
    | hd::tl -> count_nodes hd + aux tl
    | [] -> 0
    in
    match mt with 
    | T(a, lst) -> 1 + aux lst

let rec count_nodes_fold mt = 
    match mt with 
    | T(_, lst) -> List.fold_left (fun acc t -> acc + count_nodes_fold t) 1 lst

(** 70. Tree construction from a node string. (medium)*)
let rec string_of_tree mt = 
    let rec aux t_lst = 
    match t_lst with 
    | hd::tl -> (string_of_tree hd) ^(aux tl)
    | [] -> ""
    in
    match mt with 
    | T(a, lst) -> (String.make 1 a) ^ (aux lst) ^ "^"  

let string_of_tree_fold mt = 
    let rec aux mt = 
    match mt with
    | T(a, lst) -> List.fold_left (fun acc t -> acc ^ (aux t) ^ "^") (String.make 1 a) lst
    in 
    (aux mt) ^ "^"



(** ??????????????    tree_of_string??????   *)

(** 71. Determine the internal path length of a tree. (easy) *)
let ipl tree = 
    let rec aux n tree = 
    match tree with
    | T(x, lst) -> (List.fold_left (fun acc t -> (aux (n+1) t) + acc) n lst)
    in
    aux 0 tree

(** 72. Construct the bottom-up order sequence of the tree nodes. (easy) *)
let rec bottom_up t = 
    match t with 
    | T( c, lst) -> (List.fold_left (fun acc t -> acc@(bottom_up t)) [] lst)@[c] 


(** 73. Lisp-like tree representation. (medium)*)
let rec lispy t = 
    match t with 
    | T(a, []) -> String.make 1 a
    | T(a, lst) -> (List.fold_left (fun acc t -> acc^ " "^(lispy t)) ("("^(String.make 1 a)) lst) ^ ")" 

