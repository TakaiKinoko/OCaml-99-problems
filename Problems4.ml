(** BINARY TREES *)
type 'a binary_tree = 
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let range a b =
    let rec aux a b =
      if a > b then [] else a :: aux (a+1) b  in
    if a > b then List.rev (aux b a) else aux a b

(*55. Construct completely balanced binary trees. (medium) *)
(** Write a function cbal_tree to construct completely balanced binary trees for a given number of nodes. 
    The function should generate ALL POSSIBLE SOLUTIONS via backtracking. 
    Put the letter 'x' as information into all nodes of the tree. *)

(** my recursive version, not backtracking, to create just one tree. In practice probably only useful when initializing a tree from scratch*)
(** always favors the rightmost branch *)
let rec cbal_tree_rec (n:int) : char binary_tree= 
    match n with 
    | 0 -> Empty
    | 1 -> Node('x', Empty, Empty)
    | x -> Node('x', (cbal_tree_rec ((x-1)/2)), (cbal_tree_rec (x-1 - ((x-1)/2))))

(*let cbal_tree (n:int) : char binary_tree list = 
    let rec add_leaf (tree:char binary_tree): char binary_tree list = 
        match tree with
        | Empty -> [Node('x', Empty, Empty)]
        | Node(x, a, b) -> [Node(x,add_leaf a,b)]@[Node(x, a, add_leaf b)]  *)

let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node('x', l, r) :: a) all right 
    in
    List.fold_left add_right_tree all left

let rec cbal_tree n =
    if n = 0 then [Empty]
    else if n mod 2 = 1 then
      let t = cbal_tree (n / 2) in
      add_trees_with t t []
    else (* n even: n-1 nodes for the left & right subtrees altogether. *)
      let t1 = cbal_tree (n / 2 - 1) in
      let t2 = cbal_tree (n / 2) in
      add_trees_with t1 t2 (add_trees_with t2 t1 [])


(** 56. Symmetric binary trees. (medium) *)
let rec is_mirror a b = 
    match a, b with 
    | Empty, Empty -> true
    | Empty, Node(_,_,_) -> false
    | Node(_,_,_), Empty -> false
    | Node(_, ax, ay), Node(_,bx, by) -> (is_mirror ax by) && (is_mirror ay bx)

let is_symmetric a = 
    match a with 
    | Empty -> true
    | Node(_, a, b) -> is_mirror a b


(** 57. Binary search trees (dictionaries). (medium) *)
(** Construct a binary search tree from a list of integer numbers.*)
let construct (l:int list): int binary_tree = 
    let rec add_node num tree = 
        match tree with 
        | Empty -> Node(num, Empty, Empty)
        | Node(x, l, r) -> if num = x then Node(x, l, r) 
                else if num <x then Node(x, add_node num l, r) 
                else Node(x, l, add_node num r)
    in 
    let rec aux tree rest =  (** tree: intermediary tree, rest: rest of the list *) 
        match rest with 
        | [] -> tree
        | hd::tl -> aux (add_node hd tree) tl
    in 
    aux Empty l


(** 58. Generate-and-test paradigm. (medium) *)
let sym_cbal_trees n = 
    List.filter is_symmetric (cbal_tree n)

(** 59.Construct height-balanced binary trees. (medium) *)
(** this one below is NOT RIGHT!!!! because height-balanced tree might NOT be completely balanced -- 
    e.g 
           x
         /   \
        x     x
       / \
      x   x
    
    it's height balanced but not completely balanced
*)
let hbal_tree_wrong (height:int) : char binary_tree list = 
    let high = (int_of_float)(2. ** (float_of_int)height) -1 
    and low = (int_of_float)(2. ** ((float_of_int)height -. 1.) ) 
    in 
    let rec aux n acc = 
    if n = high then (cbal_tree n)@acc
    else aux (n+1) (cbal_tree n)@acc
    in 
    aux low []


(** 61. Count the leaves of a binary tree. (easy) *)
let rec count_leaves = function 
    | Empty -> 0
    | Node(_, Empty, Empty) -> 1 
    | Node(_, l, r) -> count_leaves l + count_leaves r

(** 61A. Collect the leaves of a binary tree in a list. (easy) *)
let rec leaves = function 
    | Empty -> []
    | Node(x, Empty, Empty) -> [x]
    | Node(_, l, r) -> (leaves l)@(leaves r)

(** 62. Collect the internal nodes of a binary tree in a list. (easy)  *)
let rec internals = function
    | Empty -> []
    | Node(x, Empty, Empty) -> []
    | Node(x, l, r) -> [x]@(internals l)@(internals r)


(** 62B. Collect the nodes at a given level in a list. (easy) *)
let at_level tree lev =
    assert (lev > 0);
    let rec aux n tree = 
    match tree with 
    | Empty -> []
    | Node(x, l, r) -> if n = 1 then [x] else (aux (n-1) l)@(aux (n-1) r)
    in 
    aux lev tree

(** 63. Construct a complete binary tree. (medium) *)
(** use the parent and child index relationship *)
let complete_binary_tree lst = 
    let len = List.length lst in  (** use 1-based counting *)
    let rec aux n : 'a binary_tree =  (** assemble the subtree at address n *)
    if 2*n + 1 <= len then Node((List.nth lst (n-1)), aux (2*n), aux (2*n +1)) 
    else if 2*n = len then Node((List.nth lst (n-1)), aux (2*n), Empty)
    else Node((List.nth lst (n-1)), Empty, Empty)
    in
    aux 1


(** 64. Layout a binary tree (1). (medium) *)
(** given a tree and a node, return the in_order sequence num of the node*)
(** return the index of the given node in an in-order traversal *)
let rec in_order_traversal tree = 
    match tree with 
    | Empty -> []
    | Node(x, l, r) -> (in_order_traversal l)@[x]@(in_order_traversal r)

(*let in_order_index node_elm tree =
    let rec aux acc node = 
    match node with 
    | Empty -> 
    | Node(x, Empty, Empty) -> 1
    | Node(x, l, r) ->  *)

let layout_binary_tree_1 tree = 
    let trav_l = in_order_traversal tree in
    let assoc_in_order = List.combine trav_l (range 1 (List.length trav_l)) in
    let rec aux lev node =
    match node with 
    | Empty -> Empty
    | Node(x, l, r) -> Node((x, List.assoc x assoc_in_order, lev), aux (lev+1) l, aux (lev+1) r)
    in 
    aux 1 tree     


(** 67. A string representation of binary trees. *)
let rec string_of_tree tree = 
    match tree with 
    | Empty -> ""
    | Node(x, Empty, Empty) ->  String.make 1 x
    | Node(x, l, r) ->  (String.make 1 x) ^ "(" ^ string_of_tree l ^ "," ^ string_of_tree r ^ ")"

let rec tree_of_string s =
    let len = String.length s in 
    match s.[0] with 
    | '('| ')' | ',' -> tree_of_string (String.sub s 1 len)
    | 


(** 68. Preorder and inorder sequences of binary trees. (medium)  *)
