open OUnit2
open Problems4

(** INVOCATION 
    ocamlbuild -use-ocamlfind -tag debug tests4.byte
*)

let tree_a = Node ('x', Node ('x', Empty, Empty),Node ('x', Node ('x', Empty, Empty), Empty))
let tree_b = Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
let tree =  Node('x', tree_a, tree_b)

let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',Node('h', Node('g', leaf 'e',Empty), Empty)),leaf 'm'),
    Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty))

let example_layout_tree2 =
    let leaf x = Node (x, Empty, Empty) in
    Node('a', Node('b', leaf 'd', leaf 'e'),
    Node('c', Empty, Node('f', leaf 'g', Empty)))

let example_layout = 
    Node (('n', 8, 1),Node (('k', 6, 2), Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
    Node (('h', 5, 4), Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty), Empty)),
    Node (('m', 7, 3), Empty, Empty)),Node (('u', 12, 2), Node (('p', 9, 3), Empty,
    Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)),Empty))

let tests = "test suite for the BIN TREE section of 99 problems" >::: [
    
    "prob 56 - is_symmetric 1" >:: (fun _ -> assert_equal true (is_symmetric tree));

    "prob 57 - BST 1" >:: (fun _ -> assert_equal (Node (3, Node (2, Node (1, Empty, Empty), Empty),
            Node (5, Empty, Node (7, Empty, Empty)))) (construct [3;2;5;7;1]));
    "prob 57 - BST 2" >:: (fun _ -> assert_equal true (not(is_symmetric(construct [3;2;5;7;4]))));
    
    "prob 58 - generate and test symmetry 1" >:: (fun _ -> assert_equal [Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
        Node ('x', Empty, Node ('x', Empty, Empty))); Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
        Node ('x', Node ('x', Empty, Empty), Empty))] (sym_cbal_trees 5));

    "prob 58 - generate and test symmetry 2" >:: (fun _ -> assert_equal 256 (List.length (sym_cbal_trees 57)));
    "prob 58 - generate and test symmetry 3" >:: (fun _ -> assert_equal [(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1); (16, 0); (17, 8);
        (18, 0); (19, 16); (20, 0)] (List.map (fun n -> n, List.length(sym_cbal_trees n)) (range 10 20)));

    "prob 65 - layout tree 1 " >:: (fun _ -> assert_equal example_layout (layout_binary_tree_1 example_layout_tree));
    
    "prob 67 - string of tree 1" >:: (fun _ -> assert_equal "a(b(d,e),c(,f(g,)))" (string_of_tree example_layout_tree2));
]

let _ = run_test_tt_main tests