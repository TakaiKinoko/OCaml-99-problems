open OUnit2
open Problems5

(** INVOCATION 
    ocamlbuild -use-ocamlfind -tag debug tests5.byte
*)
let t = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])])

let tests = "test suite for the Multi Tree section of 99 problems" >::: [
    "prob 70C - count nodes 1" >:: (fun _ -> assert_equal 2 (count_nodes (T('a', [T('f',[]) ]))));
    "prob 70C - count nodes 2" >:: (fun _ -> assert_equal 7 (count_nodes (T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])]))));
    "prob 70C - fold left 1" >::(fun _ -> assert_equal 2 (count_nodes_fold (T('a', [T('f',[]) ]))));
    "prob 70C - count nodes 2" >:: (fun _ -> assert_equal 7 (count_nodes_fold (T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])]))));

    "prob 70 - string_of_tree 1">:: (fun _ -> assert_equal "afg^^c^bd^e^^^" (string_of_tree t));
    "prob 70 - string_of_tree 2">:: (fun _ -> assert_equal "afg^^c^bd^e^^^" (string_of_tree_fold t));

    "prob 72 - bottom-up 1" >:: (fun _-> assert_equal ['b'; 'a'] (bottom_up (T('a', [T('b', [])]))) );
    "prob 72 - bottom-up 2" >:: (fun _-> assert_equal ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a'] (bottom_up t));

    "prob 73 - lispy 1" >:: (fun _ -> assert_equal "a" (lispy (T('a', []))));
    "prob 73 - lispy 2" >:: (fun _ -> assert_equal "(a b)" (lispy (T('a', [T('b', [])]))));   
    "prob 73 - lispy 3" >:: (fun _ -> assert_equal "(a (f g) c (b d e))" (lispy t));  
]

let _ = run_test_tt_main tests