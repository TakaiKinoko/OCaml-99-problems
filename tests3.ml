open OUnit2
open Problems3

(** INVOCATION 
    ocamlbuild -use-ocamlfind -tag debug tests3.byte
*)

let bool_table1 = 
[([("a", true); ("b", true); ("c", true)], true);
 ([("a", true); ("b", true); ("c", false)], true);
 ([("a", true); ("b", false); ("c", true)], true);
 ([("a", true); ("b", false); ("c", false)], false);
 ([("a", false); ("b", true); ("c", true)], false);
 ([("a", false); ("b", true); ("c", false)], false);
 ([("a", false); ("b", false); ("c", true)], false);
 ([("a", false); ("b", false); ("c", false)], false)]

let bool_table2 = (let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c)))))

let tests = "test suite for the LOGIC section of 99 problems" >::: [
    
    "prob 46&47 - booltable2 1" >:: (fun _ -> assert_equal [(true, true, true); (true, false, true); (false, true, false);
        (false, false, false)] (table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")))));   
    
    "prob 48 - booltable 1" >:: (fun _ -> assert_equal [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
        ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)] (table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")))));

    "prob 48 - booltable 2" >:: (fun _ -> assert_equal  bool_table1  bool_table2);   

    "prob 49 - graycode 1" >:: (fun _ -> assert_equal ["0"; "1"] (gray 1));
    "prob 49 - graycode 2" >:: (fun _ -> assert_equal ["00"; "01"; "11"; "10"] (gray 2));
    "prob 49 - graycode 3" >:: (fun _ -> assert_equal ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] (gray 3));
]

let _ = run_test_tt_main tests