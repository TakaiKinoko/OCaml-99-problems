open OUnit2
open Problems6

(** INVOCATION 
    ocamlbuild -use-ocamlfind -tag debug tests5.byte
*)

let tests = "test suite for the GRAPH section of 99 problems" >::: [
    


]

let _ = run_test_tt_main tests