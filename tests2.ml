open OUnit2
open Problems2

(** INVOCATION 
    ocamlbuild -use-ocamlfind -tag debug tests2.byte
*)

let tests = "test suite for the arithmetic section of 99 problems" >::: [
    "prob 31 - prime 1" >:: (fun _ -> assert_equal true (is_prime 3));
    "prob 31 - prime 2" >:: (fun _ -> assert_equal true (is_prime 7));   
    "prob 31 - prime 3" >:: (fun _ -> assert_equal true (not (is_prime 1))); 
    "prob 31 - prime 4" >:: (fun _ -> assert_equal true (is_prime 1301081) ~printer: string_of_bool); 
    "prob 31 - prime 5" >:: (fun _ -> assert_equal false (is_prime 105));

    "prob 32 - gcd 1" >:: (fun _ -> assert_equal 2 (gcd 10 6) ~printer: string_of_int);
    "prob 32 - gcd 2" >:: (fun _ -> assert_equal 1 (gcd 13 27) ~printer: string_of_int);
    "prob 32 - gcd 2" >:: (fun _ -> assert_equal 2 (gcd 20536 7826) ~printer: string_of_int);

    "prob 33 - coprime 1">:: (fun _ -> assert_equal true (coprime 13 27));
    "prob 33 - coprime 2">:: (fun _ -> assert_equal false (coprime 20536 7826));

    "prob 34 - totient 1" >:: (fun _ -> assert_equal 4 (phi 10) ~printer: string_of_int);
    "prob 34 - totient 2" >:: (fun _ -> assert_equal 12 (phi 13) ~printer: string_of_int);

    "prob 35 - prime factors 1">:: (fun _ -> assert_equal [3;3;5;7] (factors 315));
    "prob 35 - prime factors 2">:: (fun _ -> assert_equal [3;3;3] (factors 27));
    "prob 35 - prime factors 3">:: (fun _ -> assert_equal [2;7;7;11] (factors 1078));
    "prob 35 - prime factors 4">:: (fun _ -> assert_equal [2;7;127] (factors 1778));
    "prob 35 - prime factors 5">:: (fun _ -> assert_equal [] (factors 1));
    "prob 35 - prime factors 6">:: (fun _ -> assert_equal [2] (factors 2));

    "prob 36 - factors with count 1" >:: (fun _ -> assert_equal [(3, 2); (5, 1); (7, 1)] (factors_alt 315));
    "prob 36 - factors with count 2" >:: (fun _ -> assert_equal [(2, 1); (7, 2); (11, 1)] (factors_alt 1078));
    "prob 36 - factors with count 3" >:: (fun _ -> assert_equal [(2, 1); (7, 1); (127, 1)] (factors_alt 1778));
    "prob 36 - factors with count 3" >:: (fun _ -> assert_equal [(3, 3)] (factors_alt 27));


    "prob 37 - totient improved 1" >:: (fun _ -> assert_equal 4 (phi_improved 10));
    "prob 37 - totient improved 1" >:: (fun _ -> assert_equal 12 (phi_improved 13));

    (** timeit testcase is meant to fail. only to put out the result *)
    "prob 38 - timeit1" >:: (fun _ -> assert_equal (timeit phi 10090) (timeit phi_improved 10090) ~printer: string_of_float);

    "prob 39 - list of primes" >:: (fun _ -> assert_equal 1000 (List.length (all_primes 2 7920)) ~printer: string_of_int);
    
    (** timeit testcase is meant to fail. only to put out the result *)
    "prob 39 - timeit2" >:: (fun _ -> assert_equal (timeit2 all_primes_alt 2 7920) (timeit2 all_primes_alt 2 7920) ~printer: string_of_float);

    "prob 40 - goldbach1" >:: (fun _ -> assert_equal (5, 23) (goldbach 28));
    "prob 40 - goldbach2" >:: (fun _ -> assert_equal (7, 47) (goldbach 54));
    (*"prob 40 - goldbach3" >:: (fun _ -> assert_equal (5569, 389965026814369) (goldbach 389965026819938)); *) (** timed out*)

    "prob 41a - goldbach_list 1" >:: (fun _ -> assert_equal [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
            (20, (3, 17))] (goldbach_list 9 20));
    "prob 41b - goldbach_limit 1" >:: (fun _ -> assert_equal [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
            (1928, (61, 1867))] (goldbach_limit 1 2000 50))
]

let _ = run_test_tt_main tests