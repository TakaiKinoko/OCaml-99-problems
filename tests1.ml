open OUnit2
open Problems1

(** INVOCATION 
    ocamlbuild -use-ocamlfind -tag debug tests1.byte
*)

let tests = "test suite for the list section of 99 problems" >::: [
    "prob 1 - test1" >:: (fun _ -> assert_equal [1] (convert_option (last_elm [2;3;4;1])) );
    "prob 1 - test2" >:: (fun _ -> assert_equal [] (convert_option (last_elm [])) );
    "prob 1 - test3" >:: (fun _ -> assert_equal ["hello"] (convert_option (last_elm ["world";"love";"travel";"hello"])) );
    
    "prob 2 - test1" >:: (fun _ -> assert_equal [4] (convert_option (second_to_last [2;3;4;1])) );
    "prob 2 - test2" >:: (fun _ -> assert_equal [] (convert_option (second_to_last [])) );
    "prob 2 - test3" >:: (fun _ -> assert_equal ["travel"] (convert_option (second_to_last ["world";"love";"travel";"hello"])) );
    "prob 2 - test4" >:: (fun _ -> assert_equal [] (convert_option (second_to_last ["hello"])) );
    
    "prob 3 - test1" >:: (fun _ -> assert_equal None (find_kth 5 [1;2;3;4]));
    "prob 3 - test2" >:: (fun _ -> assert_equal [4] (convert_option (find_kth 4 [1;2;3;4])));
    "prob 3 - test3" >:: (fun _ -> assert_equal [1] (convert_option (find_kth 1 [1;2;3;4])));

    "prob 4 - naive1" >:: (fun _ -> assert_equal 10 (list_len_naive [1;2;3;4;5;6;7;8;9;0]));
    "prob 4 - naive2" >:: (fun _ -> assert_equal 0 (list_len_naive []));
    "prob 4 - naive3" >:: (fun _ -> assert_equal 1 (list_len_naive [2]));
    "prob 4 - tail1" >:: (fun _ -> assert_equal 10 (list_len_tail [1;2;3;4;5;6;7;8;9;0]));
    "prob 4 - tail2" >:: (fun _ -> assert_equal 0 (list_len_tail []));
    "prob 4 - tail3" >:: (fun _ -> assert_equal 1 (list_len_tail [2]));
    "prob 4 - tail4" >:: (fun _ -> assert_equal 3 (list_len_tail ['a';'b';'c']));

    "prob 5 - rev1" >:: (fun _ -> assert_equal [] (rev []));
    "prob 5 - rev2" >:: (fun _ -> assert_equal [2;1] (rev [1;2]));
    "prob 5 - rev3" >:: (fun _ -> assert_equal ["hello"; "world"] (rev ["world"; "hello"]));

    "prob 6 - palindrome 1" >:: (fun _ -> assert_equal true (is_palin [1;2;3;3;2;1]));
    "prob 6 - palindrome 2" >:: (fun _ -> assert_equal true (is_palin ["x" ; "a" ; "m" ; "a" ; "x"]));
    "prob 6 - palindrome 3" >:: (fun _ -> assert_equal true (is_palin []));

    "prob 7 - flatten v1" >:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"] (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]));
    "prob 7 - flatten v2" >:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"] (flatten_v2 [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]));
    "prob 7 - flatten v3" >:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"] (flatten_v3 [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]));

    "prob 8 - compress 1" >:: (fun _ -> assert_equal ["a"; "b"; "c"; "a"; "d"; "e"] (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]));
    "prob 8 - compress 1" >:: (fun _ -> assert_equal [1] (compress [1;1;1;1;1;1;1;1;1]));

    "prob 9 - pack 1" >:: (fun _ -> assert_equal [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
            ["e"; "e"; "e"; "e"]] (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]));
    "prob 9 - pack 2" >:: (fun _ -> assert_equal [[1;1;1;1];[2;2];[3];[4;4];[5]] (pack [1;1;1;1;2;2;3;4;4;5]) );
    "prob 9 - pack 3" >:: (fun _ -> assert_equal [[1];[2];[3]] (pack [1;2;3]));
    (* corner case. How to have it spit out []? *)
    "prob 9 - pack 4" >:: (fun _ -> assert_equal [[]] (pack []));
    
    "prob 10 - run length v1" >:: (fun _ -> assert_equal [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] (run_len ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]));
    "prob 10 - run length encode" >:: (fun _ -> assert_equal [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]));
    "prob 10 - run length encode_v2" >:: (fun _ -> assert_equal [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] (encode_v2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]));

    "prob 11 - run length encode_v3" >:: (fun _ -> assert_equal [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
                    Many (4, "e")] (encode_v3 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]));

    "prob 12 - decode" >:: (fun _ -> assert_equal ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]));

    "prob 13 - encode_v4" >:: (fun _ -> assert_equal [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
                    Many (4, "e")] (encode_v4 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]));

    "prob 14 - duplicate 1" >:: (fun _ -> assert_equal ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] (duplicate ["a";"b";"c";"c";"d"]));
    "prob 14 - duplicate 2" >:: (fun _ -> assert_equal [] (duplicate []));
    "prob 14 - duplicate 3" >:: (fun _ -> assert_equal [Many (4, "a"); Many (4, "a"); One "b"; One "b" ; Many (2, "c");Many (2, "c"); Many (2, "a"); Many (2, "a"); One "d";One "d";
                    Many (4, "e");Many (4, "e")] (duplicate (encode_v4 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])));

    "prob 15 - replicate n times" >:: (fun _ -> assert_equal ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] (rep_n ["a";"b";"c"] 3));

    "prob 16 - drop nth 1" >:: (fun _ -> assert_equal ["a"; "b"; "d"; "e"; "g"; "h"; "j"] (drop_nth ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 ));
    "prob 16 - drop nth 2" >:: (fun _ -> assert_equal [] (drop_nth [1;1;1] 1));
    "prob 16 - drop nth 3" >:: (fun _ -> assert_equal [] (drop_nth [] 1));

    "prob 17 - split 1" >:: (fun _ -> assert_equal (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3));
    "prob 17 - split 2" >:: (fun _ -> assert_equal (["a"; "b"; "c"; "d"], []) (split ["a";"b";"c";"d"] 5));
    "prob 17 - split 3" >:: (fun _ -> assert_equal ([1;2;3] ,[]) (split [1;2;3] 3));
    "prob 17 - split 4" >:: (fun _ -> assert_equal ([] ,[]) (split [] 3));
    "prob 17 - split 5" >:: (fun _ -> assert_equal ([] ,[1;2;3]) (split [1;2;3] 0));

    "prob 18 - slice 1" >:: (fun _ -> assert_equal ["c"; "d"; "e"; "f"; "g"] (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6) );
    "prob 18 - slice 2" >:: (fun _ -> assert_equal [1] (slice [1;2;3] 0 0));
    "prob 18 - slice 3" >:: (fun _ -> assert_equal [1] (slice [1] 0 0));
    "prob 18 - slice 4" >:: (fun _ -> assert_equal [1;2;3;4] (slice [1;2;3;4] 0 3));
    "prob 18 - slice 5" >:: (fun _ -> assert_equal [4] (slice [1;2;3;4] 3 3));

    "prob 19 - rotate left 1" >:: (fun _ -> assert_equal ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3));
    "prob 19 - rotate left 2" >:: (fun _ -> assert_equal ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)));
    "prob 19 - rotate left 3" >:: (fun _ -> assert_equal ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-10)));
    "prob 19 - rotate left 4" >:: (fun _ -> assert_equal ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 67));

    "prob 20 - remove at 1" >:: (fun _ -> assert_equal ["a"; "c"; "d"] (remove_at 1 ["a";"b";"c";"d"]));
    "prob 20 - remove at 2" >:: (fun _ -> assert_equal ["b"; "c"; "d"] (remove_at 0 ["a";"b";"c";"d"]));
    "prob 20 - remove at 1" >:: (fun _ -> assert_equal ["a"; "b"; "c"] (remove_at 3 ["a";"b";"c";"d"]));

    "prob 21 - insert_at 1 ">:: (fun _ -> assert_equal ["a"; "alfa"; "b"; "c"; "d"] (insert_at "alfa" 1 ["a";"b";"c";"d"]));
    "prob 21 - insert_at 2 ">:: (fun _ -> assert_equal ["a"; "b"; "c"; "alfa"; "d"] (insert_at "alfa" 3 ["a";"b";"c";"d"]));
    "prob 21 - insert_at 3 ">:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "alfa"] (insert_at "alfa" 4 ["a";"b";"c";"d"]));

    "prob 22 - range 1" >:: (fun _ -> assert_equal [4; 5; 6; 7; 8; 9] (range 4 9));
    "prob 22 - range 2" >:: (fun _ -> assert_equal [9; 8; 7; 6; 5; 4] (range 9 4));
    "prob 22 - range 3" >:: (fun _ -> assert_equal [4] (range 4 4));
    "prob 22 - range 4" >:: (fun _ -> assert_equal [-4; -5; -6; -7; -8; -9] (range ~-4 ~-9));
    "prob 22 - range 5" >:: (fun _ -> assert_equal [-9; -8; -7; -6; -5; -4] (range ~-9 ~-4)); 



    "prob 25 - permutation 1" >:: (fun _ -> assert_equal (List.sort (fun x y -> if x < y then -1 else if x=y then 0 else 1) (permutation ["a"; "b"; "c"; "d"; "e"; "f"])) ["a"; "b"; "c"; "d"; "e"; "f"]);
    "prob 25 - permutation 2" >:: (fun _ -> assert_equal (List.sort (fun x y -> if x < y then -1 else if x=y then 0 else 1) (permutation ["a"; "b"])) ["a"; "b"]);   
    "prob 25 - permutation 3" >:: (fun _ -> assert_equal (List.sort (fun x y -> if x < y then -1 else if x=y then 0 else 1) (permutation ["a"])) ["a"]);
    "prob 25 - permutation 4" >:: (fun _ -> assert_equal (List.sort (fun x y -> if x < y then -1 else if x=y then 0 else 1) (permutation [1;2;3;4;5;6])) [1; 2; 3; 4; 5; 6]);
    
    "prob 26 - combination 1" >:: (fun _ -> assert_equal [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]] (extract 2 ["a";"b";"c";"d"]));
    "prob 26 - combination 2" >:: (fun _ -> assert_equal [["a"]; ["b"]; ["c"]] (extract 1 ["a"; "b"; "c"]));
    "prob 26 - combination 3" >:: (fun _ -> assert_equal [["a"; "b"; "c"]] (extract 3 ["a"; "b"; "c"]));
]

let _ = run_test_tt_main tests