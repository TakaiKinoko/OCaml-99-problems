(** IMPORTANT: #26 #27 #28 *)


(**1. Write a function last : 'a list -> 'a option 
that returns the last element of a list. (easy) *)
let rec last_elm (l:'a list):('a option) = 
    match l with 
    | [] -> None
    | elm::[] -> Some(elm)
    | hd::tl -> last_elm tl

(** helper function for testing last_elm*)
let convert_option (o: 'a option): 'a list = 
    match o with 
    | None -> []
    | Some(elm) -> [elm]  (** unpack the element out of Some *)


(**2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec second_to_last = function 
    | x::_::[]-> Some(x)
    | _::x::y-> second_to_last (x::y)
    | _::[] -> None
    | [] -> None  

let rec last_two = function 
    | x::y::[] -> Some(x,y)
    | x::y::z -> last_two (y::z)
    | _::[] -> None
    | [] -> None

(**3. Find the k'th element of a list *)
let rec find_kth k lst = 
    assert (k > 0);    (** 1 based counting *)
    match lst with
    | hd::tl -> if k = 1 then Some(hd) else find_kth (k-1) tl
    | [] -> None


(**4. Find the number of elements of a list. (easy) *)
let rec list_len_naive = function
    | [] ->0
    | hd::tl -> 1 + (list_len_naive tl)  (** not tail recursive*)

let list_len_tail lst = 
    let rec helper n lst = 
    match lst with
    | [] -> n
    | hd::tl -> helper (n+1) tl
    in helper 0 lst


(**5. Reverse a list. (easy)
OCaml standard library has List.rev but we ask that you reimplement it.  *)
let rev lst = 
    let rec helper l lst = 
    match lst with
    | hd::tl -> helper (hd::l) tl
    | [] -> l
    in helper [] lst


(** 6. Find out whether a list is a palindrome. (easy) *)
let is_palin lst = 
    (rev lst) = lst 

(** 7. Flatten a nested list structure. (medium) *)
(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

(** v.1 *)
let rec flatten lst =  
    match lst with
    | []-> []
    | hd::tl -> match hd with
        | One(x) -> x::(flatten tl)
        | Many(l) -> List.append (flatten l) (flatten tl) 

(** v.2 no nested match *)
let rec flatten_v2 lst = 
    match lst with 
    | []-> []
    | One x:: tl -> x::(flatten_v2 tl)
    | Many l:: tl -> (flatten l)@(flatten tl)

(** v.3 with auxiliary function *)
let flatten_v3 lst = 
    let rec aux acc =  function 
    | [] -> acc
    | One x:: tl -> aux (x::acc) tl
    | Many l:: tl -> aux (aux acc l) tl 
    in 
    List.rev (aux [] lst)


(**8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress lst =
    match lst with
    | x::y::z -> if x = y then compress (y::z) else x::(compress (y::z))
    | x::[] -> [x]
    | [] -> []

(** note that ocaml doesn't allow binding the same variable twice in a matching case, unlike scheme *)

(** slightly improve the above would be: *)
let rec compress = function 
    | a :: (b ::_ as tl) -> if a = b then compress tl else a::compress tl   (** fancy syntax!! tl is b::_ *)
    | x -> x 

(** 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
(*let rec pack lst = 
    let rec aux acc lst =
    match lst with 
    | hd::tl -> if hd = List.hd acc then aux (hd::acc) tl else acc
    | [] -> acc 
    in match lst with 
    | x::y::z -> if x = y then (aux [] lst)::(pack (y::z)) else (aux [] (y::z))::(pack z)
    | x::[] -> [[x]]
    | [] -> [] *)

let pack lst =
    let rec aux n acc l : 'a list list =   (** n is the front list of the new list to be matched with, acc is the rest of the return list; l is the rest of the list to be processed *)
    match l with 
    | hd::tl -> begin
        match n with 
        | x::xs -> 
        if hd = x 
        then aux (hd::n) acc tl
        else aux [hd] (n::acc) tl
        | []-> aux [hd] acc tl
    end
    | [] -> (n::acc)
    in 
    rev(aux [] [] lst)

(** 10. Run-length encoding of a list. (easy) *)
(** verbose test version*)
let run_len lst = 
    let f elm = 
    match elm with 
    | [] -> raise (Invalid_argument("empty element"))
    | hd::tl -> (List.length elm, hd)
    in
    List.map f (pack lst)  

(** modified *)
let encode lst = 
    List.map (fun elm-> (List.length elm),(List.hd elm)) (pack lst)


(** using pack costs more memory. the following one is better*)
let encode_v2 lst = 
    let rec aux n acc l =  (** n is the head of the return list, acc is the rest *)
    match l with  
    | hd::tl -> if hd = snd n then aux ((fst n) +1, hd) acc tl else aux (1, hd) (n::acc) tl
    | [] -> (n::acc)
    in match lst with 
    | [] -> []
    | hd::tl -> rev(aux (1, hd) [] tl)  (** this made sure that fst n and snd n will always be valid*)

(** 11. Modified run-length encoding. (easy) *)
type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode_v3 lst = 
    let rec aux n acc l : 'a list = 
    match l with 
    | hd::tl -> begin
        match n with 
        | One(c) -> if hd = c then aux (Many (2, c))  acc tl else aux (One hd) (n::acc) tl
        | Many (i, c) -> if hd = c then aux (Many(i+1, c)) acc tl else aux (One hd) (n::acc) tl  
        end
    | [] -> (n::acc)
    in match lst with 
    | []->[]
    | hd::tl -> rev(aux (One hd) [] tl)

(** 12. Decode a run-length encoded list. (medium) *)
let decode lst = 
    let rec aux ret_l in_l =
    match in_l with
    | hd::tl -> begin 
        match hd with 
        | One(c) -> aux (c::ret_l) tl 
        | Many(i,c) -> if i > 2 then aux (c::ret_l) (Many(i-1, c)::tl) else aux (c::ret_l) (One(c)::tl)
        end
    | [] -> ret_l
    in
    rev (aux [] lst)

(** 13. Run-length encoding of a list (direct solution). (medium) *)
let encode_v4 lst = 
    let rec aux ret_hd ret_tl in_l = 
    match in_l with 
    | hd::tl -> begin
        match ret_hd with
        | One(c) -> if hd =  c then aux (Many(2, c)) ret_tl tl else aux (One hd) (ret_hd::ret_tl) tl
        | Many(i, c) -> if hd = c then aux (Many (i+1, c)) ret_tl tl else aux (One hd) (ret_hd::ret_tl) tl
    end
    | []-> ret_hd::ret_tl
    in
    rev (aux (One (List.hd lst)) [] (List.tl lst))
 
 (** 14. Duplicate the elements of a list. (easy) *)
 let duplicate lst = 
    let rec aux ret_l in_l = 
    match in_l with
    | hd::tl-> aux (hd::hd::ret_l) tl 
    | [] -> ret_l
    in 
    rev (aux [] lst)

(** 15. Replicate the elements of a list a given number of times. (medium) *)
let rep_n lst n = 
    let rec aux i ret_l in_l = 
    match in_l with
    (*| hd::tl -> if i>0 then aux (i-1) (hd::ret_l) in_l else aux n ret_l tl*)
    | hd::tl -> if i>1 then aux (i-1) (hd::ret_l) in_l else aux n (hd::ret_l) tl
    | [] -> ret_l
    in 
    rev (aux n [] lst) 

(** 16. Drop every N'th element from a list. (medium) *)
let drop_nth lst n = 
    assert (n > 0);
    let rec aux i ret_l in_l = 
    match in_l with
    | hd::tl ->
    if i = 1
    then aux n ret_l (List.tl in_l)
    else aux (i-1) (hd::ret_l) tl
    | [] -> ret_l
    in
    rev (aux n [] lst)

(**17. Split a list into two parts; the length of the first part is given. (easy) *)
let split lst n =
    let rec aux i fst snd = 
    match i with
    | 0 -> ([], lst)  (** handle special case where argument i is less than 1. see test case  *)
    | 1 -> rev((List.hd snd)::fst), (List.tl snd) 
    | _ -> aux (i-1) ((List.hd snd)::fst) (List.tl snd)
    in 
    if n > List.length lst then (lst, []) else aux n [] lst


(** 18. Extract a slice from a list. (medium) *)
(** zero-based counting, inclusive at both ends *)
let slice lst i k = 
    assert (i>=0 && k<= List.length lst);
    let rec aux n ret_l in_l = 
        if n < i then aux (n+1) ret_l (List.tl in_l) 
        else if n <= k then aux (n+1) ((List.hd in_l)::ret_l) (List.tl in_l)
        else ret_l
    in 
    rev (aux 0 [] lst)

(** 19. Rotate a list N places to the left. (medium) *)
let rotate lst n =  
    let rec aux i left rest = 
    match i with 
    | 0 -> List.append rest (rev left)
    | _ -> aux (i-1) ((List.hd rest)::left) (List.tl rest)
    in 
    if n >= 0 then aux (n mod (List.length lst)) [] lst
    else rev (aux ((~-n) mod (List.length lst)) [] (rev lst))

(** 20 Remove the K'th element from a list. (easy) *)
(** zero based counting*)
let remove_at k lst =
    assert (k>=0 && k< List.length lst);
    let rec aux (i:int) (fst_l:'a list) (snd_l: 'a list) : 'a list = 
    if i = k then  (rev fst_l)@(List.tl snd_l) 
    else (aux (i+1) ((List.hd snd_l)::fst_l) (List.tl snd_l))
    in
    aux 0 [] lst

(**21. Insert an element at a given position into a list. (easy) *)
let insert_at e pos lst =  (* 'a -> int -> 'a list*)
(** zero based counting *)
    assert (pos >=0 && pos <= List.length lst);
    let rec aux i left right = 
    if i = pos 
    then rev(e::left)@right
    else aux (i+1) ((List.hd right)::left) (List.tl right)
    in aux 0 [] lst

(** 22. Create a list containing all integers within a given range. (easy) *)
let rec range m n = 
    if m<n then 
    m::(range (m+1) n)
    else if m = n then [n]
    else m::(range (m-1) n)

(**23. Extract a given number of randomly selected elements from a list. (medium)*)
let rand_select lst n = 
    assert (n >= 0);
    let rec aux (l:'a list) (n:int) = 
      match n with 
      | 0 -> l
      | _ -> aux ((List.nth lst (Random.int (List.length lst)))::l) (n-1)
    in List.rev (aux [] n)

(**24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let lotto_select n m = 
    assert (n >= 0 && m >=1);
    let rec get_rand n lst= 
    match n with 
    | 0 -> lst
    | _ -> get_rand (n-1) (((Random.int m-1) + 1)::lst)
    in get_rand n []

(**25. Generate a random permutation of the elements of a list. (easy)*)
let permutation lst = 
    assert (lst<>[]);
    let rec extract_nth n l =  (** return a list as what's remained after the nth's been extracted *)
        match l with 
        | hd::tl -> if n == 0 then tl else hd::(extract_nth (n-1) tl)
        | [] -> []
    in 
    let get_rand lst =   (** return a rand item and the rest of the list *)
        let n = Random.int (List.length lst)
        in (List.nth lst n), (extract_nth n lst)
    in 
    let rec aux n ret_l rest_l = 
        let temp = get_rand rest_l in
        match n with 
        | 1 -> (List.hd rest_l)::(List.rev ret_l)
        | _ -> aux (n - 1)  ((fst temp)::ret_l) (snd temp)
    in aux (List.length lst) [] lst

(**26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)
let rec extract n lst = 
    if n <= 0 then [[]]
    else match lst with 
        | [] -> []
        | hd::tl ->
            let with_hd = List.map (fun l -> hd::l) (extract (n-1) tl) 
            and without_hd = extract n tl in 
        with_hd @ without_hd
     
(** 27. Group the elements of a set into disjoint subsets. (medium)*)

(** 1. In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? 
Write a function that generates all the possibilities and returns them in a list. *)
(** 2. Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.*)
(* This implementation is less streamlined than the one-extraction
    version, because more work is done on the lists after each
    transform to prepend the actual items. The end result is cleaner
    in terms of code, though. *)
  
    let group list sizes =
      let initial = List.map (fun size -> size, []) sizes in
  
    (* The core of the function. Prepend accepts a list of groups,
       each with the number of items that should be added, and
       prepends the item to every group that can support it, thus
       turning [1,a ; 2,b ; 0,c] into [ [0,x::a ; 2,b ; 0,c ];
       [1,a ; 1,x::b ; 0,c]; [ 1,a ; 2,b ; 0,c ]]
  
       Again, in the prolog language (for which these questions are
       originally intended), this function is a whole lot simpler.  *)
    let prepend p list =
      let emit l acc = l :: acc in
      let rec aux emit acc = function
        | [] -> emit [] acc
        | (n,l) as h :: t ->
           let acc = if n > 0 then emit ((n-1, p::l) :: t) acc
                     else acc in
           aux (fun l acc -> emit (h :: l) acc) acc t
      in
      aux emit [] list
    in
    let rec aux = function
      | [] -> [ initial ]
      | h :: t -> List.concat (List.map (prepend h) (aux t))
    in
    let all = aux list in
    (* Don't forget to eliminate all group sets that have non-full
       groups *)
    let complete = List.filter (List.for_all (fun (x,_) -> x = 0)) all in
    List.map (List.map snd) complete


(** 28. Sorting a list of lists according to length of sublists. (medium) *)


