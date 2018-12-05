type bool_expr = 
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

(** 46 & 47. Truth tables for logical expressions (2 variables). (medium) *)
(** Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments). 
The return value must be a list of triples containing (value_of_a, value_of_b, value_of_expr) *)

let table2 (a:string) (b:string) (exp:bool_expr): (bool * bool * bool) list = 
    let rec parse_expr (a:string) (b:string) (sub1:bool) (sub2:bool) (exp: bool_expr): bool = 
    match exp with
    | Var (s) -> if s = a then sub1 else sub2     (** terminal case *)
    | Not (e) -> not (parse_expr a b sub1 sub2 e)
    | And (e1, e2) -> (parse_expr a b sub1 sub2 e1) && (parse_expr a b sub1 sub2 e2)
    | Or (e1, e2) -> if (parse_expr a b sub1 sub2 e1) then true else (parse_expr a b sub1 sub2 e2)
    in
    let wrap (bool1:bool) (bool2:bool) : (bool * bool * bool) = bool1, bool2, parse_expr a b bool1 bool2 exp
    in 
    ((wrap true true)::(wrap true false)::(wrap false true)::(wrap false false)::[])

(** 48. Truth tables for logical expressions. (medium) *)
(** Generalize the previous problem in such a way that the logical expression may contain any number of logical variables. 
    Define table in a way that table variables expr returns the truth table for the expression expr, 
    which contains the logical variables enumerated in variables. *)

(** helper function that returns the index of a given item i in the list: lst *)
let rec find_pos (lst:'a list) (i:'a) (starting_index: int) : int =
    match lst with 
    | hd::tl -> if hd = i then starting_index else find_pos tl i (starting_index + 1)
    | [] -> failwith ("Nonmatch")

(** args: variable list --  string list
          bool_list -- corresponding boolean value for all variables
          expr
    return: what the expression evaluates to *)
let rec parse_expr (var_lst:string list) (bool_lst: bool list) (expr: bool_expr): bool = 
    match expr with
    | Var (s) -> List.nth bool_lst  (find_pos var_lst s 0)   (** terminal case *)
    | Not (e) -> not (parse_expr var_lst bool_lst e)
    | And (e1, e2) -> (parse_expr var_lst bool_lst e1) && (parse_expr var_lst bool_lst e2)
    | Or (e1, e2) -> if (parse_expr var_lst bool_lst e1) then true else (parse_expr var_lst bool_lst e2)

(** takes the var_lst, bool_lst and the boolean evaluation result from parse_expr, wrap into return format *)
let wrap (var_lst:string list) (bool_lst: bool list) (eval:bool) : ((string * bool) list * bool) = 
    let rec aux (i:int) (acc:(string * bool) list) : (string * bool) list = 
        if i = List.length var_lst then List.rev acc
        else aux (i+1) (((List.nth var_lst i), (List.nth bool_lst i))::acc)
    in 
    ((aux 0 []), eval)

(** generate all permutations of a bool list of length n *)
let generate_bool_list n : (bool list) list= 
    let rec aux acc i = 
        if i = n then acc
        else begin
            let half1 = List.map (fun x-> true::x) acc 
            and half2 = List.map (fun x-> false::x) acc
        in 
        List.rev( aux (half1@half2) (i+1) )
        end
    in 
    if n = 0 then [] else aux [[true]; [false]] 1

let table (var_lst:string list) (expr: bool_expr): ((string * bool) list * bool) list = 
    let bool_lst = generate_bool_list (List.length var_lst) 
    in
    let rec aux acc n = 
    if n = List.length bool_lst then List.rev acc
    else aux ((wrap var_lst (List.nth bool_lst n) (parse_expr var_lst (List.nth bool_lst n) expr))::acc) (n+1)
    in 
    aux [] 0


(** 49. Gray code. (medium) *)
let rec gray n = 
    match n with 
    | 1 -> ["0"; "1"]
    | _ -> (List.map (fun x-> "0"^x) (gray (n-1)))@(List.rev(List.map (fun x-> "1"^x) (gray (n-1))))



(** 50. Huffman code (hard) *)
(*let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5) ]
huffman fs should be :  [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
 ("d", "111")] *)

type huff_IR =     (** intermediary representation initialization *)
    | Leaf of (string * int) * string 
    | Inner of huff_IR * huff_IR 

let init_IR l =  (** initially everyone is Leave with an empty list *)
    List.map (fun x -> Leaf(x, "")) l
(** the intermediary form of fs would be [ (("a", 45), []); (("b", 13), []); ...] *)

(** compute the total frequency of this node. if it's a leaf then the frequency of itself otherwise the total frequency of its subtrees *)
let rec compute_freq (i:huff_IR) : int = 
    match i with 
    | Leaf ((_, x),_) -> x
    | Inner(Leaf ((_, x),_), Leaf ((_, y),_)) -> x+y
    | Inner(Leaf ((_, x),_), Inner(y,z)) -> x + compute_freq y + compute_freq z
    | Inner(Inner(x, y), Leaf((_, z), _)) -> compute_freq x + compute_freq y + z
    | Inner(Inner (x,y), Inner(z, w) ) -> (compute_freq x) + (compute_freq y) + (compute_freq z) + (compute_freq w)

(** sort all the huff_IR nodes in increasing order *)
let sort_IR (l: huff_IR list) =
    List.sort (fun x y -> Pervasives.compare (compute_freq x) (compute_freq y)) l

(** adding either 1 or 0 to the beginning of the code in e or all children of e *)
let rec fatten e n = 
    match e with 
    | Leaf ((a,b), x) -> Leaf((a,b), n^x)
    | Inner(a, b) -> Inner((fatten a n),(fatten b n))

(** extract from an element of the final list its string and huffman code list *)
let rec extract_code ele =
    match ele with 
    | Leaf ((a,b), x) -> [(a, x)]
    | Inner(x, y) -> (extract_code x)@(extract_code y)

let huffman l = 
    assert (List.length l >= 2);
    let ir = sort_IR (init_IR l) 
    in 
    let rec generate ir = 
        match ir with 
        | [] -> failwith ("imbalanced tree")
        | hd::[] -> failwith ("imbalanced tree")
        | hd::tl::[] -> (fatten hd "1")::(fatten tl "0")::[]     (** terminating case *)
        | hd::tl::rest -> generate (sort_IR (Inner((fatten hd "1"), (fatten tl "0"))::rest))
    in
    List.flatten( List.map extract_code (generate ir))

(** represent the list as    (name*freq)*(int * int)*(int list) list        *)