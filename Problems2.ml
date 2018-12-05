(** IMPORTANT: #31 *)

(** 31. Determine whether a given integer number is prime. (medium) *)
let is_prime n = 
    let rec aux i bool =
        if i = n then bool else if bool = false then false else aux (i+1) (bool && (n mod i <> 0))
    in 
    if n<=1 then false 
    else aux 2 true

(** 32. Determine the greatest common divisor of two positive integer numbers. (medium)*)
let rec gcd m n = 
    if m mod n = 0 then n
    else if n > (m mod n) then gcd n (m mod n)
    else gcd (m mod n) n

(** 33. Determine whether two positive integer numbers are coprime. (easy) *)
let coprime m n = 
    gcd m n = 1

(** 34. Calculate Euler's totient function φ(m). (medium) *)
let phi m = 
    let rec aux acc i = 
    if i = m then acc else if coprime m i then aux (acc+1) (i+1) else aux acc (i+1)
    in aux 0 1

(** 35. Determine the prime factors of a given positive integer. (medium)*)
(**Construct a flat list containing the prime factors in ascending order. *)
let rec factors m = 
    let rec aux acc i n = 
    if (i = m) && (is_prime i) then i::acc 
    else if i=m then acc
    else if i = 1 then aux acc (i+1) n
    else if ((n mod i = 0) && (is_prime i)) then (aux (i::acc) 1 (n/i))
    else aux acc (i+1) n
    in 
    List.rev(aux [] 1 m) 

(** 36. Determine the prime factors of a given positive integer (2). (medium)
Construct a list containing the prime factors and their multiplicity. 
Hint: The problem is similar to problem Run-length encoding of a list (direct solution).*)

let rec factors_alt m = 
    let rec aux (hd:int*int) (acc: (int*int) list) (lst:int list) = 
    match lst with 
    | h1::(h2::t) -> if h1 = h2 then aux (h1, (snd hd)+1) acc (h2::t) else aux (h2, 1) (hd::acc) (h2::t) 
    | h::t -> aux hd acc t 
    | [] -> hd::acc
    in match (factors m) with 
    | hd::tl -> List.rev(aux (hd,1) [] (hd::tl))
    | []-> []

(** 37. Calculate Euler's totient function φ(m) (improved). (medium) *)
(*let phi_improved m = 
    let rec aux acc lst = 
    match lst with 
    | hd::tl-> aux (acc *. ((fst hd) -. 1.) *. ((fst hd) ** ((snd hd)-. 1.))) tl 
    | []-> acc
    and floatify lst = 
    List.map (fun x -> float_of_int (fst x), float_of_int (snd x)) (factors_alt lst)
    in 
    int_of_float(aux 1. (floatify m))  *)

let phi_improved m = 
    let rec pow i n = 
    if n < 1 then 1
    else i * pow i (n-1)
    in 
    let rec aux acc lst : int = 
    match lst with 
    | hd::tl -> aux (acc * ((fst hd)-1) * (pow (fst hd)( (snd hd)-1))) tl
    | [] -> acc
    in 
    aux 1 (factors_alt m)

(** 38. Performance measurement*) 
let timeit f a =
    let t0 = Unix.gettimeofday() in
    ignore(f a);
    let t1 = Unix.gettimeofday() in
    t1 -. t0


(** 39. A list of prime numbers. (easy) *)
let all_primes low high = 
    let rec aux n lst = 
        if n = high && is_prime n then List.rev (n::lst)
        else if n = high then List.rev lst 
        else if is_prime n then aux (n+1) (n::lst)
        else aux (n+1) lst
    in
    aux low []

(** there's a much cheaper is_prime in the given solution *)
let is_prime_alt n =
    let n = max n (-n) in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    is_not_divisor 2
  
let rec all_primes_alt a b =
    if a > b then [] else
      let rest = all_primes_alt (a + 1) b in
      if is_prime_alt a then a :: rest else rest

let timeit2 f a b=
    let t0 = Unix.gettimeofday() in
    ignore(f a b);
    let t1 = Unix.gettimeofday() in
    t1 -. t0


(** 40. Goldbach's conjecture. (medium) *)
let goldbach n = 
    assert(n mod 2 = 0 && n > 2);
    let rec aux i =
    if is_prime i && is_prime (n-i) then (i,(n-i))
    else aux (i+1) 
    in 
    aux 2
    

(** 41. A list of Goldbach compositions. (medium) *)
let goldbach_list low high = 
    (** assert high >= 4 *)
    let rec next acc n =
    if n = high then ((n, (goldbach n))::acc)
    else next ((n, (goldbach n))::acc) (n+2)
    in
    if low <=2 then List.rev (next [] 4)
    else if (low mod 2 = 0) then List.rev (next [] low)
    else List.rev (next [] (low+1))

let goldbach_limit low high lim = 
    List.filter (fun x -> if (fst (snd x)) > lim && (snd (snd x))>lim then true else false) (goldbach_list low high)