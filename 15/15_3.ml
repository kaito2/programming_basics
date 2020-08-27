(* Purpose: Find all prime numbers less than or equal to n. *)
(* sieve: int -> int list *)
let sieve n = 
    (* Purpose: Generate list (2..n). *)
    (* from_two_to_n: int -> int list *)
    let rec from_two_to_n n = if n <= 2 then [2] else List.append (from_two_to_n (n - 1)) [n] in
    (* Purpose: Find all prime numbers from list *)
    (* sieve_list: int list -> int list *)
    let rec sieve_list lst = 
        (* Purpose: Remove divisible numbers by `divisor`. *)
        (* remove_divisible_numbers: int -> int list -> int list *)
        let remove_divisible_numbers divisor dividend_lst = List.filter (fun n -> n mod divisor != 0) dividend_lst in
        match lst with
              [] -> []
            | first :: rest -> first :: sieve_list (remove_divisible_numbers first rest)
    in sieve_list (from_two_to_n n)

(* tests *)
let t1 = sieve 120 
    = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113]
