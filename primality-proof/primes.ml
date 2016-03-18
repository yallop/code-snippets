type z = Z
type _ s = S

type (_,_,_) add =
    AddZ : (z, 'n, 'n) add
  | AddS : ('n, 'm, 'o) add -> ('n s, 'm, 'o s) add

type one = z s
type two = one s
type three = two s
type four = three s
type five = four s

type (_,_,_) mul =
    Mul1 : (one, 'n, 'n) mul
  (* If n * m == o 
     then Sn * m = o + m *)
  | MulN : ('n, 'm, 'o) mul *
           ('m, 'o, 'p) add ->
    ('n s, 'm, 'p) mul

(* If n is composite then
   there exist m >= 2 and o >= 2 such that
    m * o == n
*)
type 'n composite = Composite : (_ s s, _ s s, 'n) mul -> 'n composite

type absurd = {silly: 'a. 'a}

(* Primes are non-composite *)
type 'n prime = 'n composite -> absurd

type 'n classify =
    Prime : 'n prime -> 'n classify
  | Composite : 'n composite -> 'n classify

let exclusive : type n. n prime * n composite -> absurd = function
  prime, composite -> prime composite

type _ n =
    Z : z n
  | S : 'n n -> 'n s n

let two_is_prime : two prime = function
    Composite (MulN (Mul1, AddS (AddS _))) -> .
  | Composite (MulN (MulN _, AddS (AddS _))) -> .

let three_is_prime : three prime = function
    Composite (MulN (Mul1, AddS (AddS (AddS _)))) -> .
  | Composite (MulN (MulN (_, AddS _), AddS (AddS AddZ))) -> .
  | Composite (MulN (MulN (_, AddS _), AddS (AddS (AddS _)))) -> .

 (* It would be nice to be able to write this, perhaps with an
    annotation to force a deeper search:

       let three_is_prime : three prime = function Composite _ -> .
 *)


let four_is_composite : four composite =
  Composite (MulN (Mul1, AddS (AddS AddZ)))

 (* Perhaps we'll be able to write something like this soon:

     let four_is_composite : four composite = composite ()

    and have implicits fill in the gaps.
 *)
