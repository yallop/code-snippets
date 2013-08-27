(*
   Higher-order abstract syntax using open types.
*)

(* The type algebra of simply-typed lambda calculus *)
type _ typ = Int : int typ | Arr : 'a typ * 'b typ -> ('a -> 'b) typ
let (=>) a b = Arr (a, b)

(* Concrete expressions, interpreted in the model m *)
type ('a, 'model) cexp = ..

(* The signature of interpretations *)
module type Interpretation =
sig
  type model = T (* The name of the model *)
  type 'a t      (* The type of the interpretation *)

  (* The term language of STLC *)
  val const : int -> (int, model) cexp
  val app : ('a -> 'b, model) cexp -> ('a, model) cexp -> ('b, model) cexp
  val abs : 'a typ -> (('a, model) cexp -> ('b, model) cexp) -> ('a -> 'b, model) cexp

  (* Extraction of a value from a concrete expression *)
  val run : ('a, model) cexp -> 'a t
end

(* First-class interpretations *)
type 'm interpretation = (module Interpretation with type model = 'm)

(* An expression is a concrete expression parameterized by its interpretation *)
type ('a, 'm) exp = 'm interpretation -> ('a, 'm) cexp

(* Top-level term constructors for STLC *)
let const : type m. int -> (int, m) exp
  = fun (type m) c (module I : Interpretation with type model = m)
    -> I.const c
              
let app : type a b m. (a -> b, m) exp -> (a, m) exp -> (b, m) exp
  = fun (type m) f p ((module I : Interpretation with type model = m) as i)
    -> I.app (f i)  (p i)

let abs : type a b m. a typ -> ((a, m) exp -> (b, m) exp) -> (a -> b, m) exp
  = fun (type m) ty f ((module I : Interpretation with type model = m) as i)
    -> I.abs ty (fun x -> f (fun _ -> x) i)


(* A simplified interpretation signature, without the model flag *)
module type Basic_interpretation =
sig
  type 'a cexp
  val const : int -> int cexp
  val app : ('a -> 'b) cexp -> 'a cexp -> 'b cexp
  val abs : 'a typ -> ('a cexp -> 'b cexp) -> ('a -> 'b) cexp
end

(* Turn a simplified signature into a full interpretation *)
module Interpret (B : Basic_interpretation) : Interpretation
  with type 'a t = 'a B.cexp =
struct
  type model = T
  type 'a t = 'a B.cexp
  type (_, _) cexp += T : 'a t -> ('a, model) cexp
  let run (T t) = t
  let const c = T (B.const c)
  let app (T f) (T p) = T (B.app f p)
  let abs typ f = T (B.abs typ (fun x -> run (f (T x))))
end

(* The size interpretation *)
module Size_interpretation = Interpret (struct
  type 'a cexp = int
  let const _ = 1
  let app f p = 1 + f + p
  let abs _ f = 1 + (f 0)
end
)
type size = Size_interpretation.model
let size_model =
  (module Size_interpretation : Interpretation with type model = size)

(* The evaluation interpretation *)
module Eval_interpretation = Interpret (struct
  type 'a cexp = 'a
  let id x = x
  let const = id and app = id and abs _ = id
end
)
type eval = Eval_interpretation.model
let eval_model =
  (module Eval_interpretation : Interpretation with type model = eval)



let size exp : int = Size_interpretation.run (exp size_model)

let eval (type a) (exp : (a, _) exp) : a =
  Eval_interpretation.run (exp eval_model)

let tests () = begin
  let t1 = abs (Int => Int) (fun f -> app f (const 3)) in

  assert ((eval t1) succ = 4);

  let t2 = abs (Int => Int) (fun f -> app f (const 4)) in

  assert (size t2 = 3);

  print_endline "tests passed"
end

