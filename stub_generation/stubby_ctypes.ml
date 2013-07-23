(* Mini C-types interface.  First-order functions of ints and doubles, plus views. *)
type 'a typ =
  Int : int typ
| Double: float typ
| View : ('a -> 'b) * ('b -> 'a) * 'a typ -> 'b typ
type 'a fn =
  Returning : 'a typ -> 'a fn
| Function : 'a typ * 'b fn -> ('a -> 'b) fn

let rec string_of_typ : type a. a typ -> string = function
  | Int -> "Int"
  | Double -> "Double"
  | View (_, _, t) -> "View (<fun>, <fun>, "^ string_of_typ t ^")"

let rec string_of_fn : type a. a fn -> string = function
  | Returning t -> "Returning ("^ string_of_typ t ^")"
  | Function (t, f) -> "Function ("^ string_of_typ t ^", "^ string_of_fn f ^")"

type boxed_typ = Boxed_typ : 'a typ -> boxed_typ
type boxed_fn = Boxed_fn : 'a fn -> boxed_fn

type boxed_value = Boxed_value : 'a typ * 'a -> boxed_value
type boxed_function = Boxed_function : 'a fn * 'a -> boxed_function

(* strip out views *)
let rec deview_typ : type a. a typ -> boxed_typ = function
  | Int -> Boxed_typ Int
  | Double -> Boxed_typ Double
  | View (_, _, t) -> deview_typ t

let rec deview_fn : type a. a fn -> boxed_fn = function
  | Returning t ->
    let Boxed_typ t' = deview_typ t in
    Boxed_fn (Returning t')
  | Function (t, f) ->
    let Boxed_typ t' = deview_typ t in
    let Boxed_fn f' = deview_fn f in
    Boxed_fn (Function (t', f'))

let int = Int
let double = Double
let returning t = Returning t
let (@->) a b = Function (a, b)
let view ~read ~write t = View (read, write, t)

(* FOREIGN is a functor, so we can give it multiple interpretations. *)
module type FOREIGN =
sig
  type 'a value
  val foreign : string -> 'a fn -> 'a value
end
