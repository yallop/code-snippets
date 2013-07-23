open Stubby_ctypes

(* A few views. *)
let bool = view ~read:((<>)0) ~write:(fun b -> compare b false) int
let int32 = view ~read:Int32.of_int ~write:Int32.to_int int
let char = view ~read:Char.chr ~write:Char.code int

(* Bindings, parameterised over the interpretation of `foreign'.  We'll
   instantiate F multiple times, to generate C and OCaml stubs, and to
   link in the stubs with the appropriate types.
*)
module Bindings (F : FOREIGN) =
struct
  open F

  let ldexp = foreign "ldexp" (double @-> int @-> returning double)
  let abs32 = foreign "abs" (int32 @-> returning int32)
  let isalpha = foreign "isalpha" (char @-> returning bool)
end
