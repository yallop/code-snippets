(* Rough-and-ready example of generating C and ML for a particular project. 

   Generating code is a case of instantiating the Example_bindings functor
   with interpretations of FOREIGN that generate C and ML stub.
*)

let ml_filename = "generated_example_bindings.ml"
let c_filename = "generated_example_stubs.c"

(* Generate the C code *)
let c_file = open_out c_filename

let () = output_string c_file "
#include <math.h>
#include <stdlib.h>
#include <ctype.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>

"

module CGen = Generators.Generate_C_stub(struct let write = output_string c_file end)
module C = Example_bindings.Bindings(CGen)

(* Generate the ML code *)
let ml_file = open_out ml_filename

module MLGen = Generators.Generate_ML_stub(struct let write = output_string ml_file end)
  
module ML = Example_bindings.Bindings(MLGen)
