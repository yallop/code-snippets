(** Three interpretations of the [foreign] function. *)

module Generate_C_stub (W : sig val write : string -> unit end)
  : Stubby_ctypes.FOREIGN
(** Interpret [foreign] as a generator of C stub code. *)

module Generate_ML_stub (W : sig val write : string -> unit end)
  : Stubby_ctypes.FOREIGN
(** Interpret [foreign] as a generator of ML [external] bindings to C code. *)

module Bind_stub
  : Stubby_ctypes.FOREIGN with type 'a value = 'a
(** Interpret [foreign] as a linker of generated ML [external] bindings. *)
