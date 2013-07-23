(** Registry of external functions. *)

val register : string -> Stubby_ctypes.boxed_function -> unit
(** Register an OCaml value under a global name. *)

val resolve : string -> Stubby_ctypes.boxed_function
(** Resolve an OCaml value by its registered name. *)
