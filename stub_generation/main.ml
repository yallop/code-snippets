open Printf

include Example_bindings.Bindings(Generators.Bind_stub)

(* Call various foreign functions. *)
let () = begin
  printf "isalpha 'c' => %b\n" (isalpha 'c');
  printf "isalpha '3' => %b\n" (isalpha '3');
  printf "abs32 (-4l) => %ld\n" (abs32 (-4l));
  printf "abs32 5l => %ld\n" (abs32 5l);
  printf "ldexp 4.1 2 => %f\n" (ldexp 4.1 2);
end
