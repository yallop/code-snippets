open Stubby_ctypes

(* Three interpretations of the [foreign] function. *)

(* Utility function: destruct [typ] values, passing through views *)
let rec case : type a b. a typ -> int:b -> double:b -> b =
  fun t ~int ~double -> match t with
  | Int -> int
  | Double -> double
  | View (_, _, t) -> case t ~int ~double

(* Interpret [foreign] as a generator of C stub code. *)
module Generate_C_stub (W : sig val write : string -> unit end) :
  FOREIGN with type 'a value = unit =
struct
  open Printf

  type 'a value = unit

  let reader = case ~int:"Val_int" ~double:"caml_copy_double"
  let writer = case ~int:"Int_val" ~double:"Double_val"

  let body name fn =
    let rec aux : type a. string list -> int -> a fn -> string =
      fun es i -> function
      | Returning t -> sprintf "%s(%s(%s))" (reader t) name
        (String.concat ", " es)
      | Function (t, f) -> aux (es @ [sprintf "%s(x%d)" (writer t) i]) (i + 1) f
    in sprintf "return %s;" (aux [] 1 fn)

  let prototype name fn =
    let rec aux : type a. string list -> int -> a fn -> string =
      fun es i -> function
      | Returning t -> sprintf "value ctypes_%s(%s)" name
        (String.concat ", " es)
      | Function (t, f) -> aux (es @ [sprintf "value x%d" i]) (i + 1) f
    in aux [] 1 fn

  let foreign name fn =
    W.write (sprintf "%s\n{\n  %s\n}\n" (prototype name fn) (body name fn))
end


(* Interpret [foreign] as a generator of ML [external] bindings to C code. *)
module Generate_ML_stub (W : sig val write : string -> unit end) :
  FOREIGN with type 'a value = unit =
struct
  open Printf

  type 'a value = unit

  let ml_name = case ~int:"int" ~double:"float"

  let binding name fn =
    let rec aux : type a. string list -> a fn -> string =
      fun es -> function
      | Returning t -> sprintf "external ctypes_%s : %s -> %s = \"ctypes_%s\""
        name (String.concat "-> " es) (ml_name t) name
      | Function (t, f) -> aux (es @ [ml_name t]) f
    in aux [] fn

  let foreign name fn =
    let Boxed_fn fn' = deview_fn fn in
    W.write (sprintf
               "%s\nlet () = External_functions.register \"%s\"
   (Stubby_ctypes.(Boxed_function (%s, ctypes_%s)))\n"
               (binding name fn) name (string_of_fn fn') name)
end

(* Interpret [foreign] as a linker of generated ML [external] bindings. *)
module Bind_stub :
  FOREIGN with type 'a value = 'a =
struct
  open Printf

  type 'a value = 'a

  let id x = x

  let rec coerce_value : type a b. a typ -> b typ -> (a -> b) option =
    fun typ typ' -> match typ, typ' with
      Int, Int -> Some id
    | Double, Double -> Some id
    | t, View (f, _, t') ->
      begin match coerce_value t t' with
      | Some v -> Some (fun x -> f (v x))
      | None -> None
      end
    | _ -> None

  let rec coerce_value' : type a b. a typ -> b typ -> (b -> a) option =
    fun typ typ' -> match typ, typ' with
      Int, Int -> Some id
    | Double, Double -> Some id
    | t, View (_, g, t') ->
      begin match coerce_value' t t' with
      | Some v -> Some (fun x -> v (g x))
      | None -> None
      end
    | _ -> None

  let rec coerce_function : type a b. a fn -> b fn -> (a -> b) option =
    fun fn fn' -> match fn, fn' with
      Returning t, Returning t' -> coerce_value t t'
    | Function (t, f), Function (t', f') ->
      begin match coerce_value' t t' with
      | Some coerce ->
        begin match coerce_function f f' with
        | Some coerce_fn ->
          Some (fun a x -> coerce_fn (a (coerce x)))
        | None -> None
        end
      | None -> None
      end
    | _ -> None

  let foreign name fn' =
    let Boxed_function (fn, f) = External_functions.resolve name in
    match coerce_function fn fn' with
    | Some g -> g f
    | None -> 
      let Boxed_fn fn' = deview_fn fn' in
      failwith (Printf.sprintf
                  "\
Linking error: the external value %s has type %s \
but was expected to have type %s"
                  name (string_of_fn fn) (string_of_fn fn'))
                           
end
