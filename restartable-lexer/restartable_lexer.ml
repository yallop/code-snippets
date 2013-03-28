(* Using async to block waiting for input in an ocamllex-generated lexer *)

open Core.Std
open Async.Std

(* Indicate that there's nothing to read *)
exception Waiting

let tokenize : string Pipe.Reader.t -> lex:(Lexing.lexbuf -> 'token) -> unit -> 'token Deferred.t
  = fun reader ~lex ->
    (* Data remaining from previous reads.  This becomes None once we
       encounter EOF. *)
    let buffer : (int * string) option ref = ref (Some (0, "")) in

    (* Copy at most `max' bytes from `src' to `dst'; save the string and the
       position in `buffer' if there's a non-empty suffix. *)
    let store ~src ~src_pos ~dst ~max =
      let src_len = String.length src - src_pos in
      let len = min src_len max in
      String.blit ~src ~dst ~src_pos ~len ~dst_pos:0;
      if src_len > len then
        buffer := Some (src_pos + len, src)
      else
        buffer := Some (0, "");
      len
    in

    (* A function suitable for passing to Lexing.from_function *)
    let refill : string -> int -> int
      = fun dst max ->
        match !buffer with
          | Some (_, "") -> raise Waiting
          | Some (src_pos, src) -> store ~src ~src_pos ~dst ~max
          | None -> 0
    in

    let lexbuf = Lexing.from_function refill in

    let rec next () =
      (* Block until there's something to read; store it in `buffer' and call
         the tokenize function *)
      try return (lex lexbuf)
      with Waiting ->
        Pipe.read reader >>= function
          | `Eof -> buffer := None; next ()
          | `Ok s -> buffer := Some (0, s); next ()
    in
    next
;;

(* copy reader to writer *)
let copy_from reader writer =
  let buf = String.create 1024 in
  let rec loop () =
    Reader.read reader buf >>= function
      | `Ok len ->
        Pipe.write writer (String.sub buf ~pos:0 ~len) >>= loop
      | `Eof ->
        return ()
  in loop ()
    
(* Consume and print tokens as they arrive from the lexer *)
let print_tokens ~reader ~lex ~show_token =
  let tokenize = tokenize reader ~lex in
  let rec loop () =
    tokenize () >>= fun tok ->
    print_endline ("> " ^ show_token tok);
    loop ()
  in 
  loop ()
;;

let main () =  begin
  (* Used to send strings to the lexer *)
  let reader, writer = Pipe.create () in

  (* An async process that tokenises the reader end of the pipe *)
  let _ = Example.(print_tokens ~reader ~show_token ~lex) in
      
  (* An async process that copies stdin to the writer end of the pipe *)
  let _ = copy_from
    (Reader.of_in_channel stdin Async_unix.Fd.Kind.Char)
    writer
  in
  
  Scheduler.go ()
end

let _ = main ()
