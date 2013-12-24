module type POS = sig type +'a t end
module type NEG = sig type -'a t end

module type SubPos =
sig
  type a and b
  module Coerce (Pos : POS) :
  sig val cast : a Pos.t -> b Pos.t end
end

module type SubNeg =
sig
  type a and b
  module Coerce (Neg : NEG) :
  sig val cast : b Neg.t -> a Neg.t end
end

type ('a, 'b) sub = (module SubPos with type a = 'a and type b = 'b)

module FlipPos (S : SubPos)  : SubNeg with type a = S.a and type b = S.b =
struct
  type a = S.a and b = S.b
  module Coerce (Neg : NEG) =
  struct
    module Coerce = S.Coerce(struct type 'a t = 'a Neg.t -> a Neg.t end)
    let cast  = Coerce.cast (fun x -> x)
  end
end

module FlipNeg (S : SubNeg) : SubPos with type a = S.a and type b = S.b =
struct
  type a = S.a and b = S.b
  module Coerce (Pos : POS) =
  struct
    module Coerce = S.Coerce(struct type 'a t = 'a Pos.t -> b Pos.t end)
    let cast  = Coerce.cast (fun x -> x)
  end
end

module SubstPos (Pos : POS) =
struct
  let cast (type s) (type t)
      (module Sub : SubPos with type a = s and type b = t)
      = let module S = Sub.Coerce(Pos) in S.cast
end

module SubstNeg (Neg : NEG) =
struct
  let cast (type s) (type t)
      (module Sub : SubPos with type a = s and type b = t)
      = let module N = FlipPos(Sub) in
        let module S = N.Coerce(Neg) in S.cast
end

let refl (type t) : (t, t) sub =
  (module
   struct
     type a = t and b = t
     module Coerce (Pos : POS) =
     struct
       let cast x = x
     end
   end)

module CastId = SubstPos(struct type 'a t = 'a end)

let cast : type s t. (s, t) sub -> s -> t = CastId.cast
             
let trans : type r s t. (r, s) sub -> (s, t) sub -> (r, t) sub =
  fun (module Sub_rs) (module Sub_st) ->
    (module struct
      type a = r and b = t
      module Coerce(Pos : POS) =
      struct
        module RS = Sub_rs.Coerce(Pos)
        module ST = Sub_st.Coerce(Pos)
        let cast x = ST.cast (RS.cast x)
      end
    end)

module LiftPlus(Pos : POS) =
struct
  let lift : type s t. (s, t) sub -> (s Pos.t, t Pos.t) sub =
    fun (module Sub_st) ->
      (module
       struct
         type a = s Pos.t and b = t Pos.t
         module Coerce (P : POS) =
           Sub_st.Coerce (struct type +'a t = 'a Pos.t P.t end)
       end)
end

module LiftMinus(Neg : NEG) =
struct
  let lift : type s t. (s, t) sub -> (t Neg.t, s Neg.t) sub =
    fun (module Sub_st) ->
      (module
       struct
         type a = t Neg.t and b = s Neg.t
         module N = FlipPos (Sub_st)
         module Coerce(Pos : POS) =
           N.Coerce(struct type 'a t = 'a Neg.t Pos.t end)
       end)
end

type ('a, 'b) sup = ('b, 'a) sub
