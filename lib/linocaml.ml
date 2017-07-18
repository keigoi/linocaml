type ('pre,'post,'a) monad = 'pre -> 'post * 'a
type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t

type empty = Empty
type 'a lin = Lin__ of 'a
type 'a data = Data__ of 'a
type ('pre,'post,'a) lin_match = ('pre,'post,'a) monad

let return a pre = pre, a
let bind f g pre = match f pre with mid, a -> g a mid
let (>>=) = bind
let (>>) m n = m >>= (fun () -> n)

open Lens
let get {get;set} pre = set pre Empty, get pre
let set {set} a pre = set pre (Lin__ a), ()
let lin f pre = pre, Lin__ (f ())
let lin_ a pre = pre, Lin__ a

module Internal = struct
  let __unset {set} pre = set pre Empty, ()
  let __get {get;set} pre = set pre Empty, match get pre with Lin__ a -> a
  let __empty = Empty
  let __run m pre = snd (m pre)
  let __monad f = f
  let __match_out m = m
  let __match_in m = m
  let __match_in2 m = m
end

module Syntax = struct
  let bind = (>>=)
  let set = set

  module Internal = struct
    let __unset = Internal.__unset
    let __get = Internal.__get
    let __empty = Internal.__empty
    let __run = Internal.__run
    let __match_out = Internal.__match_out
  end
end
