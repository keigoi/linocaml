type ('pre,'post,'a) monad = 'pre -> 'post * 'a
type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t

type empty = Empty
type 'a data = Data_Internal__ of 'a
type 'a lin  = Lin_Internal__ of 'a
type ('pre,'post,'a) linval = ('pre,'post,'a) monad

let return a pre = pre, a
let bind f g pre = match f pre with mid, a -> g a mid
let (>>=) = bind
let (>>) m n = m >>= (fun () -> n)

let linval f pre = pre, Lin_Internal__ (f ())
let linval_ a pre = pre, Lin_Internal__ a

open Lens

let get {get;put} pre = put pre Empty, get pre
let put {get;put} m pre =
  let mid, v = m pre in
  put mid v, ()

let putval {put} a pre = put pre (Lin_Internal__ a), ()

let map {get;put} f pre =
  let Lin_Internal__ (Data_Internal__ a) = get pre in
  let b,c = f a in
  put pre (Lin_Internal__ (Data_Internal__ b)), c

let lin_split m pre =
  let Lin_Internal__ (a, b), () = m pre in
  a, b

module Internal = struct
  let __run m pre = snd (m pre)
  let __dispose_one {put} pre = put pre Empty, ()
  let __dispose_env m pre =
    let a = __run m pre in Empty, a
  let __peek {get;put} pre = put pre Empty, match get pre with Lin_Internal__ a -> a
  let __linval_out m = m
  let __linval_in m = m
  let __monad f = f
end

module Syntax = struct
  let return = return
  let bind = (>>=)
  let putval = putval
  let empty = Empty

  module Internal = struct
    let __run = Internal.__run
    let __dispose_one = Internal.__dispose_one
    let __dispose_env = Internal.__dispose_env
    let __peek = Internal.__peek
    let __linval_in = Internal.__linval_in
    let __linval_out = Internal.__linval_out
  end
end
