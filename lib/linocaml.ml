type ('pre,'post,'a) monad = 'pre -> 'post * 'a
type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t

type empty = Empty
type 'a lin = Lin of 'a
type 'a data = Data of 'a

let return a pre = pre, a
let bind f g pre = match f pre with mid, a -> g a mid
let (>>=) = bind
let (>>) m n = m >>= (fun () -> n)


module Syntax = struct
  open Lens
  let bind = (>>=)
  let set {set} b pre = set pre (Lin b), ()

  module Internal = struct
    let __unset {set} pre = set pre Empty, ()
    let __get {get;set} pre = set pre Empty, match get pre with Lin a -> a
    let __empty = Empty
    let __run m pre = snd (m pre)
  end
end

           
