include Linocaml_base

module Make(M:S.IO)
       : S.IO_LIN with type 'a io = 'a M.t
                                                 = struct
  open Linocaml_base

  type +'a io = 'a M.t
  type ('pre,'post,'a) monad = 'pre -> ('post * 'a) io
  type ('a,'pre,'post,'b) bindfun = 'a -> ('pre, 'post, 'b) monad
  
  
  let return a pre = M.return (pre, a)
  let bind f g pre = M.(>>=) (f pre) (fun (mid, ()) -> g () mid)
  let (>>=) = bind
  let (>>) m n = m >>= (fun () -> n)
  
  let linbind f g pre = M.(>>=) (f pre) (fun (mid, a) -> g a mid)
  let (>>>==) = linbind
  
  let linret a pre = M.return (pre, (Lin_Internal__ a))
  let linret_ f pre = M.return (pre, (Lin_Internal__ (f ())))
  
  open Lens
  
  let get {get;put} pre = M.return (put pre Empty, get pre)
  let put {get;put} m pre =
    M.(>>=) (m pre) (fun (mid, v) -> M.return (put mid v, ()))
  
  let putval {put} a pre = M.return (put pre (Lin_Internal__ a), ())
  
  let map {get;put} f pre =
    let Lin_Internal__ (Data_Internal__ a) = get pre in
    M.(>>=) (f a) (fun (b,c) ->
    M.return (put pre (Lin_Internal__ (Data_Internal__ b)), c))
  
  let lin_split m pre =
    M.(>>=) (m pre) (fun (Lin_Internal__ (a, b), ()) ->
    M.return (a, b))
  
  
  module Internal = struct
    let __run m pre = M.(>>=) (m pre) (fun (_,a) -> M.return a)
    let __dispose_one {put} pre = M.return (put pre Empty, ())
    let __dispose_env m pre =
      M.(>>=) (__run m pre) (fun a -> M.return (Empty, a))
    let __takeval {get;put} pre = M.return (put pre Empty, match get pre with Lin_Internal__ a -> a)
    let __monad f = f
  end
  
  module Syntax = struct
    let return = return
    let bind = bind
    let linbind = linbind
    let putval = putval
    let empty = Empty
  
    module Internal = struct
      let __mkbindfun f = f
      let __run = Internal.__run
      let __dispose_one = Internal.__dispose_one
      let __dispose_env = Internal.__dispose_env
      let __takeval = Internal.__takeval
    end
  end
end

module Std = Make(Io_std)
module Lwt = Make(Io_lwt)
