type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t
type empty = Empty

type 'a data = Data of 'a
type 'a lin  = Lin_Internal__ of 'a

let o : ('a, 'b, 'a, 'b) slot = {Lens.get=(fun x -> x); put=(fun _ y -> y)}

module type IO = sig
  type +'a io
  val (>>=) : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io
end

module type LIN_IO = sig
  module IO : IO
  type ('pre,'post,'a) monad
  type 'f bind

  val return : 'a -> ('p,'p,'a lin) monad
  val lift : 'a IO.io -> ('p,'p,'a lin) monad

  (* extract a linval *)
  val bind : ('pre,'mid,'a lin) monad -> ('a lin -> ('mid,'post,'b lin) monad) bind -> ('pre,'post,'b lin) monad
  val (>>) : ('pre,'mid,unit lin) monad -> ('mid,'post,'b lin) monad -> ('pre,'post,'b lin) monad
  val (>>=) : ('pre,'mid,'a lin) monad -> ('a lin -> ('mid,'post,'b lin) monad) bind -> ('pre,'post,'b lin) monad

  val (^^) : ('p, 'q, 'pre, 'post) slot -> ('p, 'q, 'a) monad -> ('pre, 'post, 'a) monad
  val ( *! ) : ('p, 'r, 'mid, 'post) slot -> ('q, empty, 'pre, 'mid) slot -> ('p * 'q, 'r, 'pre, 'post) slot

  (* get a value from slot *)
  val get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a lin) monad

  (* put a linear value into slot *)
  val put : (empty,'a lin,'mid,'post) slot -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit lin) monad

  (* put a pure value into slot *)
  val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit lin) monad

  val lin_split : ('p, ('a lin * 'b lin) lin, unit lin) monad -> ('p, 'a lin, 'b lin) monad

  val run_o : ('a -> (empty, empty, 'b lin) monad) -> 'a -> 'b IO.io

  module Internal : sig
    val __monad : ('a -> ('b * 'c) IO.io) -> ('a, 'b, 'c) monad
  end

  module Syntax : sig
    val bind : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) bind -> ('pre,'post,'b) monad
    val empty : empty

    module Internal : sig
      val __bind_raw : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
      val __return_raw : 'a -> ('p,'p,'a) monad

      val __mkbindfun : ('a -> ('pre,'post,'b) monad) -> ('a -> ('pre, 'post, 'b) monad) bind

      val __putval_raw : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
      val __takeval_raw : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad

      val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) monad -> ('pre,empty,'a) monad
      val __run : ('pre,'post,'a lin) monad -> 'pre -> 'a IO.io
    end
  end
end

module Make(M:IO): LIN_IO with module IO = M =
struct
  module IO = M

  type ('pre,'post,'a) monad = 'pre -> ('post * 'a) M.io
  type 'f bind = 'f

  let return a pre = M.return (pre, Lin_Internal__ a)
  let lift m pre = IO.(>>=) m (fun x -> M.return (pre, (Lin_Internal__ x)))

  let bind f g pre = M.(>>=) (f pre) (fun (mid, a) -> g a mid)
  let (>>=) = bind
  let (>>) m n = m >>= (fun (Lin_Internal__ ()) -> n)

  open Lens

  let (^^) {get;put} m pre = M.(>>=) (m (get pre)) (fun (q, a) -> M.return (put pre q, a))

  let ( *! ) : 'a 'b 'pre 'mid 'c 'post. ('a, 'b, 'mid, 'post) slot -> ('c, empty, 'pre, 'mid) slot -> ('a * 'c, 'b, 'pre, 'post) slot =
    fun {get=get1;put=put1} {get=get2;put=put2} ->
    {get=(fun pre -> get1 (put2 pre Empty), get2 pre); put=(fun pre d -> put1 (put2 pre Empty) d)}

  let get {get;put} pre = M.return (put pre Empty, get pre)
  let put {put; _} m pre =
    M.(>>=) (m pre) (fun (mid, v) -> M.return (put mid v, Lin_Internal__ ()))

  let putval {put; _} a pre = M.return (put pre (Lin_Internal__ a), Lin_Internal__ ())

  let lin_split m pre =
    M.(>>=) (m pre) (fun (Lin_Internal__ (a, b), Lin_Internal__ ()) ->
    M.return (a, b))

  let run_o f x = IO.(>>=) (f x Empty) (fun (_,Lin_Internal__ x) -> IO.return x)

  module Internal = struct
    let __monad f = f
  end

  module Syntax = struct
    let bind = bind
    let empty = Empty

    module Internal = struct
      let __return_raw v pre = M.return (pre, v)
      let __bind_raw = fun m f pre -> M.(>>=) (m pre) (fun (mid,x) -> f x mid)

      let __putval_raw = fun {put; _} v pre ->
        M.return (put pre (Lin_Internal__ v), ())

      let __takeval_raw {get;put} pre =
        M.return (put pre Empty, match get pre with Lin_Internal__ a -> a)

      let __mkbindfun f = f
      let __run m pre = M.(>>=) (m pre) (fun (_,Lin_Internal__ a) -> M.return a)
      let __dispose_env m pre =
        M.(>>=) (m pre) (fun (_,a) -> M.return (Empty, a))
    end
  end
end
