type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t
type empty = Empty

type 'a data = Data_Internal__ of 'a
type 'a lin  = Lin_Internal__ of 'a

(* BEGIN from Cohttp *)
(** The [IO] module defines the blocking interface for reading
    and writing to Cohttp streams *)
module type IO = sig

  (** ['a t] represents a blocking monad state *)
  type +'a io

  (** [a >>= b] will pass the result of [a] to the
      [b] function.  This is a monadic [bind]. *)
  val (>>=) : 'a io -> ('a -> 'b io) -> 'b io

  (** [return a] will construct a constant IO value. *)
  val return : 'a -> 'a io

  (** [ic] represents an input channel *)
  type ic

  (** [oc] represents an output channel *)
  type oc

  (** [read_line ic] will read a single line terminated
      by CR or CRLF from the input channel [ic].  It returns
      {!None} if EOF or other error condition is reached. *)
  val read_line : ic -> string option io

  (** [read ic len] will block until a maximum of [len] characters
      are read from the input channel [ic].  It returns an
      empty string if EOF or some other error condition occurs
      on the input channel, and can also return fewer than [len]
      characters if input buffering is not sufficient to satisfy the
      request. *)
  val read : ic -> int -> string io

  (** [write oc s] will block until the complete [s] string is
      written to the output channel [oc]. *)
  val write : oc -> string -> unit io

  (** [flush oc] will return when all previously buffered content
      from calling {!write} have been written to the output channel
      [oc]. *)
  val flush : oc -> unit io
end
(* END from Cohttp *)

module type LIN_IO = sig
  module IO : IO
  type ('pre,'post,'a) monad
  type ('a, 'pre,'post,'b) bindfun
     
  val return : 'a -> ('p,'p,'a) monad
  val lift : 'a IO.io -> ('p,'p,'a) monad
  
  (* bind is limited to type unit *)
  val bind : ('pre,'mid,unit) monad -> (unit -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
  val (>>=) : ('pre,'mid,unit) monad -> (unit -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
  val (>>) : ('pre,'mid,unit) monad -> ('mid,'post,'b) monad -> ('pre,'post,'b) monad
  
  (* extract a linval *)
  val linbind : ('pre,'mid,'a) monad -> ('a, 'mid,'post,'b) bindfun -> ('pre,'post,'b) monad
  val (>>>==) : ('pre,'mid,'a) monad -> ('a, 'mid,'post,'b) bindfun -> ('pre,'post,'b) monad
  
  (* make a linval *)
  val linret : 'a -> ('p,'p,'a lin) monad
  val linret_ : (unit -> 'a) -> ('p,'p,'a lin) monad
  
  (* get and put on slots *)
  val get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a lin) monad
  val put : (empty,'a lin,'mid,'post) slot -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit) monad
  
  (* === utility functions === *)
  val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
  val map : ('a data lin, 'b data lin, 'pre, 'post) slot -> ('a -> ('b * 'c) IO.io) -> ('pre, 'post, 'c) monad
  val lin_split : ('p, ('a lin * 'b lin) lin, unit) monad -> ('p, 'a lin, 'b lin) monad

  module Internal : sig
    val __monad : ('a -> ('b * 'c) IO.io) -> ('a, 'b, 'c) monad
  end
  
  module Syntax : sig
    val bind : ('pre,'mid,unit) monad -> (unit -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
    val linbind : ('pre,'mid,'a) monad -> ('a, 'mid,'post,'b) bindfun -> ('pre,'post,'b) monad
    val return : 'a -> ('p,'p,'a) monad
  
    val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
    val empty : empty
  
    module Internal : sig
      val __mkbindfun : ('a -> ('pre,'post,'b) monad) -> ('a, 'pre, 'post, 'b) bindfun
      val __run : ('pre,'post,'a) monad -> 'pre -> 'a IO.io
      val __takeval : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad      
      val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) monad -> ('pre,empty,'a) monad
    end
  end
end

module Make(M:IO): LIN_IO with module IO = M =
struct
  module IO = M

  type ('pre,'post,'a) monad = 'pre -> ('post * 'a) M.io
  type ('a,'pre,'post,'b) bindfun = 'a -> ('pre, 'post, 'b) monad
  
  let return a pre = M.return (pre, a)
  let lift m pre = IO.(>>=) m (fun x -> M.return (pre, x))
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
