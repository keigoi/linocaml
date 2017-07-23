type ('pre,'post,'a) monad
type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t

type empty = Empty
type 'a data = Data_Internal__ of 'a
type 'a lin  = Lin_Internal__ of 'a
type ('pre,'post,'a) linval
   
val return : 'a -> ('p,'p,'a) monad
val bind : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
val (>>) : ('pre,'mid,unit) monad -> ('mid,'post,'b) monad -> ('pre,'post,'b) monad
val (>>=) : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad

(* make a linval *)
val linval : (unit -> 'a) -> ('p,'p,'a lin) linval
val linval_ : 'a -> ('p,'p,'a lin) linval

(* get and put on slots *)
val get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a lin) linval
val put : (empty,'a lin,'mid,'post) slot -> ('pre,'mid,'a lin) linval -> ('pre,'post,unit) monad

(* === utility functions === *)
val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
val map : ('a data lin, 'b data lin, 'pre, 'post) slot -> ('a -> 'b * 'c) -> ('pre, 'post, 'c) monad
val lin_split : ('p, ('a lin * 'b lin) lin, unit) monad -> ('p, 'a lin, 'b lin) linval

module Syntax : sig
  val bind : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
  val return : 'a -> ('p,'p,'a) monad

  val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
  val empty : empty

  module Internal : sig
    val __run : ('pre,'post,'a) monad -> 'pre -> 'a
    val __dispose_one : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
    val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) linval -> ('pre,empty,'a) linval
    val __peek : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad      
    val __linval_in : ('pre,'post,'a lin) monad -> ('pre,'post,'a lin) linval
    val __linval_out : ('pre,'post,'a lin) linval -> ('pre,'post,'a lin) monad
  end
end
     
module Internal : sig
  val __run : ('pre,'post,'a) monad -> 'pre -> 'a
  val __dispose_one : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
  val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) linval -> ('pre,empty,'a) linval
  val __peek : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad      
  val __linval_in : ('pre,'post,'a lin) monad -> ('pre,'post,'a lin) linval
  val __linval_out : ('pre,'post,'a lin) linval -> ('pre,'post,'a lin) monad
  val __monad : ('pre -> 'post * 'a) -> ('pre,'post,'a) monad
end
