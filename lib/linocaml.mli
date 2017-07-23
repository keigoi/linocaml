type ('pre,'post,'a) monad
type ('a, 'pre,'post,'b) bind

type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t
type empty = Empty

type 'a data = Data_Internal__ of 'a
type 'a lin  = Lin_Internal__ of 'a
   
val return : 'a -> ('p,'p,'a) monad
val bind : ('pre,'mid,'a) monad -> ('a, 'mid,'post,'b) bind -> ('pre,'post,'b) monad
val (>>=) : ('pre,'mid,'a) monad -> ('a, 'mid,'post,'b) bind -> ('pre,'post,'b) monad
val (>>) : ('pre,'mid,unit) monad -> ('mid,'post,'b) monad -> ('pre,'post,'b) monad

(* make a linval *)
val linret : 'a -> ('p,'p,'a lin) monad

(* get and put on slots *)
val get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a lin) monad
val put : (empty,'a lin,'mid,'post) slot -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit) monad

(* === utility functions === *)
val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
val map : ('a data lin, 'b data lin, 'pre, 'post) slot -> ('a -> 'b * 'c) -> ('pre, 'post, 'c) monad
val lin_split : ('p, ('a lin * 'b lin) lin, unit) monad -> ('p, 'a lin, 'b lin) monad

module Syntax : sig
  val bind : ('pre,'mid,'a) monad -> ('a, 'mid,'post,'b) bind -> ('pre,'post,'b) monad
  val return : 'a -> ('p,'p,'a) monad

  val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
  val empty : empty

  module Internal : sig
    val __mkbind : ('a -> ('pre,'post,'b) monad) -> ('a, 'pre, 'post, 'b) bind
    val __run : ('pre,'post,'a) monad -> 'pre -> 'a
    val __dispose_one : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
    val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) monad -> ('pre,empty,'a) monad
    val __peek : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad      
  end
end
     
module Internal : sig
  val __run : ('pre,'post,'a) monad -> 'pre -> 'a
  val __dispose_one : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
  val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) monad -> ('pre,empty,'a) monad
  val __peek : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad      
  val __monad : ('pre -> 'post * 'a) -> ('pre,'post,'a) monad
end
