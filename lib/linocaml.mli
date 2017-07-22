type ('pre,'post,'a) monad
type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t

type empty = Empty
type 'a data = Data_Internal__ of 'a
type 'a lin  = Lin_Internal__ of 'a
type ('pre,'post,'a) lin_match
   
val return : 'a -> ('p,'p,'a) monad
val bind : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
val (>>) : ('pre,'mid,unit) monad -> ('mid,'post,'b) monad -> ('pre,'post,'b) monad
val (>>=) : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad

val get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a lin) lin_match
val get2 : ('a lin,empty,'mid,'post) slot -> ('pre,'mid,unit) monad -> ('pre,'post,'a lin) lin_match
val set : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
val map : ('a data lin, 'b data lin, 'pre, 'post) slot -> ('a -> 'b * 'c) -> ('pre, 'post, 'c) monad
val lin : (unit -> 'a) -> ('p,'p,'a lin) lin_match
val lin_ : 'a -> ('p,'p,'a lin) lin_match

module Syntax : sig
  val bind : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
  val return : 'a -> ('p,'p,'a) monad
  val set : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad

  module Internal : sig
    val __unset : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
    val __get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad
    val __empty : empty
    val __run : ('pre,'post,'a) monad -> 'pre -> 'a
    val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) lin_match -> ('pre,empty,'a) lin_match
    val __match_in : ('pre,'post,'a lin) monad -> ('pre,'post,'a lin) lin_match
    val __match_out : ('pre,'post,'a lin) lin_match -> ('pre,'post,'a lin) monad
  end
end
     
module Internal : sig
  val __unset : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
  val __get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad
  val __empty : empty
  val __run : ('pre,'post,'a) monad -> 'pre -> 'a
  val __match_out : ('pre,'post,'a lin) lin_match -> ('pre,'post,'a lin) monad
  val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) lin_match -> ('pre,empty,'a) lin_match
  val __monad : ('pre -> 'post * 'a) -> ('pre,'post,'a) monad
  val __match_in : ('pre,'post,'a lin) monad -> ('pre,'post,'a lin) lin_match
  val __match_in2 : ('pre -> 'post * 'a lin) -> ('pre,'post,'a lin) lin_match
end
