type ('pre,'post,'a) monad
type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t

type empty
type 'a lin = Lin__ of 'a
type 'a data = Data__ of 'a
type ('pre,'post,'a) lin_match
   
val return : 'a -> ('p,'p,'a) monad
val bind : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
val (>>) : ('pre,'mid,unit) monad -> ('mid,'post,'b) monad -> ('pre,'post,'b) monad
val (>>=) : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad

val get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) lin_match
val set : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad

module Syntax : sig
  val bind : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
  val set : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad

  module Internal : sig
    val __unset : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
    val __get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad
    val __empty : empty
    val __run : ('pre,'post,'a) monad -> 'pre -> 'a
    val __match_out : ('pre,'post,'a) lin_match -> ('pre,'post,'a lin) monad
  end
end
     
module Internal : sig
  val __unset : ('a,empty,'pre,'post) slot -> ('pre,'post,unit) monad
  val __get : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad
  val __empty : empty
  val __run : ('pre,'post,'a) monad -> 'pre -> 'a
  val __match_out : ('pre,'post,'a) lin_match -> ('pre,'post,'a lin) monad
    
  val __monad : ('pre -> 'post * 'a) -> ('pre,'post,'a) monad
  val __match_in : ('pre,'post,'a lin) monad -> ('pre,'post,'a) lin_match
  val __match_in2 : ('pre -> 'post * 'a lin) -> ('pre,'post,'a) lin_match
end
