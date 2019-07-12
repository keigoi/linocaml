open Base

module type IO = sig
  type +'a io
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io
end

module type LIN_MONAD = sig
  module IO : IO
  type ('pre, 'post, 'a) monad = {__m:('pre -> ('post * 'a) IO.io)}
  val return : 'a -> ('pre, 'pre, 'a data) monad
  val (>>=) : ('pre, 'mid, 'a data) monad
              -> ('a -> ('mid, 'post, 'b) monad)
              -> ('pre, 'post, 'b) monad
  val (>>) : ('pre, 'mid, 'a data) monad
             -> ('mid, 'post, 'b) monad
             -> ('pre, 'post, 'b) monad
  val run :
    (unit -> (unit, unit, 'a data) monad)
    -> 'a IO.io
end

module type LENS = sig
  module LinMonad : LIN_MONAD
  open LinMonad
  type all_empty = [`cons of unit * 'xs] as 'xs
  val (@>) : ('p, 'q, 'a) monad
             -> ('p, 'q, 'pre, 'post) lens
             -> ('pre, 'post, 'a) monad
  val (<@) : ('p, 'q, 'pre, 'post) lens
            -> ('p, 'q, 'a) monad
            -> ('pre, 'post, 'a) monad
  val _0 : ('a, 'b, [`cons of 'a * 'xs], [`cons of 'b * 'xs]) lens
  val _1 : ('a, 'b, [`cons of 'x1 * [`cons of 'a * 'xs]], [`cons of 'x1 * [`cons of 'b * 'xs]]) lens
  val _2 : ('a, 'b, [`cons of 'x1 * [`cons of 'x2 * [`cons of 'a * 'xs]]], [`cons of 'x1 * [`cons of 'x2 * [`cons of 'b * 'xs]]]) lens
  val run': ('a -> (all_empty, all_empty, 'b data) monad) -> 'a -> 'b LinMonad.IO.io

  val extend : ('pre, [`cons of unit * 'pre], unit data) monad
  val shrink : ([`cons of unit * 'pre], 'pre, unit data) monad
end

module type LIN_MATCH = sig
  module LinMonad : LIN_MONAD
  open LinMonad
  type 'f bind
  val (>>-) : ('pre, 'mid, 'a lin) monad
              -> ('a lin -> ('mid, 'post, 'b) monad) bind
              -> ('pre, 'post, 'b) monad
  val return_lin : 'a -> ('p,'p,'a lin) monad
  val (!%) : (unit, 'b lin, 'c data) monad
             -> ('pre, 'pre, ('b lin * 'c data) lin) monad
  val (!%!) : (unit, 'b lin, 'c data) monad
             -> ('pre, 'pre, 'b lin) monad
  val (%>!) : ('a, 'b lin, 'c data) monad
              -> ('a, unit, 'pre, 'post) lens -> ('pre, 'post, 'b lin) monad
  val (%>) : ('a, 'b lin, 'c data) monad
              -> ('a, unit, 'pre, 'post) lens -> ('pre, 'post, ('b lin * 'c data) lin) monad
  val put_lin : (unit,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad
  val put_linval : (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad
  val get_lin : ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad

  module Syntax : sig
    val bind_data : ('pre,'mid,'a data) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
    val bind_lin : ('pre,'mid,'a lin) monad -> ('a lin -> ('mid,'post,'b) monad) bind -> ('pre,'post,'b) monad
    val bind_raw : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) bind -> ('pre,'post,'b) monad
    val return_lin : 'a -> ('p,'p,'a lin) monad
    val get_lin : ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad
    val put_linval : (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad

    module Internal : sig
      val _mkbind : 'f -> 'f bind
      val _peek : ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad
      val _poke : 'post -> ('pre, 'post, unit data) monad
      val _map_lin : ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
    end
  end
end

module type S = sig
  module IO : IO
  include LIN_MONAD with module IO := IO
  include LENS
          with module LinMonad.IO = IO
          with type ('pre, 'post, 'a) LinMonad.monad = ('pre, 'post, 'a) monad
  include LIN_MATCH
          with module LinMonad := LinMonad
end
module type LIN_ARRAY = sig
  type 'a larr = 'a array lin

  module LinMonad : LIN_MONAD
  open LinMonad
  val alloc :
    'a list -> (unit, 'a larr, unit data) monad
  val dealloc :
    ('a larr, unit, unit data) monad
  val to_list :
    ('a larr, 'a larr, 'a list data) monad
  val lookup :
    int -> ('a larr, 'a larr, 'a data) monad
  val update :
    int -> 'a -> ('a larr, 'a larr, unit data) monad
  val map :
    ('a -> 'b) -> ('a larr, 'b larr, unit data) monad
  val mapM :
    ('a -> ('pre, 'pre, 'b data) monad) -> ('a larr, 'b larr, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
  val mapiM :
    (int -> 'a -> ('pre, 'pre, 'b data) monad) -> ('a larr, 'b larr, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
  val iterM :
    ('a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad
  val iteriM :
    (int -> 'a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad
end
