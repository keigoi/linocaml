open Linocaml_base

(* BEGIN from Cohttp *)
(** The [IO] module defines the blocking interface for reading
    and writing to Cohttp streams *)
module type IO = sig

  (** ['a t] represents a blocking monad state *)
  type +'a t

  (** [a >>= b] will pass the result of [a] to the
      [b] function.  This is a monadic [bind]. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** [return a] will construct a constant IO value. *)
  val return : 'a -> 'a t

  (** [ic] represents an input channel *)
  type ic

  (** [oc] represents an output channel *)
  type oc

  (** [read_line ic] will read a single line terminated
      by CR or CRLF from the input channel [ic].  It returns
      {!None} if EOF or other error condition is reached. *)
  val read_line : ic -> string option t

  (** [read ic len] will block until a maximum of [len] characters
      are read from the input channel [ic].  It returns an
      empty string if EOF or some other error condition occurs
      on the input channel, and can also return fewer than [len]
      characters if input buffering is not sufficient to satisfy the
      request. *)
  val read : ic -> int -> string t

  (** [write oc s] will block until the complete [s] string is
      written to the output channel [oc]. *)
  val write : oc -> string -> unit t

  (** [flush oc] will return when all previously buffered content
      from calling {!write} have been written to the output channel
      [oc]. *)
  val flush : oc -> unit t
end
(* END from Cohttp *)

module type IO_LIN = sig
  type +'a io
  type ('pre,'post,'a) monad
  type ('a, 'pre,'post,'b) bindfun
     
  val return : 'a -> ('p,'p,'a) monad
  
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
  val map : ('a data lin, 'b data lin, 'pre, 'post) slot -> ('a -> ('b * 'c) io) -> ('pre, 'post, 'c) monad
  val lin_split : ('p, ('a lin * 'b lin) lin, unit) monad -> ('p, 'a lin, 'b lin) monad
  
  module Syntax : sig
    val bind : ('pre,'mid,unit) monad -> (unit -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
    val linbind : ('pre,'mid,'a) monad -> ('a, 'mid,'post,'b) bindfun -> ('pre,'post,'b) monad
    val return : 'a -> ('p,'p,'a) monad
  
    val putval : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) monad
    val empty : empty
  
    module Internal : sig
      val __mkbindfun : ('a -> ('pre,'post,'b) monad) -> ('a, 'pre, 'post, 'b) bindfun
      val __run : ('pre,'post,'a) monad -> 'pre -> 'a io
      val __takeval : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) monad      
      val __dispose_env : ('pre,'this_shoulb_be_all_empty,'a) monad -> ('pre,empty,'a) monad
    end
  end
end
