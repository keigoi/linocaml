type 'a lin = Lin_Internal__ of 'a
type 'a data = Data of 'a
type empty = Empty
type ('a, 'b, 'pre, 'post) lens =
  {get: 'pre -> 'a; put: 'pre -> 'b -> 'post}

let compose
   : 'a 'b 'in1 'in2 'out1 'out2. ('a, 'b, 'in1, 'in2) lens -> ('in1, 'in2, 'out1, 'out2) lens -> ('a, 'b, 'out1, 'out2) lens =
  fun l1 l2 ->
  {get=(fun out1 -> l1.get (l2.get out1)); put=(fun out1 b -> l2.put out1 (l1.put (l2.get out1) b))}

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
    (unit -> (empty, empty, 'a data) monad)
    -> 'a IO.io
end

module LinMonadMake(IO:IO)
       : LIN_MONAD
       with module IO = IO
  = struct
  module IO = IO
  type ('pre, 'post, 'a) monad = {__m:('pre -> ('post * 'a) IO.io)}
  let return : 'a 'pre. 'a -> ('pre, 'pre, 'a data) monad = fun a -> {__m=(fun pre -> IO.return (pre, Data a))}
  let (>>=) : 'pre 'mid 'a 'post 'b. ('pre, 'mid, 'a data) monad
              -> ('a -> ('mid, 'post, 'b) monad)
              -> ('pre, 'post, 'b) monad = fun m f -> {__m=(fun pre -> IO.bind (m.__m pre) (fun (mid, Data a) -> (f a).__m mid))}
  let (>>) : 'pre 'mid 'a 'post 'b. ('pre, 'mid, 'a data) monad
             -> ('mid, 'post, 'b) monad
             -> ('pre, 'post, 'b) monad = fun m1 m2 -> {__m=(fun pre -> (IO.bind (m1.__m pre) (fun (mid, _) -> m2.__m mid)))}
  let run : 'a.
            (unit -> (empty, empty, 'a data) monad)
            -> 'a IO.io = fun f -> IO.bind ((f ()).__m Empty) (fun (_, Data x) -> IO.return x)
end

module type LENS = sig
  module LinMonad : LIN_MONAD
  open LinMonad
  type all_empty = [`cons of empty * 'xs] as 'xs
  val (@>) : ('p, 'q, 'a) monad
             -> ('p, 'q, 'pre, 'post) lens
             -> ('pre, 'post, 'a) monad
  val id : ('a, 'b, 'a, 'b) lens
  val _0 : ('a, 'b, [`cons of 'a * 'xs], [`cons of 'b * 'xs]) lens
  val _1 : ('a, 'b, [`cons of 'x1 * [`cons of 'a * 'xs]], [`cons of 'x1 * [`cons of 'b * 'xs]]) lens
  val _2 : ('a, 'b, [`cons of 'x1 * [`cons of 'x2 * [`cons of 'a * 'xs]]], [`cons of 'x1 * [`cons of 'x2 * [`cons of 'b * 'xs]]]) lens
  val succ : ('a, 'b, 'xs, 'ys) lens -> ('a, 'b, [`cons of 'x * 'xs], [`cons of 'x * 'ys]) lens
  val run': (unit -> (all_empty, all_empty, 'a data) monad) -> 'a LinMonad.IO.io

  val extend : ('pre, [`cons of empty * 'pre], unit data) monad
  val shrink : ([`cons of empty * 'pre], 'pre, unit data) monad
end

module LensMake(IO:IO)(M:LIN_MONAD with module IO = IO)
       : LENS with module LinMonad := M
  = struct
  open M
  type all_empty = [`cons of empty * 'xs] as 'xs
  let (@>) : 'p 'q 'pre 'post 'a.
             ('p, 'q, 'a) monad
             -> ('p, 'q, 'pre, 'post) lens
             -> ('pre, 'post, 'a) monad =
    fun m l ->
    {__m=(fun pre ->
       IO.bind (m.__m (l.get pre)) (fun (q, a) -> IO.return (l.put pre q, a)))}
  let id =
    {get=(fun a -> a); put=(fun _ b -> b)}
  let _0 =
    {get=(fun(`cons(a,_)) -> a); put=(fun(`cons(_,xs)) b -> `cons(b,xs))}
  let _1 =
    {get=(fun(`cons(_,`cons(a,_))) -> a); put=(fun(`cons(x,`cons(_,xs))) b -> `cons(x,`cons(b,xs)))}
  let _2 =
    {get=(fun(`cons(_,(`cons(_,`cons(a,_))))) -> a); put=(fun(`cons(x,`cons(y,`cons(_,xs)))) b -> `cons(x,`cons(y,`cons(b,xs))))}
  let succ l =
    {get=(fun(`cons(_,xs)) -> l.get xs); put=(fun(`cons(x,xs)) b -> `cons(x,l.put xs b))}
  let run' =
    fun f ->
    let rec all_empty = `cons(Empty, all_empty) in
    IO.bind ((f ()).__m all_empty) (fun (_, Data x) -> IO.return x)
  let extend : 'pre. ('pre, [`cons of empty * 'pre], unit data) monad =
    {__m=(fun pre ->
       IO.return (`cons(Empty, pre), Data ())
    )}
  let shrink : type pre. ([`cons of empty * pre], pre, unit data) monad =
    {__m=(fun (`cons(Empty,pre)) ->
       IO.return (pre, Data ())
    )}
end

module type LIN_MATCH = sig
  module LinMonad : LIN_MONAD
  open LinMonad
  type 'f bind
  val (>>-) : ('pre, 'mid, 'a lin) monad
              -> ('a lin -> ('mid, 'post, 'b) monad) bind
              -> ('pre, 'post, 'b) monad
  val return_lin : 'a -> ('p,'p,'a lin) monad
  val (!%) : (empty, 'b lin, 'c data) monad
             -> ('pre, 'pre, ('b lin * 'c data) lin) monad
  val (!%!) : (empty, 'b lin, 'c data) monad
             -> ('pre, 'pre, 'b lin) monad
  val (%>!) : ('a, 'b lin, 'c data) monad
              -> ('a, empty, 'pre, 'post) lens -> ('pre, 'post, 'b lin) monad
  val (%>) : ('a, 'b lin, 'c data) monad
              -> ('a, empty, 'pre, 'post) lens -> ('pre, 'post, ('b lin * 'c data) lin) monad
  val put_lin : (empty,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad
  val put_linval : (empty,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad
  val get_lin : ('a lin, empty, 'pre, 'post) lens -> ('pre,'post,'a lin) monad

  module Syntax : sig
    val bind_data : ('pre,'mid,'a data) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
    val bind_lin : ('pre,'mid,'a lin) monad -> ('a lin -> ('mid,'post,'b) monad) bind -> ('pre,'post,'b) monad
    val bind_raw : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) bind -> ('pre,'post,'b) monad
    val return_lin : 'a -> ('p,'p,'a lin) monad
    val get_lin : ('a lin, empty, 'pre, 'post) lens -> ('pre,'post,'a lin) monad
    val put_linval : (empty,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad
    val empty : empty

    module Internal : sig
      val _mkbind : 'f -> 'f bind
      val _run : ('pre,'post,'a data) monad -> 'pre -> 'a IO.io
      val _dispose_env : ('pre,'all_empty,'a) monad -> ('pre,empty,'a) monad
      val _peek : ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad
      val _poke : 'post -> ('pre, 'post, unit data) monad
      val _map_lin : ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
    end
  end
end

module LinMatchMake(IO:IO)(M:LIN_MONAD with module IO = IO)
       : LIN_MATCH with module LinMonad := M = struct
  open M
  type 'f bind = 'f

  let (>>-) : 'pre 'mid 'a 'post 'b. ('pre, 'mid, 'a lin) monad
              -> ('a lin -> ('mid, 'post, 'b) monad) bind
              -> ('pre, 'post, 'b) monad =
    fun m f ->
    {__m=(fun pre ->
       IO.bind (m.__m pre) (fun (mid, x) -> (f x).__m mid))}

  let return_lin : 'a 'p. 'a -> ('p,'p,'a lin) monad =
    fun v ->
    {__m=(fun pre ->
       IO.return (pre, Lin_Internal__ v))}

  let (!%) : 'a 'b 'c. (empty, 'b lin, 'c data) monad
             -> ('pre, 'pre, ('b lin * 'c data) lin) monad
    = fun m ->
    {__m=(fun pre ->
       IO.bind (m.__m Empty) (fun (b, c) -> IO.return (pre, Lin_Internal__ (b, c))))}

  let (!%!) : 'a 'b 'c. (empty, 'b lin, 'c data) monad
             -> ('pre, 'pre, 'b lin) monad
    = fun m ->
    {__m=(fun pre ->
       IO.bind (m.__m Empty) (fun (b, _) -> IO.return (pre, b)))}

  let (%>) : 'a 'pre 'post 'b 'c. ('a, 'b lin, 'c data) monad
              -> ('a, empty, 'pre, 'post) lens -> ('pre, 'post, ('b lin * 'c data) lin) monad
    = fun m l ->
    {__m=(fun pre ->
       IO.bind (m.__m (l.get pre)) (fun (b, c) ->
           IO.return (l.put pre Empty, Lin_Internal__ (b, c))))}

  let (%>!) : 'a 'pre 'post 'b 'c. ('a, 'b lin, 'c data) monad
              -> ('a, empty, 'pre, 'post) lens -> ('pre, 'post, 'b lin) monad
    = fun m l ->
    {__m=(fun pre ->
       IO.bind (m.__m (l.get pre)) (fun (b, _) ->
           IO.return (l.put pre Empty, b)))}


  let get_lin : 'a 'pre 'post. ('a lin, empty, 'pre, 'post) lens -> ('pre,'post,'a lin) monad =
    fun l ->
    {__m=(fun pre ->
       IO.return (l.put pre Empty, l.get pre))}

  let put_lin : 'a 'mid 'post 'pre. (empty,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad =
    fun l m ->
    {__m=(fun pre ->
       IO.bind (m.__m pre) (fun (mid, v) ->
           IO.return (l.put mid v, Data ())))}

  let put_linval : 'a 'pre 'post. (empty,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad =
    fun l v ->
    {__m=(fun pre ->
       IO.return (l.put pre (Lin_Internal__ v), Data ()))}

  module Syntax = struct
    let bind_data = (>>=)
    let bind_lin = (>>-)
    let bind_raw {__m=m} f = {__m=(fun pre -> IO.bind (m pre) (fun (mid,x) -> (f x).__m mid))}
    let return_lin = return_lin
    let get_lin = get_lin
    let put_linval = put_linval
    let empty = Empty

    module Internal = struct
      let _lin x = Lin_Internal__ x
      let _unlin (Lin_Internal__ x) = x
      let _mkbind : 'f. 'f -> 'f bind =
        fun f -> f
      let _run : 'pre 'post 'a. ('pre,'post,'a data) monad -> 'pre -> 'a IO.io =
        fun m pre ->
           IO.bind (m.__m pre) (fun (_, Data v) -> IO.return v)
      let _dispose_env m =
        {__m=(fun pre ->
           IO.bind (m.__m pre) (fun (_,a) -> IO.return (Empty, a)))}
      let _peek : 'pre 'post 'a. ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad =
        fun f ->
        {__m=(fun pre -> ((f pre).__m pre))}
      let _poke : 'pre 'post. 'post -> ('pre, 'post, unit data) monad =
        fun post ->
        {__m=(fun _ -> IO.return (post, Data ()))}
      let _map_lin : 'a 'b 'pre 'post. ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
        = fun f l ->
        {__m=fun pre -> IO.return (l.put pre @@ _lin (f (_unlin @@ l.get pre)), Data ())}
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

module Make(IO:IO) : S with module IO = IO = struct
  module LinMonad = LinMonadMake(IO)
  include LinMonad
  include LensMake(IO)(LinMonad)
  include LinMatchMake(IO)(LinMonad)
end
