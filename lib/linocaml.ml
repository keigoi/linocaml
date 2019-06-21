module Base = Base
include Base

module LinMonadMake(IO:S.IO)
       : S.LIN_MONAD
       with module IO = IO
  = struct
  module IO = IO
  type ('pre, 'post, 'a) monad = {__m:('pre -> ('post * 'a) IO.io)}
  let return a =
    {__m=(fun pre -> IO.return (pre, {data=a}))}
  let (>>=) m f =
    {__m=(fun pre ->
       IO.bind (m.__m pre) (fun (mid, {data=a}) -> (f a).__m mid))}
  let (>>) m1 m2 =
    {__m=(fun pre ->
       IO.bind (m1.__m pre) (fun (mid, _) -> m2.__m mid))}
  let run f =
    IO.bind ((f ()).__m ()) (fun (_, {data=x}) -> IO.return x)
end

module LensMake(IO:S.IO)(M:S.LIN_MONAD with module IO = IO)
       : S.LENS with module LinMonad := M
  = struct
  open M
  type all_empty = [`cons of unit * 'xs] as 'xs
  let (@>) : 'p 'q 'pre 'post 'a.
             ('p, 'q, 'a) monad
             -> ('p, 'q, 'pre, 'post) lens
             -> ('pre, 'post, 'a) monad =
    fun m l ->
    {__m=(fun pre ->
       IO.bind (m.__m (lens_get l pre)) (fun (q, a) -> IO.return (lens_put l pre q, a)))}
  let _0 = Zero
  let _1 = Succ Zero
  let _2 = Succ (Succ Zero)
  let run' =
    fun f x ->
    let rec all_empty = `cons((), all_empty) in
    IO.bind ((f x).__m all_empty) (fun (_, {data=x}) -> IO.return x)
  let extend : 'pre. ('pre, [`cons of unit * 'pre], unit data) monad =
    {__m=(fun pre ->
       IO.return (`cons((), pre), {data=()})
    )}
  let shrink : type pre. ([`cons of unit * pre], pre, unit data) monad =
    {__m=(fun (`cons((),pre)) ->
       IO.return (pre, {data=()})
    )}
end

module LinMatchMake(IO:S.IO)(M:S.LIN_MONAD with module IO = IO)
       : S.LIN_MATCH with module LinMonad := M = struct
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
       IO.return (pre, {__lin=v}))}

  let (!%) : 'a 'b 'c. (unit, 'b lin, 'c data) monad
             -> ('pre, 'pre, ('b lin * 'c data) lin) monad
    = fun m ->
    {__m=(fun pre ->
       IO.bind (m.__m ()) (fun (b, c) -> IO.return (pre, {__lin=(b, c)})))}

  let (!%!) : 'a 'b 'c. (unit, 'b lin, 'c data) monad
             -> ('pre, 'pre, 'b lin) monad
    = fun m ->
    {__m=(fun pre ->
       IO.bind (m.__m ()) (fun (b, _) -> IO.return (pre, b)))}

  let (%>) : 'a 'pre 'post 'b 'c. ('a, 'b lin, 'c data) monad
              -> ('a, unit, 'pre, 'post) lens -> ('pre, 'post, ('b lin * 'c data) lin) monad
    = fun m l ->
    {__m=(fun pre ->
       IO.bind (m.__m (lens_get l pre)) (fun (b, c) ->
           IO.return (lens_put l pre (), {__lin=(b, c)})))}

  let (%>!) : 'a 'pre 'post 'b 'c. ('a, 'b lin, 'c data) monad
              -> ('a, unit, 'pre, 'post) lens -> ('pre, 'post, 'b lin) monad
    = fun m l ->
    {__m=(fun pre ->
       IO.bind (m.__m (lens_get l pre)) (fun (b, _) ->
           IO.return (lens_put l pre (), b)))}


  let get_lin : 'a 'pre 'post. ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad =
    fun l ->
    {__m=(fun pre ->
       IO.return (lens_put l pre (), lens_get l pre))}

  let put_lin : 'a 'mid 'post 'pre. (unit,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad =
    fun l m ->
    {__m=(fun pre ->
       IO.bind (m.__m pre) (fun (mid, v) ->
           IO.return (lens_put l mid v, {data=()})))}

  let put_linval : 'a 'pre 'post. (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad =
    fun l v ->
    {__m=(fun pre ->
       IO.return (lens_put l pre {__lin=v}, {data=()}))}

  module Syntax = struct
    let bind_data = (>>=)
    let bind_lin = (>>-)
    let bind_raw {__m=m} f = {__m=(fun pre -> IO.bind (m pre) (fun (mid,x) -> (f x).__m mid))}
    let return_lin = return_lin
    let get_lin = get_lin
    let put_linval = put_linval

    module Internal = struct
      let _lin x = {__lin=x}
      let _unlin ({__lin=x}) = x
      let _mkbind : 'f. 'f -> 'f bind =
        fun f -> f
      let _run : 'pre 'post 'a. ('pre,'post,'a data) monad -> 'pre -> 'a IO.io =
        fun m pre ->
           IO.bind (m.__m pre) (fun (_, {data=v}) -> IO.return v)
      let _dispose_env m =
        {__m=(fun pre ->
           IO.bind (m.__m pre) (fun (_,a) -> IO.return ((), a)))}
      let _peek : 'pre 'post 'a. ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad =
        fun f ->
        {__m=(fun pre -> ((f pre).__m pre))}
      let _poke : 'pre 'post. 'post -> ('pre, 'post, unit data) monad =
        fun post ->
        {__m=(fun _ -> IO.return (post, {data=()}))}
      let _map_lin : 'a 'b 'pre 'post. ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
        = fun f l ->
        {__m=fun pre -> IO.return (lens_put l pre @@ _lin (f (_unlin @@ lens_get l pre)), {data=()})}
    end
  end
end

module Make(IO:S.IO) : S.S with module IO = IO = struct
  module LinMonad = LinMonadMake(IO)
  include LinMonad
  include LensMake(IO)(LinMonad)
  include LinMatchMake(IO)(LinMonad)
end

module Direct = Make(struct
  type 'a io = 'a
  let bind a f = f a
  let return a = a
end)

include Direct

module LinArray = LinArray.Make(Direct)
module LinArray1 = LinArray1.Make(Direct)
