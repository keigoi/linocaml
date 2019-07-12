module Base = Base
module S = S
include Base

module LinMonadMake(IO:S.IO)
       : S.LIN_MONAD
       with module IO = IO
  = struct
  module IO = IO
  type ('pre, 'post, 'a) monad = {__m:('pre -> ('post * 'a) IO.io)}
  let[@inline] return a =
    {__m=(fun[@inline] pre -> IO.return (pre, {data=a}))}
  let[@inline] (>>=) m f =
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m pre) (fun[@inline] (mid, {data=a}) -> (f a).__m mid))}
  let[@inline] (>>) m1 m2 =
    {__m=(fun[@inline] pre ->
       IO.bind (m1.__m pre) (fun[@inline] (mid, _) -> m2.__m mid))}
  let[@inline] run f =
    IO.bind ((f ()).__m ()) (fun[@inline] (_, {data=x}) -> IO.return x)
end[@@inline]

module LensMake(IO:S.IO)(M:S.LIN_MONAD with module IO = IO)
       : S.LENS with module LinMonad := M
  = struct
  open M
  type all_empty = [`cons of unit * 'xs] as 'xs
  let (@>) : 'p 'q 'pre 'post 'a.
             ('p, 'q, 'a) monad
             -> ('p, 'q, 'pre, 'post) lens
             -> ('pre, 'post, 'a) monad =
    fun[@inline] m l ->
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m (lens_get l pre)) (fun[@inline] (q, a) -> IO.return (lens_put l pre q, a)))}
  let[@inline] (<@) l m = m @> l
  let _0 = Zero
  let _1 = Succ Zero
  let _2 = Succ (Succ Zero)
  let run' =
    fun[@inline] f x ->
    let rec all_empty = `cons((), all_empty) in
    IO.bind ((f x).__m all_empty) (fun[@inline] (_, {data=x}) -> IO.return x)
  let extend : 'pre. ('pre, [`cons of unit * 'pre], unit data) monad =
    {__m=(fun[@inline] pre ->
       IO.return (`cons((), pre), {data=()})
    )}
  let shrink : type pre. ([`cons of unit * pre], pre, unit data) monad =
    {__m=(fun[@inline] (`cons((),pre)) ->
       IO.return (pre, {data=()})
    )}
end[@@inline]

module LinMatchMake(IO:S.IO)(M:S.LIN_MONAD with module IO = IO)
       : S.LIN_MATCH with module LinMonad := M = struct
  open M
  type 'f bind = 'f

  let (>>-) : 'pre 'mid 'a 'post 'b. ('pre, 'mid, 'a lin) monad
              -> ('a lin -> ('mid, 'post, 'b) monad) bind
              -> ('pre, 'post, 'b) monad =
    fun[@inline] m f ->
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m pre) (fun[@inline] (mid, x) -> (f x).__m mid))}

  let return_lin : 'a 'p. 'a -> ('p,'p,'a lin) monad =
    fun[@inline] v ->
    {__m=(fun[@inline] pre ->
       IO.return (pre, {__lin=v}))}

  let (!%) : 'a 'b 'c. (unit, 'b lin, 'c data) monad
             -> ('pre, 'pre, ('b lin * 'c data) lin) monad
    = fun[@inline] m ->
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m ()) (fun[@inline] (b, c) -> IO.return (pre, {__lin=(b, c)})))}

  let (!%!) : 'a 'b 'c. (unit, 'b lin, 'c data) monad
             -> ('pre, 'pre, 'b lin) monad
    = fun[@inline] m ->
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m ()) (fun (b, _) -> IO.return (pre, b)))}

  let (%>) : 'a 'pre 'post 'b 'c. ('a, 'b lin, 'c data) monad
              -> ('a, unit, 'pre, 'post) lens -> ('pre, 'post, ('b lin * 'c data) lin) monad
    = fun[@inline] m l ->
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m (lens_get l pre)) (fun[@inline] (b, c) ->
           IO.return (lens_put l pre (), {__lin=(b, c)})))}

  let (%>!) : 'a 'pre 'post 'b 'c. ('a, 'b lin, 'c data) monad
              -> ('a, unit, 'pre, 'post) lens -> ('pre, 'post, 'b lin) monad
    = fun m l ->
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m (lens_get l pre)) (fun[@inline] (b, _) ->
           IO.return (lens_put l pre (), b)))}


  let get_lin : 'a 'pre 'post. ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad =
    fun[@inline] l ->
    {__m=(fun[@inline] pre ->
       IO.return (lens_put l pre (), lens_get l pre))}

  let put_lin : 'a 'mid 'post 'pre. (unit,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad =
    fun l m ->
    {__m=(fun[@inline] pre ->
       IO.bind (m.__m pre) (fun[@inline] (mid, v) ->
           IO.return (lens_put l mid v, {data=()})))}

  let put_linval : 'a 'pre 'post. (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad =
    fun[@inline] l v ->
    {__m=(fun[@inline] pre ->
       IO.return (lens_put l pre {__lin=v}, {data=()}))}

  module Syntax = struct
    let bind_data = (>>=)
    let bind_lin = (>>-)
    let bind_raw {__m=m} f = {__m=(fun[@inline] pre -> IO.bind (m pre) (fun[@inline] (mid,x) -> (f x).__m mid))}
    let return_lin = return_lin
    let get_lin = get_lin
    let put_linval = put_linval

    module Internal = struct
      let _lin x = {__lin=x}
      let _unlin ({__lin=x}) = x
      external _mkbind : 'f -> 'f bind = "%identity"
      let _peek : 'pre 'post 'a. ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad =
        fun[@inline] f ->
        {__m=(fun[@inline] pre -> ((f pre).__m pre))}
      let _poke : 'pre 'post. 'post -> ('pre, 'post, unit data) monad =
        fun[@inline] post ->
        {__m=(fun[@inline] _ -> IO.return (post, {data=()}))}
      let _map_lin : 'a 'b 'pre 'post. ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
        = fun[@inline] f l ->
        {__m=fun[@inline] pre -> IO.return (lens_put l pre @@ _lin (f (_unlin @@ lens_get l pre)), {data=()})}
    end
  end
end[@@inline]

module Make(IO:S.IO) : S.S with module IO = IO = struct
  module LinMonad = LinMonadMake(IO)
  include LinMonad
  include LensMake(IO)(LinMonad)
  include LinMatchMake(IO)(LinMonad)
end[@@inline]

module Direct = Make[@inlined](struct
  type 'a io = 'a
  external[@inline] bind : 'a -> ('a -> 'b) -> 'b = "%revapply"
  external[@inline] return : 'a -> 'a = "%identity"
end)

include Direct

module LinArray_Make = LinArray.Make
module LinArray1_Make = LinArray1.Make
module LinArray = LinArray.Make(Direct)
module LinArray1 = LinArray1.Make(Direct)
