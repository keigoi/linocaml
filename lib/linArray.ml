
module Make(M:S.S) : S.LIN_ARRAY with module LinMonad = M = struct
  module LinMonad = M
  open Base
  open M
  type 'a larr = 'a array lin

  let alloc : 'a. 'a list -> (unit, 'a larr, unit data) monad =
    fun[@inline] xs -> {__m=(fun[@inline]  _ -> IO.return ({__lin=Array.of_list xs}, {data=()}))}

  let dealloc : 'a. ('a larr, unit, unit data) monad =
    {__m=(fun[@inline] _ -> IO.return ((), {data=()}))}
  let to_list : 'a. ('a larr, 'a larr, 'a list data) monad =
    {__m=(fun[@inline] larr -> IO.return (larr, {data=Array.to_list larr.__lin}))}
  let lookup : 'a.
               int -> ('a larr, 'a larr, 'a data) monad =
    fun[@inline] i -> {__m=(fun[@inline] larr -> IO.return (larr, {data=larr.__lin.(i)}))}

  let update : 'a.
               int -> 'a -> ('a larr, 'a larr, unit data) monad =
    fun[@inline] i v -> {__m=(fun[@inline] larr -> larr.__lin.(i) <- v; IO.return (larr, {data=()}))}

  let map : 'a 'b.
            ('a -> 'b) -> ('a larr, 'b larr, unit data) monad =
    fun[@inline] f -> {__m=(fun[@inline] larr -> IO.return ({__lin=Array.map f larr.__lin}, {data=()}))}

  let mapiM : 'a 'pre 'b 'post.
             (int -> 'a -> ('pre, 'pre, 'b data) monad) -> ('a larr, 'b larr, 'pre, 'post) lens -> ('pre, 'post, unit data) monad =
    fun[@inline] f l ->
     Syntax.Internal._peek (fun[@inline] pre ->
       let larr = lens_get l pre in
       let rec loop i xs =
         if i < Array.length larr.__lin then
           f i larr.__lin.(i) >>= fun x -> loop (i+1) (x::xs)
         else
           return xs
       in
       loop 0 [] >>= fun[@inline] xs ->
       Syntax.Internal._poke (lens_put l pre {__lin=(Array.of_list (List.rev xs))}))

  let mapM : 'a 'pre 'b 'post.
             ('a -> ('pre, 'pre, 'b data) monad) -> ('a larr, 'b larr, 'pre, 'post) lens -> ('pre, 'post, unit data) monad =
    fun[@inline] f l -> mapiM (fun[@inline] _ x -> f x) l

  let iteriM : 'a 'pre.
               (int -> 'a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad =
    fun f l ->
    Syntax.Internal._peek (fun[@inline] pre ->
        let larr = lens_get l pre in
        let rec loop i () =
          if i < Array.length larr.__lin then
            f i larr.__lin.(i) >>= loop (i+1)
          else
            return ()
        in
        loop 0 ())

  let iterM : 'a 'pre.
              ('a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad =
    fun[@inline] f l -> iteriM (fun[@inline] _ x -> f x) l

end[@@inline]
