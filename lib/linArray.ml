open Base

module type LIN_ARRAY = sig
  type 'a larr = 'a array lin

  module LinMonad : Base.LIN_MONAD
  open LinMonad
  val alloc :
    'a list -> (empty, 'a larr, unit data) monad
  val dealloc :
    ('a larr, empty, unit data) monad
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

module Make(M:Base.LIN_MONAD)(M1:Base.LIN_MATCH with module LinMonad=M) : LIN_ARRAY with module LinMonad = M = struct
  module LinMonad = M
  open Base
  open M
  open M1
  type 'a larr = 'a array lin
  let alloc : 'a. 'a list -> (empty, 'a larr, unit data) monad =
    fun xs -> {__m=(fun  _ -> IO.return (Lin_Internal__ (Array.of_list xs), Data ()))}

  let dealloc : 'a. ('a larr, empty, unit data) monad =
    {__m=(fun _ -> IO.return (Empty, Data ()))}
  let to_list : 'a. ('a larr, 'a larr, 'a list data) monad =
    {__m=(fun ((Lin_Internal__ arr) as larr) -> IO.return (larr, Data (Array.to_list arr)))}
  let lookup : 'a.
               int -> ('a larr, 'a larr, 'a data) monad =
    fun i -> {__m=(fun ((Lin_Internal__ arr) as larr) -> IO.return (larr, Data arr.(i)))}

  let update : 'a.
               int -> 'a -> ('a larr, 'a larr, unit data) monad =
    fun i v -> {__m=(fun ((Lin_Internal__ arr) as larr) -> arr.(i) <- v; IO.return (larr, Data ()))}

  let map : 'a 'b.
            ('a -> 'b) -> ('a larr, 'b larr, unit data) monad =
    fun f -> {__m=(fun (Lin_Internal__ arr) -> IO.return (Lin_Internal__ (Array.map f arr), Data ()))}

  let mapiM : 'a 'pre 'b 'post.
             (int -> 'a -> ('pre, 'pre, 'b data) monad) -> ('a larr, 'b larr, 'pre, 'post) lens -> ('pre, 'post, unit data) monad =
    fun f l ->
     Syntax.Internal._peek (fun pre ->
       let Lin_Internal__ arr = l.get pre in
       let rec loop i xs =
         if i < Array.length arr then
           f i arr.(i) >>= fun x -> loop (i+1) (x::xs)
         else
           return xs
       in
       loop 0 [] >>= fun xs ->
       Syntax.Internal._poke (l.put pre (Lin_Internal__ (Array.of_list (List.rev xs))))
       )

  let mapM : 'a 'pre 'b 'post.
             ('a -> ('pre, 'pre, 'b data) monad) -> ('a larr, 'b larr, 'pre, 'post) lens -> ('pre, 'post, unit data) monad =
    fun f l -> mapiM (fun _ x -> f x) l

  let iteriM : 'a 'pre.
               (int -> 'a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad =
    fun f l ->
    Syntax.Internal._peek (fun pre ->
        let Lin_Internal__ arr = l.get pre in
        let rec loop i () =
          if i < Array.length arr then
            f i arr.(i) >>= loop (i+1)
          else
            return ()
        in
        loop 0 ())

  let iterM : 'a 'pre.
              ('a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad =
    fun f l -> iteriM (fun _ x -> f x) l

end
