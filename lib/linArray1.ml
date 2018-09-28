open Base

module type LIN_ARRAY = sig
  type 'a larr = 'a array lin

  module LinMonad : Base.LIN_MONAD
  open LinMonad
  val alloc :
    'a list -> ('pre, 'pre, 'a larr) monad
  val dealloc :
    ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
  val to_list :
    ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, ('a larr * 'a list data) lin) monad
  val lookup :
    int -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, ('a larr * 'a data) lin) monad
  val update :
    int -> 'a -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'a larr) monad
  val map :
    ('a -> 'b) -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'b larr) monad
  val mapM :
    ('a -> ('pre, 'pre, 'b data) monad) -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'b larr) monad
  val mapiM :
    (int -> 'a -> ('pre, 'pre, 'b data) monad) -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'b larr) monad
  (* val iterM :
   *   ('a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad
   * val iteriM :
   *   (int -> 'a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad *)
end

module Make(M:Base.LIN_MONAD)(M1:Base.LIN_MATCH with module LinMonad=M) : LIN_ARRAY with module LinMonad = M = struct
  module LinMonad = M
  open Base
  open M
  open M1
  type 'a larr = 'a array lin

  let _lin x = Lin_Internal__ x
  let _unlin (Lin_Internal__ x) = x

  let alloc : 'a. 'a list -> ('pre, 'pre, 'a larr) monad =
    fun xs -> {__m=(fun  pre -> IO.return (pre, Lin_Internal__ (Array.of_list xs)))}

  let dealloc : 'a 'pre 'post. ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, unit data) monad =
    fun l -> {__m=(fun pre -> IO.return (l.put pre Empty, Data ()))}

  let to_list : 'a. ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, ('a larr * 'a list data) lin) monad =
    fun l -> {__m=(fun pre ->
                let (Lin_Internal__ arr) as larr = l.get pre in
                IO.return (l.put pre Empty, Lin_Internal__ ((larr, Data (Array.to_list arr)))))}

  let lookup : 'a.
               int -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, ('a larr * 'a data) lin) monad =
    fun i l -> {__m=(fun pre ->
                  let ((Lin_Internal__ arr) as larr) = l.get pre in
                  IO.return (l.put pre Empty, Lin_Internal__ (larr, Data arr.(i))))}

  let update : 'a.
               int -> 'a -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'a larr) monad =
    fun i v l -> {__m=(fun pre ->
                  let ((Lin_Internal__ arr) as larr) = l.get pre in
                  arr.(i) <- v;
                  IO.return (l.put pre Empty, larr))}

  let map : 'a 'b.
            ('a -> 'b) -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'b larr) monad =
    fun f l -> {__m=(fun pre ->
                let (Lin_Internal__ arr) = l.get pre in
                IO.return (l.put pre Empty, Lin_Internal__ (Array.map f arr)))}

  let mapiM : 'a 'pre 'b 'post.
             (int -> 'a -> ('pre, 'pre, 'b data) monad) -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'b larr) monad =
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
       Syntax.Internal._poke (l.put pre Empty) >> return_lin (Array.of_list (List.rev xs)))

  let mapM : 'a 'pre 'b 'post.
             ('a -> ('pre, 'pre, 'b data) monad) -> ('a larr, empty, 'pre, 'post) lens -> ('pre, 'post, 'b larr) monad =
    fun f l -> mapiM (fun _ x -> f x) l

  (* let iteriM : 'a 'pre.
   *              (int -> 'a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad =
   *   fun f l ->
   *   Syntax.Internal._peek (fun pre ->
   *       let Lin_Internal__ arr = l.get pre in
   *       let rec loop i () =
   *         if i < Array.length arr then
   *           f i arr.(i) >>= loop (i+1)
   *         else
   *           return ()
   *       in
   *       loop 0 ())
   *
   * let iterM : 'a 'pre.
   *             ('a -> ('pre, 'pre, unit data) monad) -> ('a larr, 'a larr, 'pre, 'pre) lens -> ('pre , 'pre, unit data) monad =
   *   fun f l -> iteriM (fun _ x -> f x) l *)

end
