(* Example: linearly-typed lists *)
open Linocaml.Base
open Linocaml.Direct

(* declaring slots *)
type ('a,'b,'c) ctx = <s:'a; t:'b; u:'c>
[@@deriving lens]
[@@runner]
(*
Generated from [@@runner]:
val run_ctx :
  (unit ->
   (< s : empty; t : empty; u : empty > lin,
    < s : empty; t : empty; u : empty > lin, 'a) monad) 
  -> unit -> 'a
val linval_ctx :
  ('a, < s : empty; t : empty; u : empty > lin, 'b) monad ->
  ('a, empty, 'b) monad

Generated from [@@deriving lens]:
val s :
  ('a, 'aa, < s : 'a; t : 'b; u : 'c > lin, < s : 'aa; t : 'b; u : 'c > lin) slot
val t :
  ('b, 'bb, < s : 'a; t : 'b; u : 'c > lin, < s : 'a; t : 'bb; u : 'c > lin) slot
val u :
  ('c, 'cc, < s : 'a; t : 'b; u : 'c > lin, < s : 'a; t : 'b; u : 'cc > lin) slot
*)                      

(* linearly typed list *)
type 'a linlist_ = Cons of 'a data * 'a linlist | Nil
 and 'a linlist = 'a linlist_ lin

(* iterating over a list in slot "s"  *)
let rec iter f =
  get s >>= function%lin
  | Cons(x, #s) -> f x >> iter f
  | Nil -> return ()

(* map on a list in slot "s" *)
let rec map f =
  match%lin get s with
  | Cons(x, #s) ->
    map f >>
    put s [%linret Cons(Data (f x), !!s)]
  | Nil -> putval s Nil

(* equivalent to List.rev_map (though this one is non tail-recursive) *)
let rev_map f =
  let rec loop () =
    match%lin get s with
    | Cons(x, #s) ->
       put t [%linret Cons(Data (f x), !!t)] >>
       (* put t (Syntax.Internal.__takeval t >>= Syntax.Internal.__mkbindfun (fun x -> )) *)
       loop ()
    | Nil -> return ()
  in
  (* accumulator *)
  putval t Nil >>
  loop () >>
  (* put it back to s *)
  put s (get t)

(* turn a normal list into linear list *)
let rec make = function
  | x::xs ->
     make xs >>
     put s [%linret Cons(Data x, !!s)]
  | [] ->
     putval s Nil

(* put some values and play wth it *)
let () =
  let f () =
    make [1;2;3] >>
    map (fun x -> x+1) >>
    map string_of_int >>
    iter (fun x -> print_endline x; return ())
  in
  run_ctx f ()

(* 
 * Currently, parameterising over slots would be a little cumbersome 
 *)
let rec iter f s s1  =
  match%lin get s with
  | Cons(x, #s1) -> f x >> iter f s s1
  | Nil -> return ()

(* map on the same types *)
let rec map f s1 s2 =
  match%lin get s1 with
  | Cons(x, #s2) ->
     map f s1 s2 >>
     put s2 [%linret Cons(Data (f x), !! s1)]
  | Nil -> putval s2 Nil

(* more liberal one *)
let rec map f s1 s2 s3 s4 =
  match%lin get s1 with
  | Cons(x, #s2) ->
     map f s1 s2 s3 s4 >>
     put s4 [%linret Cons(Data (f x), !! s3)]
  | Nil -> putval s4 Nil


module O = struct
  type ('a,'b) t = <tmp:'a; outer:'b>
  [@@deriving lens][@@runner]
end

let root = {Linocaml.Lens.get=(fun x -> x); put=(fun _ x -> x)}


(* with less slot parameters, by using "temporary environment" *)
let rec map f s1 s2 =
  let rec loop () =
    match%lin get O.tmp with
    | Cons(x, #O.tmp) ->
       put O.tmp (loop ()) >>
       [%linret Cons(Data (f x), !! O.tmp)]
    | Nil -> return Nil
  in
  let s1' = s1 ##. O.outer
  and s2' = s2 ##. O.outer
  in
  put root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
  put O.tmp (get s1') >>
  put s2' (loop ()) >>
  put root (O.linval_t (get O.outer))

let iter f s =
  let s' = s ##. O.outer
  in
  let rec loop () =
    match%lin get O.tmp with
    | Cons(x, #O.tmp) -> f x; loop ()
    | Nil -> return ()
  in
  put root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
  put O.tmp (get s') >>
  loop () >>
  put root (O.linval_t (get O.outer))
           
  
(* again play wth them *)
let () =
(* let () = *)
  let f () =
    make [1;2;3] >>
    map (fun x -> x+1) s s >>
    map string_of_int s s >>
    iter (fun x -> print_endline x; return ()) s
  in
  run_ctx f ()

(* with less slot parameters, by using "temporary environment" *)
let map f s =
    let s' = s ##. O.outer
    in
    let rec loop () =
      match%lin get O.tmp with
      | Cons(x, #O.tmp) ->
         put O.tmp (loop ()) >>
         [%linret Cons(Data (f x), !! O.tmp)]
      | Nil -> return Nil
    in
    put root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
    put O.tmp (get s') >>
    put O.tmp (loop ()) >>
    lin_split @@ put root (O.linval_t [%linret (!! O.outer, !! O.tmp) ])
  
(* again play wth them *)
let () =
(* let () = *)
  let f () =
    make [100;200;300] >>
    put s (map (fun x -> x*2) s) >>
    put s (map string_of_int s) >>
    iter (fun x -> print_endline x; return ()) s
  in
  run_ctx f ()
   
let f () =
  let s = s ##. O.outer
  in
  put root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
  let%lin `A(#O.tmp) = get s in
  put s (get O.tmp) >>
  put root (O.linval_t (get O.outer))
