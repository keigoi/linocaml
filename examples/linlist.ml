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
  match%lin get_lin s with
  | Cons(x, #s) -> f x; iter f
  | Nil -> return ()

(* map on a list in slot "s" *)
let rec map f =
  match%lin get_lin s with
  | Cons(x, #s) ->
    map f >>
    put_lin s [%linret Cons(Data (f x), !!s)]
  | Nil -> put_linval s Nil

(* equivalent to List.rev_map *)
let rev_map f =
  let rec loop () =
    match%lin get_lin s with
    | Cons(x, #s) ->
       put_lin t [%linret Cons(Data (f x), !!t)] >>
       loop ()
    | Nil -> return ()
  in
  (* accumulator *)
  put_linval t Nil >>
  loop ()

(* turn a normal list into linear list *)
let rec make = function
  | x::xs ->
     make xs >>
     put_lin s [%linret Cons(Data x, !!s)]
  | [] ->
     put_linval s Nil

(* put_lin some values and play wth it *)
let () =
  let f () =
    make [1;2;3] >>
    map (fun x -> x+1) >>
    map string_of_int >>
    iter (fun x -> print_endline x; ())
  in
  run_ctx f ()

(*
 * Currently, parameterising over slots would be a little cumbersome
 *)
let rec iter f s s1 =
  match%lin get_lin s with
  | Cons(x, #s1) -> f x; iter f s s1
  | Nil -> return ()

(* map on the same types *)
let rec map f s1 s2 =
  match%lin get_lin s1 with
  | Cons(x, #s2) ->
     map f s1 s2 >>
     put_lin s2 [%linret Cons(Data (f x), !! s1)]
  | Nil -> put_linval s2 Nil

(* more liberal one *)
let rec map f s1 s2 s3 s4 =
  match%lin get_lin s1 with
  | Cons(x, #s2) ->
     map f s1 s2 s3 s4 >>
     put_lin s4 [%linret Cons(Data (f x), !! s3)]
  | Nil -> put_linval s4 Nil


module O = struct
  type ('a,'b) t = <tmp:'a; outer:'b>
  [@@deriving lens][@@runner]
end

let root = {get=(fun x -> x); put=(fun _ x -> x)}


(* with less slot parameters, by using "temporary environment" *)
let map f s1 s2 =
  let rec loop () =
    match%lin get_lin O.tmp with
    | Cons(x, #O.tmp) ->
       put_lin O.tmp (loop ()) >>
       [%linret Cons(Data (f x), !! O.tmp)]
    | Nil -> return_lin Nil
  in
  let s1' = s1 ##. O.outer
  and s2' = s2 ##. O.outer
  in
  put_lin root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
  put_lin O.tmp (get_lin s1') >>
  put_lin s2' (loop ()) >>
  put_lin root (O.linval_t (get_lin O.outer))

let iter f s =
  let s' = s ##. O.outer
  in
  let rec loop () =
    match%lin get_lin O.tmp with
    | Cons(x, #O.tmp) -> f x; loop ()
    | Nil -> return ()
  in
  put_lin root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
  put_lin O.tmp (get_lin s') >>
  loop () >>
  put_lin root (O.linval_t (get_lin O.outer))


(* again play wth them *)
let () =
(* let () = *)
  let f () =
    make [1;2;3] >>
    map (fun x -> x+1) s s >>
    map string_of_int s s >>
    iter (fun x -> print_endline x) s
  in
  run_ctx f ()

(* (\* with less slot parameters, by using "temporary environment" *\)
 * let map f s =
 *     let s' = s ##. O.outer
 *     in
 *     let rec loop () =
 *       match%lin get_lin O.tmp with
 *       | Cons(x, #O.tmp) ->
 *          put_lin O.tmp (loop ()) >>
 *          [%linret Cons(Data (f x), !! O.tmp)]
 *       | Nil -> return_lin Nil
 *     in
 *     put_lin root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
 *     put_lin O.tmp (get_lin s') >>
 *     put_lin O.tmp (loop ()) >>
 *     lin_split @@ put_lin root (O.linval_t [%linret (!! O.outer, !! O.tmp) ])
 *
 * (\* again play wth them *\)
 * let () =
 * (\* let () = *\)
 *   let f () =
 *     make [100;200;300] >>
 *     put_lin s (map (fun x -> x*2) s) >>
 *     put_lin s (map string_of_int s) >>
 *     iter (fun x -> print_endline x) s
 *   in
 *   run_ctx f () *)

let f () =
  let s = s ##. O.outer
  in
  put_lin root [%linret object method tmp=Linocaml.Base.Empty method outer= !!root end] >>
  let%lin `A(#O.tmp) = get_lin s in
  put_lin s (get_lin O.tmp) >>
  put_lin root (O.linval_t (get_lin O.outer))
