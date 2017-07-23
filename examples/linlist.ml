(* Example: linearly-typed lists *)

open Linocaml

(* declaring slots *)
type ('a,'b,'c) ctx = <s:'a; t:'b; u:'c>
[@@deriving lens]
[@@runner]

type 'a linlist_ = Cons of 'a data * 'a linlist | Nil
 and 'a linlist = 'a linlist_ lin

(* iterating over a list in slot "s"  *)
let rec iter f  =
  match%lin get s with
  | Cons(x, #s) -> f x >> iter f
  | Nil -> return ()

(* map on a list in slot "s" *)
let rec map f =
  match%lin get s with
  | Cons(x, #s) ->
    map f >>
    put s [%linval Cons(Data_Internal__ (f x), !!s)]
  | Nil -> putval s Nil

(* equivalent to List.rev_map (though this one is non tail-recursive) *)
let rev_map f =
  let rec loop () =
    match%lin get s with
    | Cons(x, #s) ->
       put t [%linval Cons(Data_Internal__ (f x), !!t)] >>
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
     put s [%linval Cons(Data_Internal__ x, !!s)]
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
     put s2 [%linval Cons(Data_Internal__ (f x), !! s1)]
  | Nil -> putval s2 Nil

(* more liberal one *)
let rec map f s1 s2 s3 s4 =
  match%lin get s1 with
  | Cons(x, #s2) ->
     map f s1 s2 s3 s4 >>
     put s4 [%linval Cons(Data_Internal__ (f x), !! s3)]
  | Nil -> putval s4 Nil

module O = struct
  type ('a,'b) t = <tmp:'a; outer:'b>
  [@@deriving lens][@@runner]
end
let root = {Lens.get=(fun x -> x); Lens.put=(fun _ x -> x)}


(* with less slot parameters, by using "temporary environment" *)
let rec map f s1 s2 =
  let s1' = s1 ##. O.outer
  and s2' = s2 ##. O.outer
  in
  let rec loop () =
    match%lin get O.tmp with
    | Cons(x, #O.tmp) ->
       loop () >>
       put O.tmp [%linval Cons(Data_Internal__ (f x), !! O.tmp)]
    | Nil -> putval O.tmp Nil
  in
  put root [%linval object method tmp=Empty method outer= !!root end] >>
  put O.tmp (get s1') >>
  loop () >>
  put s2' (get O.tmp) >>
  put root (O.linval_t (get O.outer))

let iter f s =
  let s' = s ##. O.outer
  in
  let rec loop () =
    match%lin get O.tmp with
    | Cons(x, #O.tmp) -> f x; loop ()
    | Nil -> return ()
  in
  put root [%linval object method tmp=Empty method outer= !!root end] >>
  put O.tmp (get s') >>
  loop () >>
  put root (O.linval_t (get O.outer))
           
  
(* again play wth them *)
let () =
  let f () =
    make [1;2;3] >>
    map (fun x -> x+1) s s >>
    map string_of_int s s >>
    iter (fun x -> print_endline x; return ()) s
  in
  run_ctx f ()

(* with less slot parameters, by using "temporary environment" *)
let map f s =
  lin_split begin
    let s' = s ##. O.outer
    in
    let rec loop () =
      match%lin get O.tmp with
      | Cons(x, #O.tmp) ->
         loop () >>
         put O.tmp [%linval Cons(Data_Internal__ (f x), !! O.tmp)]
      | Nil -> putval O.tmp Nil
    in
    put root [%linval object method tmp=Empty method outer= !!root end] >>
    put O.tmp (get s') >>
    loop () >>
    put root (O.linval_t [%linval (!! O.outer, !! O.tmp) ])
  end
  
(* again play wth them *)
let () =
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
  put root [%linval object method tmp=Empty method outer= !!root end] >>
  let%lin `A(#O.tmp) = get s in
  put s (get O.tmp) >>
  put root (O.linval_t (get O.outer))
