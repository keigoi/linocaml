(* Example: linearly-typed lists *)

open Linocaml

type 'a linlist_ = Cons of 'a data * 'a linlist | Nil
 and 'a linlist = 'a linlist_ lin

(* decraring lists *)
type ('a,'b,'c) ctx = <s:'a; t:'b; u:'c>
[@@deriving lens]
[@@runner]

(* iterating over the list in slot "s"  *)
let rec iter f  =
  match%lin get s with
  | Cons(x, #s) -> f x >> iter f
  | Nil -> return ()

(* map on a lists in the slot "s" *)
let rec map f =
  match%lin get s with
  | Cons(x, #s) ->
    map f >>
    let%lin #s = [%linval Cons(Data__ (f x), !!s)]
    in return ()
  | Nil -> set s Nil

(* turn a normal list into linear list *)
let rec make = function
  | x::xs ->
     make xs >>
     let%lin #s = [%linval Cons(Data__ x, !!s)]
     in return ()
  | [] ->
     let%lin #s = [%linval Nil] in
     return ()

(* put some values and play wth it *)
let () =
  let f () =
    make [1;2;3] >>
    map (fun x -> x+1) >>
    map string_of_int >>
    iter (fun x -> print_endline x; return ()) >>
    return ()
  in
  run_ctx (f ())

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
     let%lin #s2 = [%linval Cons(Data__ (f x), !! s1)]
     in return ()
  | Nil -> set s2 Nil

(* more liberal one *)
let rec map f s1 s2 s3 s4 =
  match%lin get s1 with
  | Cons(x, #s2) ->
     map f s1 s2 s3 s4 >>
     let%lin #s4 = [%linval Cons(Data__ (f x), !! s3)]
     in return ()
  | Nil -> set s4 Nil
  
(* and same slots can be passed! *)
let f () =
  iter (fun x -> return ()) s s

let g () =
  map string_of_int s s s s

let h () =
  let%lin #t = get s in return ()
;;
