(* Example: linearly-typed lists *)
open Linocaml
let s = Zero
let t = Succ Zero

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
  run' f ()

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
