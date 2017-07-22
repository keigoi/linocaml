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

(* map on a list in the slot "s" *)
let rec map f =
  match%lin get s with
  | Cons(x, #s) ->
    map f >>
    let%lin #s = [%linval Cons(Data_Internal__ (f x), !!s)]
    in return ()
  | Nil -> set s Nil

(* equivalent to List.rev_map (though this one is non tail-recursive) *)
let rev_map f =
  let rec loop () =
    match%lin get s with
    | Cons(x, #s) ->
       let%lin #t = [%linval Cons(Data_Internal__ (f x), !!t)] in
       loop ()
    | Nil -> return ()
  in
  (* accumulator *)
  set t Nil >>
  loop () >>
  (* put it back to s *)
  let%lin #s = get t in
  return ()

(* turn a normal list into linear list *)
let rec make = function
  | x::xs ->
     make xs >>
     let%lin #s = [%linval Cons(Data_Internal__ x, !!s)]
     in return ()
  | [] ->
     set s Nil

(* put some values and play wth it *)
let () =
  let f () =
    make [1;2;3] >>
    map (fun x -> x+1) >>
    map string_of_int >>
    iter (fun x -> print_endline x; return ()) >>
    return ()
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
     let%lin #s2 = [%linval Cons(Data_Internal__ (f x), !! s1)]
     in return ()
  | Nil -> set s2 Nil

(* more liberal one *)
let rec map f s1 s2 s3 s4 =
  match%lin get s1 with
  | Cons(x, #s2) ->
     map f s1 s2 s3 s4 >>
     let%lin #s4 = [%linval Cons(Data_Internal__ (f x), !! s3)]
     in return ()
  | Nil -> set s4 Nil

module O = struct
  type ('a,'b) t = <tmp:'a; outer:'b>
  [@@deriving lens][@@runner]
end
let root = {Lens.get=(fun x -> x); Lens.set=(fun _ x -> x)}


(* with less slot parameters, by using "temporary environment" *)
let rec map f s1 s2 =
  let%lin #root = [%linval object method tmp=Empty method outer= !!root end]
  in
  let s1' = s1 ##. O.outer
  and s2' = s2 ##. O.outer
  in
  let%lin #O.tmp = get s1'
  in
  let rec loop () =
    match%lin get O.tmp with
    | Cons(x, #O.tmp) ->
       loop () >>
       let%lin #O.tmp = [%linval Cons(Data_Internal__ (f x), !! O.tmp)]
       in return ()
    | Nil -> set O.tmp Nil
  in
  loop () >>
  let%lin #s2' = get O.tmp
  in
  let%lin #root = O.linval_t (get O.outer) in
  return ()
  


(* and same slots can be used for each argument! *)
let f () =
  iter (fun x -> return ()) s s

let g () =
  map string_of_int s s

let h () =
  let%lin #t = get s in return ()
;;

(* BAD *)
(* let f () = *)
(*   let%lin #s = [%linval Data_Internal__ !!s ] *)
(*   in *)
(*   match%lin get s with *)
(*   | _ -> return () (\* here the content of s is lost without actually using it*\) *)
   
let f () =
  let%lin #s = [%linval object method tmp=Internal.__empty method outer= !!s end] in
  let%lin #s = [%linval object method tmp=Internal.__empty method outer= !!s end] in
  return ()

(* let s1 = linlens @@@ s *)
(*generated part*)
(* let s2 = s @@@ linlens @@@ O.outer @@@ linlens *)
(* let t2 = t @@@ linlens @@@ O.outer @@@ linlens *)
(*generated part end*)
type ('a,'b) u = {f:'a;g:'b}
let f () =
  let%lin #root = [%linval object method tmp=Internal.__empty method outer= !!root end] in
  let s = s ##. O.outer
  in
  (*generated part end*)
  let%lin `A(#O.tmp) = get s in
  let%lin #s = get O.tmp in
  let%lin #root = O.linval_t (get O.outer)
  in
  return ()

       

(* strange type ??*)
(* let f () = *)
(*   let%lin #s = [%linval {O.tmp=Internal.__empty; outer= !!root}] in *)
(*   return () *)

(* let f () = *)
(*   Internal.__get root >>= fun x -> return {O.tmp=Internal.__empty; outer= x} >>= *)
(*     fun y -> set s y >> return () *)

(* let f () = *)
(*   let%lin #s = [%linval {O.tmp=Internal.__empty; outer=1}] in *)
(*   return () *)

(* let g () = *)
(*   let%lin #s = [%linval 1] *)
(*   in *)
(*   match%lin get s with *)
(*   | 1 -> return ()  *)
   
(* let f () = *)
(*   (\* let%lin open M in *\) *)
(*   (\* let s = {Lens.get=(fun (Lin_Internal__ o) -> s.Lens.get (O.outer.Lens.get o)); *\) *)
(*   (\*          set=(fun (Lin_Internal__ o) -> Obj.magic ())} (\\* s.set o.outer (o *\\) *\) *)
(*   (\* in *\) *)
(*   let%lin #root =  [%linval {O.tmp=Internal.__empty; outer= !!linlens}] in *)
(*   return () *)
(*     >> begin *)
(*       match%lin get s with *)
(*       | _ -> return () *)
(*     end >> *)
(*     set O.tmp (Data_Internal__ true) >> *)
(*     (\* begin match%lin get O.tmp with *\) *)
(*     (\* | x -> return () *\) *)
(*     (\* end >> *\) *)
(*     match%lin get root with *)
(*     | {O.tmp=#s;outer=#t} -> return () *)
  
         
  
(* val run_val : (<a:empty;outer:empty>,<a:empty,outer:empty>,'a lin) lin_match ->
 *)
