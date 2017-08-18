let (!%) = Printf.sprintf

open Linocaml.Direct

type 't plist = ([`Cons of 't data * 'a lin | `Nil] as 'a) lin
               
(* declare slots s and t *)
type ('a,'b) ctx = <s : 'a; t : 'b>
[@@deriving lens]
[@@runner]

module LinList = struct
  (* turn a normal list into linear list *)
  let rec make = function
    | x::xs ->
       make xs >>
       let%lin #s = [%linret `Cons(Data x, !!s)]
       in return ()
    | [] ->
       putval s `Nil

  let rec iter s1 s2 f =
    match%lin get s1 with
    | `Cons(hd,#s2) -> f hd; iter s1 s2 f
    | `Nil -> return ()
end

            
let f () =
  let open LinList in
  make [1;2;3] >>
  iter s s (fun x -> print_endline (!% "%d;" x); return ()) >>
  return ()

let () =
  run_ctx f ()
