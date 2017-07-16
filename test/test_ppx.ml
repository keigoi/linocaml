let (!%) = Printf.sprintf

open Linocaml

type 't plist = ([`Cons of 't data * 'a lin | `Nil] as 'a) lin
               
(* declare a single slot 's' *)
type ('a,'b) ctx = <s : 'a; t : 'b>
[@@deriving lens]
[@@runner]
            
module LinList = struct
  let make ~bindto ls =
    Linocaml.Syntax.set bindto ls

  let rec iter s1 s2 f =
    match%lin get s1 with
    | `Cons(hd,#s2) -> f hd; iter s1 s2 f
    | `Nil -> return ()
end

let t = s
            
let f () =
  let open LinList in
  let ls = `Cons(Data__ 1,Lin__(`Cons(Data__ 2,Lin__(`Cons(Data__ 3,Lin__ `Nil))))) in
  let%slot #s = make ls in
  iter s s (fun x -> print_endline (!% "%d;" x); return ()) >>
  return ()

let () =
  run_ctx (f ())
