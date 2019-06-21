(* Example: linearly-typed arrays *)
open Linocaml
open Linocaml.LinArray


let example () =
  alloc [100; 200; 300] >>
  map string_of_int >>
  update 1 "Hello" >>
  lookup 1 >>= fun x ->
  dealloc >>
  return x

let () =
  print_endline (run example)

let example2 () =
  alloc [23; 34; 45] @> _0 >>
  alloc [100; 200; 300] @> _1 >>
  iteriM (fun i x ->
      lookup i @> _1 >>= fun y ->
      update i (x + y) @> _1) _0 >>
  to_list @> _1 >>= fun xs ->
  dealloc @> _0 >>
  dealloc @> _1 >>
  return xs

let () =
  List.iter (fun x -> print_endline (string_of_int x)) (run' example2 ())

let example3 () =
  alloc [100; 200; 300] @> _0 >>
  alloc ["abc"; "def"; "ghi"] @> _1 >>
  mapiM (fun i x ->
      lookup i @> _1 >>= fun s ->
      return (s ^ string_of_int x)) _0 >>
  to_list @> _0 >>= fun xs ->
  dealloc @> _1 >>
  dealloc @> _0 >>
  return xs

let () =
  List.iter print_endline (run' example3 ())

open LinArray1

let example4 () =
  let s = _0 in
  let%lin #s  = alloc [100; 200; 300] in
  let%lin #s = map string_of_int s in
  let%lin #s = update 1 "Hello" s in
  let%lin #s, x = lookup 1 s in
  dealloc s >>
  (print_endline x;
  return ())

let () =
  run' example4 ()

let example5 () =
  let s = _0 and t = _1 in
  alloc [100; 200; 300]       >>- fun%lin #s ->
  alloc ["abc"; "def"; "ghi"] >>- fun%lin #t ->
  mapiM (fun i x ->
      lookup i t  >>- fun%lin (#t, str) ->
      return (str ^ string_of_int x)) s
                              >>- fun%lin #s ->
  to_list s                   >>- fun%lin (#s, xs) ->
  dealloc t >>
  dealloc s >>
  return xs

let example5' () =
  let s = _0 and t = _1 in
  let%lin #s = alloc [100; 200; 300] in
  let%lin #t = alloc ["abc"; "def"; "ghi"] in
  let%lin #s =
    mapiM (fun i x ->
        let%lin #t, str = lookup i t in
        return (str ^ string_of_int x)) s in
  let%lin #s, xs = to_list s in
  dealloc t >>
  dealloc s >>
  return xs

let () =
  List.iter print_endline (run' example5' ())

let example6 () =
  extend >>
  extend >>
  example5 () >>= fun x ->
  shrink >>
  shrink >>
  return x

let () =
  List.iter print_endline (run example6)
