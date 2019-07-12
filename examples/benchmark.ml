
open Linocaml
open Linocaml.LinArray

let linocaml_array () =
  Linocaml.run begin fun () ->
      alloc [100; 200; 300] >>
      map string_of_int >>
      lookup 1 >>= fun x ->
      update 1 ("Hello, " ^ x) >>
      dealloc >>
      return ()
    end

let linocaml_array_lwt () =
  let open Linocaml_lwt in
  let module Arr = Linocaml.LinArray_Make[@inlined](Linocaml_lwt) in
  let open Arr in
  Linocaml_lwt.run begin fun () ->
      alloc [100; 200; 300] >>
      map string_of_int >>
      lookup 1 >>= fun x ->
      update 1 ("Hello, " ^ x) >>
      dealloc >>
      return ()
    end

let direct_array () =
  let arr = Array.of_list [100; 200; 300] in
  let arr = Array.map string_of_int arr in
  let x = arr.(1) in
  arr.(1) <- "Hello, " ^ x


open Core_bench.Bench

let linocaml_array =
  Test.create ~name:"linocaml_array" linocaml_array
let linocaml_array_lwt =
  Test.create ~name:"linocaml_array_lwt" linocaml_array_lwt
let direct_array =
  Test.create ~name:"direct_array" direct_array

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      [linocaml_array; direct_array]
      (* [linocaml_array; linocaml_array_lwt; direct_array] *)
