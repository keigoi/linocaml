open Linocaml

module Channel : sig
  type 'a t
  val create     : unit -> 'a t
  val send       : 'a t -> 'a -> unit
  val receive    : 'a t -> 'a
end = struct
  type 'a t      = 'a Event.channel
  let create     = Event.new_channel
  let send ch x  = Event.sync (Event.send ch x)
  let receive ch = Event.sync (Event.receive ch)
end

module Session : sig
  type ('v, 's) send
  type ('v, 's) recv
  type close
  type 's prot
  val s2c : ('s * 'c) prot -> (('v, 's) send * ('v, 'c) recv) prot
  val c2s : ('s * 'c) prot -> (('v, 's) recv * ('v, 'c) send) prot
  val finish : (close * close) prot

  val connect : ('s * 'c) Channel.t -> ('s * 'c) prot -> ('pre, 'pre, 'c lin) monad
  val accept : ('s * 'c) Channel.t -> ('pre, 'pre, 's lin) monad

  val send : (('v, 's) send lin, unit, 'pre, 'post) lens ->
             'v ->
             ('pre, 'post, 's lin) monad
  val receive : (('v, 's) recv lin, unit, 'pre, 'post) lens ->
                ('pre, 'post, ('s lin * 'v data) lin) monad
  val close : (close lin, unit, 'pre, 'post) lens ->
              ('pre, 'post, unit data) monad
end = struct
  let unlin x = x.__lin
  type ('v, 's) send = 'v Channel.t * 's lin
  type ('v, 's) recv = 'v Channel.t * 's lin
  type close = unit
  type 't prot = 't
  let s2c : ('s * 'c) prot -> (('v, 's) send * ('v, 'c) recv) prot =
    fun (s,c) ->
    let newchan = Channel.create () in
    ((newchan,{__lin=s}),(newchan,{__lin=c}))
  let c2s = s2c
  let finish : (close * close) prot = (), ()

  let connect : ('s * 'c) Channel.t -> ('s * 'c) -> ('pre, 'pre, 'c lin) monad =
    fun ch (s,c) ->
    {__m=(fun pre ->
       Channel.send ch (s,c);
       (pre, {__lin=c})
    )}
  let accept : ('s * 'c) Channel.t -> ('pre, 'pre, 's lin) monad =
    fun ch ->
    {__m=(fun pre ->
       let s,_ = Channel.receive ch in
       (pre, {__lin=s})
    )}


  let send : (('v, 's) send lin, unit, 'pre, 'post) lens ->
             'v ->
             ('pre, 'post, 's lin) monad =
    fun l v ->
    {__m=(fun pre ->
       let ch, cont = unlin @@ lens_get l pre in
       Channel.send ch v;
       (lens_put l pre (), cont)
    )}
  let receive : (('v, 's) recv lin, unit, 'pre, 'post) lens ->
                ('pre, 'post, ('s lin * 'v data) lin) monad =
    fun l ->
    {__m=(fun pre ->
       let ch, cont = unlin @@ lens_get l pre in
       let v = Channel.receive ch in
       (lens_put l pre (), {__lin=(cont, {data=v})})
    )}
  let close : (close lin, unit, 'pre, 'post) lens ->
              ('pre, 'post, unit data) monad =
    fun l ->
    {__m=(fun pre ->
       (lens_put l pre (), {data=()})
    )}
end

open Session

(**
 * A solution for the Santa Claus problem
 *)

type 'a sesslist_ = Cons of 'a lin * 'a sesslist | Nil
 and 'a sesslist = 'a sesslist_ lin

type kind = Elf | Reindeer

let santa ch () =
  let s = _0 in
  let e = _1 in
  let r = _2 in
  let rec iter :
            type a pre mid0 mid.
                 int ->
                 (a sesslist_ lin, unit, pre, [ `cons of unit * mid0 ]) lens ->
                 (unit, a sesslist_ lin, [ `cons of a lin * mid0 ], mid) lens ->
                 (unit, a sesslist_ lin, [ `cons of unit * mid0 ], pre) lens ->
                 (unit -> (mid, pre, unit data) monad) -> (pre, pre, unit data) monad =
    fun i l1 l2 l3 f ->
    if i=0 then
      return ()
    else
      match%lin get_lin l1 with
      | Cons(#s, #l2) ->
         f () >>
         iter (i-1) l1 l2 l3 f
      | Nil ->
         put_linval l3 Nil
  in
  let rec loop (ecount,rcount) () =
    let%lin #s = accept ch in
    let%lin #s, kind = receive s in
    (match kind with
    | Elf ->
       put_lin e [%linret Cons(!!s, !!e)] >>
       return (ecount+1, rcount)
    | Reindeer ->
       put_lin r [%linret Cons(!!s, !!r)] >>
       return (ecount, rcount+1)) >>= fun (ecount, rcount) ->
    if ecount=3 then
      iter 3 e e e (fun () ->
          let%lin #s = send s "Make a new toy!" in
          close s
        )  >>
      loop (ecount-3,rcount) ()
    else if rcount=9 then
      iter 9 r r r (fun () ->
          let%lin #s = send s "Let's go out and deliver!" in
          close s) >>
      loop (ecount,0) ()
    else
      loop (ecount,rcount) ()
  in
  put_linval e Nil >>
  put_linval r Nil >>
  loop (0,0) ()

let rec elf i ch () =
  Printf.printf "Elf %d start\n" i;
  Unix.sleepf (Random.float 5.);
  Printf.printf "Elf %d woke up" i;
  print_newline();
  let s = _0 in
  let prot = c2s (s2c finish) in
  let%lin #s = connect ch prot in
  let%lin #s = send s Elf in
  let%lin #s, str = receive s in
  Printf.printf "Elf %d heard from Santa: %s" i str;
  print_newline();
  close s >>
  elf i ch ()

let rec reindeer i ch () =
  Printf.printf "Reindeer %d start\n" i;
  Unix.sleepf (Random.float 5.);
  Printf.printf "Reindeer %d woke up" i;
  print_newline ();
  let s = _0 in
  let prot = c2s (s2c finish) in
  let%lin #s = connect ch prot in
  let%lin #s = send s Reindeer in
  let%lin #s, str = receive s in
  Printf.printf "Reindeer %d heard from Santa: %s\n" i str;
  print_newline ();
  close s >>
  reindeer i ch ()

let () =
  let ch = Channel.create () in
  for i = 0 to 10 do
    ignore @@
      Thread.create (fun () ->
          run' (elf i ch)) ();
  done;
  for i = 0 to 10 do
    ignore @@
      Thread.create (fun () ->
          run' (reindeer i ch)) ();
  done;
  run' (santa ch) ()
