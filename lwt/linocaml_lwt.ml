type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Linocaml.Lens.t
type empty = Linocaml.Base.empty

type 'a data = 'a Linocaml.Base.data
type 'a lin  = 'a Linocaml.Base.lin

include Linocaml.Base.Make(struct
  type 'a io = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return
  
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  
  let read_line = Lwt_io.read_line_opt
  
  let read ic count =
    let count = min count Sys.max_string_length in
    Lwt_io.read ~count ic
  
  let write = Lwt_io.write
  
  let flush = Lwt_io.flush
end)
