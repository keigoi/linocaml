include Linocaml.Base

include Linocaml.Make (struct
  type 'a io = 'a Lwt.t
  let bind = Lwt.bind
  let return = Lwt.return
end)
