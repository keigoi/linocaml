type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Linocaml.Lens.t
type empty = Linocaml.Base.empty

type 'a data = 'a Linocaml.Base.data
type 'a lin  = 'a Linocaml.Base.lin

include Linocaml.Base.Make(struct
  type 'a io = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return
end)
