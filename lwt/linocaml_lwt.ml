type (+'a,-'b,-'pre,+'post) slot = ('a,'b,'pre,'post) Linocaml.Lens.t = {get:'pre -> 'a; put:'pre -> 'b -> 'post}
type empty = Linocaml.Base.empty

type 'a data = 'a Linocaml.Base.data = Data of 'a
type 'a lin  = 'a Linocaml.Base.lin

include Linocaml.Base.Make(struct
  type 'a io = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return
end)
