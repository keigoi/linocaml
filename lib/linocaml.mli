type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t
type empty = Linocaml_base.empty = Empty

type 'a data = 'a Linocaml_base.data = Data_Internal__ of 'a
type 'a lin  = 'a Linocaml_base.lin  = Lin_Internal__ of 'a

module Std : S.IO_LIN with type 'a io = 'a
module Lwt : S.IO_LIN with type 'a io = 'a Lwt.t

module Make(M:S.IO) : S.IO_LIN with type 'a io = 'a M.t
