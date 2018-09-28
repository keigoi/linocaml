type (+'a,-'b,-'pre,+'post) lens = ('a,'b,'pre,'post) Base.lens = {get:'pre -> 'a; put:'pre -> 'b -> 'post}
type empty = Base.empty

type 'a data = 'a Base.data = Data of 'a
type 'a lin  = 'a Base.lin

module M = Base.Make(struct
  type 'a io = 'a
  let bind a f = f a
  let return a = a
end)

include M

module LinArray = LinArray.Make(M)(M)
module LinArray1 = LinArray1.Make(M)(M)
