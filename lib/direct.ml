type (+'a,-'b,-'pre,+'post) lens = ('a,'b,'pre,'post) Base.lens = {get:'pre -> 'a; put:'pre -> 'b -> 'post}
type empty = Base.empty

type 'a data = 'a Base.data = Data of 'a
type 'a lin  = 'a Base.lin

include Base.Make(struct
  type 'a io = 'a
  let bind a f = f a
  let return a = a
end)
