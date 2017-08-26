type ('a,'b,'pre,'post) slot = ('a,'b,'pre,'post) Lens.t
type empty = Base.empty

type 'a data = 'a Base.data
type 'a lin  = 'a Base.lin

include Base.Make(struct
  type 'a io = 'a
  let (>>=) a f = f a
  let return a = a
end  
)
