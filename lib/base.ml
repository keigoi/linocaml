type 'a lin = {__lin:'a}
type 'a data = {data:'a}
type (_,_,_,_) lens =
  | Zero : ('a,'b,[`cons of 'a * 'xs], [`cons of 'b * 'xs]) lens
  | Succ :
      ('x,'y,'xs,'ys) lens
      -> ('x,'y,[`cons of 'a * 'xs], [`cons of 'a * 'ys]) lens

let rec lens_get : type x y xs ys. (x,y,xs,ys) lens -> xs -> x = fun l xs ->
  match l,xs with
  | Zero,(`cons(hd,_)) -> hd
  | Succ l,(`cons(_,tl)) -> lens_get l tl

let rec lens_put : type x y xs ys. (x,y,xs,ys) lens -> xs -> y -> ys = fun l xs b ->
  match l,xs with
  | Zero,(`cons(_,tl)) -> `cons(b,tl)
  | Succ l,(`cons(hd,tl)) -> `cons(hd,lens_put l tl b)
