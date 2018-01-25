type (+'a,-'b,-'pre,+'post) t = {get:'pre -> 'a; put:'pre -> 'b -> 'post}

let _0 = {get=(fun (a,_) -> a); put=(fun (_,ss) b -> (b,ss))}
let _1 = {get=(fun (_,(a,_)) -> a); put=(fun (s0,(_,ss)) b -> (s0,(b,ss)))}
let _2 = {get=(fun (_,(_,(a,_))) -> a); put=(fun (s0,(s1,(_,ss))) b -> (s0,(s1,(b,ss))))}
let _3 = {get=(fun (_,(_,(_,(a,_)))) -> a); put=(fun (s0,(s1,(s2,(_,ss)))) b -> (s0,(s1,(s2,(b,ss)))))}

let compose
   : 'a 'b 'in1 'in2 'out1 'out2. ('a, 'b, 'in1, 'in2) t -> ('in1, 'in2, 'out1, 'out2) t -> ('a, 'b, 'out1, 'out2) t =
  fun l1 l2 ->
  {get=(fun out1 -> l1.get (l2.get out1)); put=(fun out1 b -> l2.put out1 (l1.put (l2.get out1) b))}
