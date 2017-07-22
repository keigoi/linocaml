type (+'a,-'b,-'pre,+'post) t = {get:'pre -> 'a; set:'pre -> 'b -> 'post}

let compose
   : 'a 'b 'in1 'in2 'out1 'out2. ('a, 'b, 'in1, 'in2) t -> ('in1, 'in2, 'out1, 'out2) t -> ('a, 'b, 'out1, 'out2) t =
  fun l1 l2 ->
  {get=(fun out1 -> l1.get (l2.get out1)); set=(fun out1 b -> l2.set out1 (l1.set (l2.get out1) b))}
