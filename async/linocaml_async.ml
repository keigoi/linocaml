include Linocaml.Base

include Linocaml.Make(struct
  type 'a io = 'a Async.Deferred.t
  let bind m f = Async.Deferred.bind m ~f
  let return = Async.Deferred.return
end)
