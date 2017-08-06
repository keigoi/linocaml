include S.IO
 with type 'a t = 'a Lwt.t
 and type ic = Lwt_io.input_channel
 and type oc = Lwt_io.output_channel
