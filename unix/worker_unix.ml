module Io = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x
  let run x = x

  type channel = Unix.file_descr

  let channel_of_int ~fd_number =
    Obj.magic fd_number

  let write_to_channel ~channel ~data =
    let length = String.length data in
    let rec aux pos =
      if pos >= length
      then return ()
      else
        let chars_written = Unix.write channel data pos length in
        aux (pos + chars_written)
    in
    aux 0
end

module Worker = Worker.Make(Io)
