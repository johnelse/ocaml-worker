module Io = struct
  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let return = Lwt.return
  let run = Lwt_main.run

  type channel = Lwt_unix.file_descr

  let channel_of_int ~fd_number =
    let unix_fd = Obj.magic fd_number in
    Lwt_unix.of_unix_file_descr unix_fd

  let write_to_channel ~channel ~data =
    let length = String.length data in
    let rec aux pos =
      if pos >= length
      then return ()
      else
        Lwt_unix.write channel data pos length
        >>= (fun chars_written -> aux (pos + chars_written))
    in
    aux 0
end

module Worker = Worker.Make(Io)
