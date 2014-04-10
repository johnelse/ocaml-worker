module type IO = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val return : 'a -> 'a t
  val run : 'a t -> 'a

  type channel

  val channel_of_int : fd_number:int -> channel
  val write_to_channel : channel:channel -> data:string -> unit t
end
