(** The [Streams] module is used to manipulate infinite sequences. *)

(** The ['a stream] type is used to represent infinite sequences of values with
    type ['a] *)
type 'a stream = Stream of 'a * (unit -> 'a stream)

(** [take n s] returns the first [n] elements of the stream [s] in a
    list. If n < 0, return an empty list. *)
val take : int -> 'a stream -> 'a list

(** [repeat x] returns a stream that has every element equal to [x]. *)
val repeat : 'a -> 'a stream

(** [map f s] creates a new stream by applying [f] to each element of
    [s]. Returns in O(1) time. *)
val map : ('a -> 'b) -> 'a stream -> 'b stream

(** [diag ss] takes a stream of streams and returns a stream of the
    diagonal elements. That is, the [n]th element of the output stream is
    the [n]th element of the [n]th stream in the input. *)
val diag : 'a stream stream -> 'a stream

(** [suffixes s] returns a stream whose [n]th element is the substream
    of [s] starting at the [n]th element of [s]. *)
val suffixes : 'a stream -> 'a stream stream

(** [interleave s t] returns a stream with the elements of [s] and [t]
    in alternating order. *)
val interleave : 'a stream -> 'a stream -> 'a stream

(** [fibs] is a stream whose [n]th element is the [n]th fibonacci
    number. *)
val fibs : unit -> int stream

(** [pi] is a stream that contains a sequence of approximations that
    converge to the real number pi. The elements of this stream are
    the partial sums of {{:
    http://en.wikipedia.org/wiki/Approximations_of_π#Gregory.E2.80.93Leibniz_series}
    this series}. *)
val pi : unit -> float stream

(** [look_and_say] is a stream whose [n]th element is the [n]th number
    in the {{: http://en.wikipedia.org/wiki/Look-and-say_sequence}
    look-and-say sequence}. *)
val look_and_say : unit -> int list stream

