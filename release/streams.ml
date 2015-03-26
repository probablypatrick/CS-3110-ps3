type 'a stream = Stream of 'a * (unit -> 'a stream)

(* NOTE: You may add the rec keyword as you wish for this file. *)

(* returns first n elements of stream s in a list*)
let rec take n (Stream (h,t)) = 
  if n <= 0 then [] else h::(take (n-1) (t ()))

(*Produces a stream whose elements are all equal to x*)
let rec repeat x =
  Stream(x, fun () -> repeat x)

(*Implement a function that lazily applies input to each element of stream s *)
let rec map f s = match s with
  | Stream (h,t) -> Stream(f h, fun () -> map f (t ()))

(*'a stream stream -> 'a stream;; takes a stream of streams, returns only diag elements*)
let rec diag s = match s with
  | Stream (Stream (ih,_), ot) -> Stream (ih, (fun () ->
    match (ot ()) with
    | Stream (Stream (_,it),_) -> diag (Stream (it (), fun () -> 
      match ()))))

let rec diag s = 
  let rec f is n = match is with
  | Stream (h,t) -> if n = 0 then h else f (t ()) (n-1) in 
  match s with
  | Stream (h,t) -> Stream ((f h , fun () -> diag )

(* return stream of suffix streams *)  
let rec suffixes s = match s with
  | Stream (h, t) -> Stream (s, fun () -> suffixes (t ()))

(* takes two streams and returns stream of values alternating between *)
let rec interleave s s' = match s with
  | Stream (h,t) -> Stream (h, fun () -> interleave s' (t ()))
  
let fibs         () = 
  let rec f a b =
    Stream (a, fun () -> f b (a + b)) 
  in f 0 1

let pi           () = 
  let rec f a b = 
    Stream(b, fun () -> f (a+.1.) (b+.(4.*.((-1.)**a)/.(2.*.a+.1.))))
  in f 1. 4.

let look_and_say () = 
  let rec makelist a b=
    match a with
      | []      -> []
      | [v]     -> [b;v]
      | h::x::t -> if h = x then makelist (x::t) (b+1) else 
        (makelist [h] b)@(makelist (x::t) 1)
  in
  let rec makestream a =
    Stream(a, fun () -> makestream (makelist a 1))
  in makestream [1]
