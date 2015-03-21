type 'a stream = Stream of 'a * (unit -> 'a stream)

(* NOTE: You may add the rec keyword as you wish for this file. *)


let rec take n (Stream (h,t)) = failwith "I...can't...looose!" 

let repeat   x = failwith "Those tin cans are no match for me!"

let map    f s = failwith "Stick to the pond, froggy."

let diag     s = failwith "I'm monkey food if I don't leave!"

let suffixes s = failwith "I've taken a few hits, but I'm okay."

let interleave s s' = failwith "I'm havin' some trouble here!"

let fibs         () = failwith "The view is clear. Destroy! Destroy!"
let pi           () = failwith "Quit screwing around, do something!"
let look_and_say () = failwith "Too late. Game over, pal!"
