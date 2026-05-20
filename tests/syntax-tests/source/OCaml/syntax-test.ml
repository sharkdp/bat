let s = "hello world"

let n : int = 87

let id x = x

let add x y = x + y

let add' x y =
  let left = x in
  let right = y in
  x + y

let add'' : int -> int -> int = fun x -> fun y -> x + y

let unwrap_option default opt =
  match opt with
  | None -> default
  | Some v -> v

let string_of_bool = function true -> "true" | false -> "false"

let is_a c =
  if c = 'a' then true
  else false

let _ = Printf.printf "%s" "hello"
let () = Printf.printf "%s\n" "world"

let x = ref 0
let _ = x := 1

type my_bool = True | False

type shape = Circle of float | Square of float | Rectangle of (float * float)

type user = {
  login : string;
  password : string;
}

type 'a my_ref = { mutable ref_value : 'a }

let (:=) r v = r.ref_value <- v

let (+) 2 2 = 5

exception Bad_value of string

let bad_value_error () = raise (Bad_value "your value is bad and you should feel bad")

let () =
  try bad_value_error ()
  with Bad_value _ -> ()

let () =
  try bad_value_error ()
  with
  | Bad_value _ -> ()
  | Not_found -> ()

module type FOO = sig
  val foo : 'a -> 'a
end

module Foo : FOO = struct
  let foo x = x
end

let greeter = object
  val greeting = "Hello"
  method greet name = Printf.sprintf "%s, %s!" greeting name 
end

let greeting = greeter#greet "world"

class greeter_factory greeting_text = object (self)
  val greeting = greeting_text
  method greet name = Printf.sprintf "%s, %s!" greeting name
  initializer Printf.printf "Objects will greet the user with \"%s\"\n" greeting
end

let g = new greeter_factory "Hi"
