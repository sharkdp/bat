val x = 0

val hello = "hello world"

val id = fn x => x

fun id' x = x

val () = print "hello world\n"

val _ = let
  val hello = "hello"
  val world = "world"
in
  print (hello ^ " " ^ world ^ "\n")
end

fun isZero n =
  if n = 0 then true
  else false

fun isTrue b =
  case b of
    true => true
  | false => false

exception Bad_value of string

fun isTrue' b =
  case b of
    true => true
  | _    => raise (Bad_value "value is not true!")

val alwaysTrue =
  isTrue' false handle Bad_value _ => true

datatype myBool = True | False

datatype shape = Square of real | Circle of real | Point

signature FOO = sig
  val foo : 'a -> 'a
end

structure Foo :> FOO = struct
  fun foo x = x
end
