import json

const
  message = "hello world"
  multiLine = """
    foo
    bar
  """
  numbers = @[1, 2, 3]

type Options = enum
  A,
  B,
  C

## Top-level comment
type
  SomeStruct* = ref object
    value*: string

proc someFunc*(): string =
  ## Function docs
  ##
  ## More docs
  result = message

proc someOtherFunc(startingValue: int): (string, int) =
  var num = startingValue
  num += 1
  if num > 10 * 10 * 10:
    echo "Encountered an error"
    raise newException(ValueError, "Value was over 1000")
  ("Fizz", num)

proc `+=`(a: var SomeStruct, b: SomeStruct): string =
  a.value.add(b.value)
  return a.value

echo someFunc()
echo(someOtherFunc(123))
discard someFunc()
