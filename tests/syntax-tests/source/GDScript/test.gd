extends Node

signal custom_signal(param)

const PI = 3.14159

var untyped_var = "Hello, World!"
var typed_int: int = 42
var typed_float: float = 3.14
var typed_string: String = "GDScript Test"
var typed_array: Array = [1, 2, 3, 4]
var typed_dict: Dictionary = {"key": "value", "number": 100}

onready var label = $Label

func say_hello() -> void:
	print("Hello from GDScript!")

func add_numbers(a: int, b: int = 10) -> int:
	return a + b

func process_value(value: int) -> String:
	if value < 0:
		return "Negative"
	elif value == 0:
		return "Zero"
	else:
		return "Positive"

func sum_array(arr: Array) -> int:
	var total: int = 0
	for num in arr:
		total += num
	return total

func describe_number(num: int) -> String:
	match num:
		0:
			return "Zero"
		1, 2, 3:
			return "Small number"
		_:
			return "Large number"

func long_description() -> String:
	return """This is a test file for GDScript.
It covers variables, functions, control structures, loops, signals, inner classes,
multiline strings, arrays, and dictionaries."""

class InnerExample:
	var inner_value: int = 99
	func show_value() -> void:
		print("Inner value is:", inner_value)

func test_inner_class() -> void:
	var inner = InnerExample.new()
	inner.show_value()

func trigger_signal() -> void:
	emit_signal("custom_signal", "TestParam")

func _ready() -> void:
	say_hello()
	var result_add = add_numbers(5)
	print("Add result:", result_add)
	print("Process value for -5:", process_value(-5))
	print("Sum of array [10, 20, 30]:", sum_array([10, 20, 30]))
	print("Description for 2:", describe_number(2))
	print("Long description:\n", long_description())
	test_inner_class()
	trigger_signal()
