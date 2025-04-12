[38;2;248;248;242mextends Node[0m

[38;2;248;248;242msignal custom_signal(param)[0m

[38;2;248;248;242mconst PI = 3.14159[0m

[38;2;248;248;242mvar untyped_var = "Hello, World!"[0m
[38;2;248;248;242mvar typed_int: int = 42[0m
[38;2;248;248;242mvar typed_float: float = 3.14[0m
[38;2;248;248;242mvar typed_string: String = "GDScript Test"[0m
[38;2;248;248;242mvar typed_array: Array = [1, 2, 3, 4][0m
[38;2;248;248;242mvar typed_dict: Dictionary = {"key": "value", "number": 100}[0m

[38;2;248;248;242monready var label = $Label[0m

[38;2;248;248;242mfunc say_hello() -> void:[0m
[38;2;248;248;242m	print("Hello from GDScript!")[0m

[38;2;248;248;242mfunc add_numbers(a: int, b: int = 10) -> int:[0m
[38;2;248;248;242m	return a + b[0m

[38;2;248;248;242mfunc process_value(value: int) -> String:[0m
[38;2;248;248;242m	if value < 0:[0m
[38;2;248;248;242m		return "Negative"[0m
[38;2;248;248;242m	elif value == 0:[0m
[38;2;248;248;242m		return "Zero"[0m
[38;2;248;248;242m	else:[0m
[38;2;248;248;242m		return "Positive"[0m

[38;2;248;248;242mfunc sum_array(arr: Array) -> int:[0m
[38;2;248;248;242m	var total: int = 0[0m
[38;2;248;248;242m	for num in arr:[0m
[38;2;248;248;242m		total += num[0m
[38;2;248;248;242m	return total[0m

[38;2;248;248;242mfunc describe_number(num: int) -> String:[0m
[38;2;248;248;242m	match num:[0m
[38;2;248;248;242m		0:[0m
[38;2;248;248;242m			return "Zero"[0m
[38;2;248;248;242m		1, 2, 3:[0m
[38;2;248;248;242m			return "Small number"[0m
[38;2;248;248;242m		_:[0m
[38;2;248;248;242m			return "Large number"[0m

[38;2;248;248;242mfunc long_description() -> String:[0m
[38;2;248;248;242m	return """This is a test file for GDScript.[0m
[38;2;248;248;242mIt covers variables, functions, control structures, loops, signals, inner classes,[0m
[38;2;248;248;242mmultiline strings, arrays, and dictionaries."""[0m

[38;2;248;248;242mclass InnerExample:[0m
[38;2;248;248;242m	var inner_value: int = 99[0m
[38;2;248;248;242m	func show_value() -> void:[0m
[38;2;248;248;242m		print("Inner value is:", inner_value)[0m

[38;2;248;248;242mfunc test_inner_class() -> void:[0m
[38;2;248;248;242m	var inner = InnerExample.new()[0m
[38;2;248;248;242m	inner.show_value()[0m

[38;2;248;248;242mfunc trigger_signal() -> void:[0m
[38;2;248;248;242m	emit_signal("custom_signal", "TestParam")[0m

[38;2;248;248;242mfunc _ready() -> void:[0m
[38;2;248;248;242m	say_hello()[0m
[38;2;248;248;242m	var result_add = add_numbers(5)[0m
[38;2;248;248;242m	print("Add result:", result_add)[0m
[38;2;248;248;242m	print("Process value for -5:", process_value(-5))[0m
[38;2;248;248;242m	print("Sum of array [10, 20, 30]:", sum_array([10, 20, 30]))[0m
[38;2;248;248;242m	print("Description for 2:", describe_number(2))[0m
[38;2;248;248;242m	print("Long description:\n", long_description())[0m
[38;2;248;248;242m	test_inner_class()[0m
[38;2;248;248;242m	trigger_signal()[0m
