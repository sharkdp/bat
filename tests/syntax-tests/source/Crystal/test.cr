# An example file to test Crystal syntax highlighting in bat
my_var : Nil = nil
my_var_also : Int32 = 42
my_other_var = 4.0
another_float = 4.0_f32
another_float_2 = 4e10
another_float_3 = -0.5
big_one = 1_000_000.111_111e-4
ternary = 1 > 2 : 3 ? 4
my_symbol = :ThisOne?
my_other_symbol = :No_That_One!
plus = :+
minus = :-
my_string : String = "this string right here, with an interpolated value of #{my_var_also}"
my_array : Array(Int32) = [1,2,3,4]
my_tuple : Tuple(Int32, Int32, Int32, Int32) = {1,2,3,4}
my_named_tuple : NamedTuple(one: Int32, two: Int32) = {"one": 1, "two": 2}
my_hash : Hash(String, Int32) = {"one" => 1, "two" => 2}
my_proc : Proc(Int32, Int32) = ->(x : Int32){ x * x}
my_other_proc : Proc(String) = ->{ "Wow, neat!" }
puts my_string
puts(my_string)

enum Colors
  Red
  Green
  Blue
end

class Greeter
  @instance_field = Colors::Red
  @@class_field = Colors::Green

  def initialize(@name = "world")
  end

  def greet 
    puts "Hello, #{@name}"
  end

  def render_greeting : String
    "Hello, #{@name}"
  end

  def with_greeting
    yield render_greeting
  end

  def is_color_default?
    @instance_field == @@class_field
  end

  def self.greet_static(name : String) : Unit
    puts "Hello, #{name}"
  end
end

greeter = Greeter.new("bat")
greeter.with_greeting do |greeting|
  puts greeting
end

puts <<-EOF
  this is a heredoc and it has a value in it of #{greeter.render_greeting}!
EOF

# This is a command:
`echo yay!`
$?.success?

my_color = Colors::Red

puts 
  case my_color
  when Colors::Red, .red?
    "Red"
  when Colors::Green, .green?
    "Green"
  when Colors::Blue, .blue?
    "Blue"
  else
    "I dunno, man. Chartreuse? Maroon?"
  end

class MyGenericClass(T)
  def initialize(@wrapped_value : T)
  end

  def get
    return @wrapped_value
  end
end


def do_stuff_with_range(r : Range(Int|String))
  return if r.empty?
  return unless !(r.empty?)
  r.each do |item|
    if /e/.match(item.to_s)
      puts "#{item} contains the letter e!"
    elsif item.to_s.empty?
      break
    else
      next # this is unnecessary, but whatever
    end
  end
end


macro print_range(range)
  {% for i in range %}
    puts {{i.id}}
  {% end %}
end

print_range(1..3)
print_range(1...3)
