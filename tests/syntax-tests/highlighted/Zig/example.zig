[38;2;248;248;242m//! this is a top level doc, starts with "//!"[0m

[38;2;248;248;242mconst std = @import("std");[0m

[38;2;248;248;242mpub fn main() anyerror!void {[0m
[38;2;248;248;242m    const stdout = std.io.getStdOut().writer();[0m
[38;2;248;248;242m    try stdout.print("Hello, {}!\n", .{"world"});[0m
[38;2;248;248;242m}[0m

[38;2;248;248;242mconst expect = std.testing.expect;[0m

[38;2;248;248;242mtest "comments" {[0m
[38;2;248;248;242m    // comments start with "//" until newline[0m
[38;2;248;248;242m    // foo bar baz[0m
[38;2;248;248;242m    const x = true; // another comment[0m
[38;2;248;248;242m    expect(x);[0m
[38;2;248;248;242m}[0m

[38;2;248;248;242m/// a doc comment starts with "///"[0m
[38;2;248;248;242m/// multiple lines are merged together[0m
[38;2;248;248;242mconst Timestamp = struct {[0m
[38;2;248;248;242m    /// number of seconds since epoch[0m
[38;2;248;248;242m    seconds: i64,[0m

[38;2;248;248;242m    /// number of nanoseconds past the second[0m
[38;2;248;248;242m    nano: u32,[0m

[38;2;248;248;242m    const Self = @This();[0m

[38;2;248;248;242m    pub fn unixEpoch() Self {[0m
[38;2;248;248;242m        return Self{[0m
[38;2;248;248;242m            .seconds = 0,[0m
[38;2;248;248;242m            .nanos = 0,[0m
[38;2;248;248;242m        };[0m
[38;2;248;248;242m    }[0m
[38;2;248;248;242m};[0m

[38;2;248;248;242mconst my_val = switch (std.Target.current.os.tag) {[0m
[38;2;248;248;242m    .linux => "Linux",[0m
[38;2;248;248;242m    else => "not Linux",[0m
[38;2;248;248;242m};[0m

[38;2;248;248;242mconst Book = enum {[0m
[38;2;248;248;242m    paperback,[0m
[38;2;248;248;242m    hardcover,[0m
[38;2;248;248;242m    ebook,[0m
[38;2;248;248;242m    pdf,[0m
[38;2;248;248;242m};[0m

[38;2;248;248;242mconst TokenType = union(enum) {[0m
[38;2;248;248;242m    int: isize,[0m
[38;2;248;248;242m    float: f64,[0m
[38;2;248;248;242m    string: []const u8,[0m
[38;2;248;248;242m};[0m

[38;2;248;248;242mconst array_lit: [4]u8 = .{ 11, 22, 33, 44 };[0m
[38;2;248;248;242mconst sentinal_lit = [_:0]u8{ 1, 2, 3, 4 };[0m

[38;2;248;248;242mtest "address of syntax" {[0m
[38;2;248;248;242m    // Get the address of a variable:[0m
[38;2;248;248;242m    const x: i32 = 1234;[0m
[38;2;248;248;242m    const x_ptr = &x;[0m

[38;2;248;248;242m    // Dereference a pointer:[0m
[38;2;248;248;242m    expect(x_ptr.* == 1234);[0m

[38;2;248;248;242m    // When you get the address of a const variable, you get a const pointer to a single item.[0m
[38;2;248;248;242m    expect(@TypeOf(x_ptr) == *const i32);[0m

[38;2;248;248;242m    // If you want to mutate the value, you'd need an address of a mutable variable:[0m
[38;2;248;248;242m    var y: i32 = 5678;[0m
[38;2;248;248;242m    const y_ptr = &y;[0m
[38;2;248;248;242m    expect(@TypeOf(y_ptr) == *i32);[0m
[38;2;248;248;242m    y_ptr.* += 1;[0m
[38;2;248;248;242m    expect(y_ptr.* == 5679);[0m
[38;2;248;248;242m}[0m

[38;2;248;248;242m// integer literals[0m
[38;2;248;248;242mconst decimal_int = 98222;[0m
[38;2;248;248;242mconst hex_int = 0xff;[0m
[38;2;248;248;242mconst another_hex_int = 0xFF;[0m
[38;2;248;248;242mconst octal_int = 0o755;[0m
[38;2;248;248;242mconst binary_int = 0b11110000;[0m

[38;2;248;248;242m// underscores may be placed between two digits as a visual separator[0m
[38;2;248;248;242mconst one_billion = 1_000_000_000;[0m
[38;2;248;248;242mconst binary_mask = 0b1_1111_1111;[0m
[38;2;248;248;242mconst permissions = 0o7_5_5;[0m
[38;2;248;248;242mconst big_address = 0xFF80_0000_0000_0000;[0m

[38;2;248;248;242m// float literals[0m
[38;2;248;248;242mconst floating_point = 123.0E+77;[0m
[38;2;248;248;242mconst another_float = 123.0;[0m
[38;2;248;248;242mconst yet_another = 123.0e+77;[0m

[38;2;248;248;242mconst hex_floating_point = 0x103.70p-5;[0m
[38;2;248;248;242mconst another_hex_float = 0x103.70;[0m
[38;2;248;248;242mconst yet_another_hex_float = 0x103.70P-5;[0m

[38;2;248;248;242m// underscores may be placed between two digits as a visual separator[0m
[38;2;248;248;242mconst lightspeed = 299_792_458.000_000;[0m
[38;2;248;248;242mconst nanosecond = 0.000_000_001;[0m
[38;2;248;248;242mconst more_hex = 0x1234_5678.9ABC_CDEFp-10;[0m

[38;2;248;248;242mfn max(comptime T: type, a: T, b: T) T {[0m
[38;2;248;248;242m    return if (a > b) a else b;[0m
[38;2;248;248;242m}[0m
