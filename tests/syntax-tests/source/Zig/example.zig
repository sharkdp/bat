//! this is a top level doc, starts with "//!"

const std = @import("std");

pub fn main() anyerror!void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, {}!\n", .{"world"});
}

const expect = std.testing.expect;

test "comments" {
    // comments start with "//" until newline
    // foo bar baz
    const x = true; // another comment
    expect(x);
}

/// a doc comment starts with "///"
/// multiple lines are merged together
const Timestamp = struct {
    /// number of seconds since epoch
    seconds: i64,

    /// number of nanoseconds past the second
    nano: u32,

    const Self = @This();

    pub fn unixEpoch() Self {
        return Self{
            .seconds = 0,
            .nanos = 0,
        };
    }
};

const my_val = switch (std.Target.current.os.tag) {
    .linux => "Linux",
    else => "not Linux",
};

const Book = enum {
    paperback,
    hardcover,
    ebook,
    pdf,
};

const TokenType = union(enum) {
    int: isize,
    float: f64,
    string: []const u8,
};

const array_lit: [4]u8 = .{ 11, 22, 33, 44 };
const sentinal_lit = [_:0]u8{ 1, 2, 3, 4 };

test "address of syntax" {
    // Get the address of a variable:
    const x: i32 = 1234;
    const x_ptr = &x;

    // Dereference a pointer:
    expect(x_ptr.* == 1234);

    // When you get the address of a const variable, you get a const pointer to a single item.
    expect(@TypeOf(x_ptr) == *const i32);

    // If you want to mutate the value, you'd need an address of a mutable variable:
    var y: i32 = 5678;
    const y_ptr = &y;
    expect(@TypeOf(y_ptr) == *i32);
    y_ptr.* += 1;
    expect(y_ptr.* == 5679);
}

// integer literals
const decimal_int = 98222;
const hex_int = 0xff;
const another_hex_int = 0xFF;
const octal_int = 0o755;
const binary_int = 0b11110000;

// underscores may be placed between two digits as a visual separator
const one_billion = 1_000_000_000;
const binary_mask = 0b1_1111_1111;
const permissions = 0o7_5_5;
const big_address = 0xFF80_0000_0000_0000;

// float literals
const floating_point = 123.0E+77;
const another_float = 123.0;
const yet_another = 123.0e+77;

const hex_floating_point = 0x103.70p-5;
const another_hex_float = 0x103.70;
const yet_another_hex_float = 0x103.70P-5;

// underscores may be placed between two digits as a visual separator
const lightspeed = 299_792_458.000_000;
const nanosecond = 0.000_000_001;
const more_hex = 0x1234_5678.9ABC_CDEFp-10;

fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}
