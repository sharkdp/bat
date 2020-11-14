set part1 hello
set part2 how; set part3 are
set part4 you

set part2;

set greeting $part1$part2$part3$part4

set somevar {
    This is a literal $ sign, and this \} escaped
    brace remains uninterpreted
}

set name Neo
set greeting "Hello, $name"

variable name NotNeo

namespace eval people {
    set name NeoAgain
}

