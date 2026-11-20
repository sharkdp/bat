// Package example demonstrates CUE syntax.
package example

import (
	"strings"
	"list"
	yamlpkg "encoding/yaml"
)

// Definitions and constraints
#Port: int & >0 & <=65535

#Service: {
	name!:     string & =~"^[a-z][a-z0-9-]*$"
	image:     string
	replicas?: uint | *1
	ports: [...#Port]
	env: [string]: string
	labels: {
		app: name
		...
	}
	_hidden: true
	#nested: bool
}

// Concrete values
services: [Name=string]: #Service & {name: Name}

services: frontend: {
	image:    "nginx:1.25"
	replicas: 3
	ports: [80, 443]
	env: {
		LOG_LEVEL: "info"
		TIMEOUT:   "30s"
	}
}

services: backend: {
	image: "registry.example.com/backend:\(version)"
	ports: [8080]
	env: DATABASE_URL: "postgres://db:5432/app"
}

version: "v1.2.3"

// Numbers
numbers: {
	decimal:    1_000_000
	float:      3.14159
	exponent:   6.022e23
	hex:        0xFF_FF
	octal:      0o755
	binary:     0b1010_1010
	multiplier: 1.5Gi
	kilo:       4K
}

// Strings and bytes
strs: {
	simple:    "hello, world"
	escaped:   "tab\tnewline\nunicode\u00e9"
	bytes:     'raw bytes \x00\xff'
	raw:       #"no \(interpolation) here"#
	multiline: """
		multiple
		lines with \(version)
		"""
	multiBytes: '''
		byte
		lines
		'''
}

// Builtin types and values
types: {
	n: null
	b: bool | *false
	i: int8 | int16 | int32 | int64
	f: float32 | float64
	s: string
	y: bytes
	r: rune
	t: _
	x: number
}

// Bottom (error) value
bottom: _|_ | 1

// Operators and builtins
ops: {
	sum:      1 + 2*3 - 4/2
	divided:  div(7, 2)
	modulo:   mod(7, 2)
	quotient: quo(-7, 2)
	rest:     rem(-7, 2)
	eq:       1 == 1
	ne:       "a" != "b"
	both:     true && false
	either:   true || false
	not:      !true
	match:    "abc" =~ "^a"
	nomatch:  "abc" !~ "^z"
	length: len([1, 2, 3])
	closed: close({a: 1})
	upper: strings.ToUpper("cue")
	sorted: list.Sort([3, 1, 2], list.Ascending)
	disj: or([1, 2, 3])
	conj: and([int, >0])
	asYAML: yamlpkg.Marshal({a: 1})
}

// Nested calls, grouping and interpolated calls
calls: {
	allPorts: list.Concat([
		[22, 25],
		list.Concat([for _, s in services {s.ports}]),
	])
	nested:  len(strings.Split("a,b", ","))
	grouped: ((1 + 2) * 3)
	interp:  "x-\(strings.ToUpper("y"))-\(len([1, 2]))"
}

// Comprehensions
let prefix = "svc-"

serviceNames: [for k, v in services if v.replicas > 1 {prefix + k}]

portMap: {
	for name, svc in services {
		for i, p in svc.ports {
			"\(name)-\(i)": p
		}
	}
}

byPort: {
	for name, svc in services
	if len(svc.ports) > 0
	let first = svc.ports[0] {
		"\(first)":        name
		(name + "-image"): svc.image
	}
}

if version != "" {
	hasVersion: true
}

// Aliases, optional and required fields, pattern constraints
X=outer: {
	inner: 1
	ref:   X.inner
}

optional?: string
required!: int
[=~"^x-"]: string

// Attributes
config: {
	port:  int  @tag(port, type=int)
	debug: bool @json(",omitempty") @yaml("debug")
}

// Embedding and default values
#Base: {
	kind: *"base" | "derived"
}

derived: #Base & {kind: "derived"}

#Limits: {
	cpu: {min: number, max: number} | *{min: 0, max: 1}
	tags: [...string] | *[]
}

// Lists
emptyList: []
openList: [1, 2, ...]
typedList: [...string]
nestedList: [[1, 2], [3, 4]]
