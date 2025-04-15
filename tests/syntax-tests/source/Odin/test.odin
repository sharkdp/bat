package main

import "core:fmt"
import "core:math"

Vector :: struct {
	components: []f64,
}

euclidean_distance :: proc(v1: Vector, v2: Vector) -> f64 {
	if len(v1.components) != len(v2.components) {
		panic("Vectors must be same dimension")
	}
	sum: f64 = 0.0;
	for i, comp in v1.components {
		diff := comp - v2.components[i];
		sum += diff * diff;
	}
	return math.sqrt(sum);
}

main :: proc() {
	v1: Vector = Vector{components = []f64{1.0, 2.0, 3.0}};
	v2: Vector = Vector{components = []f64{4.0, 6.0, 8.0}};
	dist: f64 = euclidean_distance(v1, v2);
	fmt.println("Distance:", dist);
}
