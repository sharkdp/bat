[38;2;248;248;242mstruct VertexOut {[0m
[38;2;248;248;242m  @builtin(position) position : vec4f,[0m
[38;2;248;248;242m  @location(0) color : vec4f[0m
[38;2;248;248;242m}[0m

[38;2;248;248;242m@vertex[0m
[38;2;248;248;242mfn vertex_main(@location(0) position: vec4f,[0m
[38;2;248;248;242m               @location(1) color: vec4f) -> VertexOut[0m
[38;2;248;248;242m{[0m
[38;2;248;248;242m  var output : VertexOut;[0m
[38;2;248;248;242m  output.position = position;[0m
[38;2;248;248;242m  output.color = color;[0m
[38;2;248;248;242m  return output;[0m
[38;2;248;248;242m}[0m

[38;2;248;248;242m@fragment[0m
[38;2;248;248;242mfn fragment_main(fragData: VertexOut) -> @location(0) vec4f[0m
[38;2;248;248;242m{[0m
[38;2;248;248;242m  return fragData.color;[0m
[38;2;248;248;242m}[0m
