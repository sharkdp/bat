[38;2;248;248;242m{[0m[38;2;248;248;242m [0m[3;38;2;253;151;31mnixpkgs[0m[38;2;248;248;242m [0m[38;2;249;38;114m?[0m[38;2;248;248;242m [0m[38;2;230;219;116m<nixpkgs>[0m
[38;2;249;38;114m,[0m[38;2;248;248;242m [0m[3;38;2;253;151;31mnixpkgs'[0m[38;2;248;248;242m [0m[38;2;249;38;114m?[0m[38;2;248;248;242m [0m[38;2;102;217;239mimport[0m[38;2;248;248;242m [0m[3;38;2;253;151;31mnixpkgs[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m}[0m[38;2;248;248;242m}[0m[38;2;248;248;242m:[0m[38;2;248;248;242m [0m[38;2;249;38;114mwith[0m[38;2;248;248;242m [0m[3;38;2;253;151;31mnixpkgs'[0m[38;2;248;248;242m;[0m

[38;2;117;113;94m# some comment[0m
[3;38;2;253;151;31mstdenv[0m[38;2;249;38;114m.[0m[3;38;2;253;151;31mmkDerivation[0m[38;2;248;248;242m [0m[38;2;249;38;114mrec[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m  [0m[38;2;166;226;46mpname[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116mtest[0m[38;2;230;219;116m"[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m  [0m[38;2;166;226;46mversion[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116m0.2.3[0m[38;2;230;219;116m"[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m  [0m[38;2;166;226;46mname[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[3;38;2;228;46;112m${[0m[3;38;2;253;151;31mpname[0m[3;38;2;228;46;112m}[0m[38;2;230;219;116m-[0m[3;38;2;228;46;112m${[0m[3;38;2;253;151;31mversion[0m[3;38;2;228;46;112m}[0m[38;2;230;219;116m"[0m[38;2;248;248;242m;[0m

[38;2;248;248;242m  [0m[38;2;166;226;46msrc[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;248;248;240m.[0m[38;2;248;248;240m/[0m[38;2;248;248;242m;[0m

[38;2;248;248;242m  [0m[38;2;166;226;46mbuildInputs[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;248;248;242m[[0m
[38;2;248;248;242m    [0m[3;38;2;253;151;31mgzip[0m
[38;2;248;248;242m    [0m[3;38;2;253;151;31mbzip2[0m
[38;2;248;248;242m    [0m[3;38;2;253;151;31mpython27[0m
[38;2;248;248;242m  [0m[38;2;248;248;242m][0m[38;2;248;248;242m;[0m
[38;2;248;248;242m}[0m
