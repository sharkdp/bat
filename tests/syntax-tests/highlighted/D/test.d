[38;2;117;113;94m//[0m[38;2;117;113;94m selective import[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;255;255;255mstd[0m[38;2;248;248;242m.[0m[38;2;255;255;255mstdio[0m[38;2;248;248;242m [0m[38;2;248;248;242m:[0m[38;2;248;248;242m [0m[38;2;255;255;255mwriteln[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;255;255;255mwritefln[0m[38;2;248;248;242m;[0m
[38;2;117;113;94m//[0m[38;2;117;113;94m non-selective import[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;255;255;255mstd[0m[38;2;248;248;242m.[0m[38;2;255;255;255malgorithm[0m[38;2;248;248;242m;[0m

[38;2;117;113;94m/*[0m[38;2;117;113;94m a multiline comment[0m
[38;2;117;113;94m*[0m
[38;2;117;113;94m*[0m[38;2;117;113;94m this function is safe because it doesn't use pointer arithmetic[0m
[38;2;117;113;94m*/[0m
[3m[38;2;102;217;239mint[0m[38;2;248;248;242m [0m[38;2;166;226;46mthe_ultimate_answer[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;249;38;114m@[0m[38;2;249;38;114msafe[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m	[0m[38;2;117;113;94m//[0m[38;2;117;113;94m assert1on[0m
[38;2;248;248;242m	[0m[38;2;249;38;114massert[0m[38;2;248;248;242m([0m[38;2;190;132;255m1[0m[38;2;248;248;242m [0m[38;2;249;38;114m!=[0m[38;2;248;248;242m [0m[38;2;190;132;255m2[0m[38;2;248;248;242m)[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m        [0m[38;2;117;113;94m//[0m[38;2;117;113;94m now we can safely return our answer	[0m
[38;2;248;248;242m	[0m[38;2;249;38;114mreturn[0m[38;2;248;248;242m [0m[38;2;190;132;255m42[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m}[0m

[3m[38;2;102;217;239mvoid[0m[38;2;248;248;242m [0m[38;2;166;226;46mmain[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m
[38;2;248;248;242m{[0m
[38;2;248;248;242m    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m function call with string literal[0m
[38;2;248;248;242m    [0m[38;2;248;248;242mwriteln[0m[38;2;248;248;242m([0m[38;2;230;219;116m"[0m[38;2;230;219;116mHello World![0m[38;2;230;219;116m"[0m[38;2;248;248;242m)[0m[38;2;248;248;242m;[0m

[38;2;248;248;242m    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m an int array declaration[0m
[38;2;248;248;242m    [0m[3m[38;2;102;217;239mint[0m[38;2;248;248;242m[[0m[38;2;248;248;242m][0m[38;2;248;248;242m [0m[38;2;255;255;255marr1[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;248;248;242m[[0m[38;2;190;132;255m1[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255m2[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255m3[0m[38;2;248;248;242m][0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m an immutable double[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mimmutable[0m[38;2;248;248;242m [0m[3m[38;2;102;217;239mdouble[0m[38;2;248;248;242m [0m[38;2;255;255;255mpi[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255m3[0m[38;2;190;132;255m.[0m[38;2;190;132;255m14[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m a mutable double[0m
[38;2;248;248;242m    [0m[3m[38;2;102;217;239mdouble[0m[38;2;248;248;242m [0m[38;2;255;255;255md1[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;255;255;255mpi[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m a pointer[0m
[38;2;248;248;242m    [0m[3m[38;2;102;217;239mdouble[0m[38;2;249;38;114m*[0m[38;2;248;248;242m [0m[38;2;255;255;255mdp1[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;249;38;114m&[0m[38;2;255;255;255md1[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m another pointer to the same thingy[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mauto[0m[38;2;248;248;242m [0m[38;2;255;255;255ma1[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;249;38;114m&[0m[38;2;255;255;255md1[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m a constant bool[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mconst[0m[38;2;248;248;242m [0m[3m[38;2;102;217;239mbool[0m[38;2;248;248;242m [0m[38;2;255;255;255mb1[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mtrue[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mif[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[38;2;255;255;255mb1[0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m	    [0m[38;2;117;113;94m//[0m[38;2;117;113;94m another function call [0m
[38;2;248;248;242m	    [0m[38;2;248;248;242mwritefln[0m[38;2;248;248;242m([0m[38;2;230;219;116m"[0m[38;2;230;219;116m%s[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m%s[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m%s[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m"[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;255;255;255marr1[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;255;255;255md1[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;248;248;242mthe_ultimate_answer[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m)[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;248;248;242m}[0m
[38;2;248;248;242m    [0m[38;2;249;38;114melse[0m[38;2;248;248;242m [0m[38;2;249;38;114mif[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[38;2;249;38;114m![0m[38;2;255;255;255mb1[0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m	    [0m[38;2;248;248;242mwriteln[0m[38;2;248;248;242m([0m[38;2;230;219;116m"[0m[38;2;230;219;116mthis seems wrong[0m[38;2;230;219;116m"[0m[38;2;248;248;242m)[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;248;248;242m}[0m
[38;2;248;248;242m    [0m[38;2;249;38;114melse[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m	    [0m[38;2;248;248;242mwriteln[0m[38;2;248;248;242m([0m[38;2;230;219;116m"[0m[38;2;230;219;116mI'm giving up, this is too crazy for me[0m[38;2;230;219;116m"[0m[38;2;248;248;242m)[0m[38;2;248;248;242m;[0m
[38;2;248;248;242m    [0m[38;2;248;248;242m}[0m
[38;2;248;248;242m}[0m
