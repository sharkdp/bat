[38;2;249;38;114mdefmodule[0m[38;2;248;248;242m [0m[4m[38;2;102;217;239mCharlex[0m[38;2;248;248;242m.[0m[4m[38;2;102;217;239mCommand[0m[38;2;248;248;242m [0m[38;2;249;38;114mdo[0m
[38;2;248;248;242m  [0m[38;2;249;38;114m@[0m[38;2;255;255;255mcallback[0m[38;2;248;248;242m usage[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m :: [0m[38;2;248;248;242m[[0m[4m[38;2;102;217;239mString[0m[38;2;248;248;242m.[0m[38;2;248;248;242mt[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m][0m

[38;2;248;248;242m  [0m[38;2;249;38;114m@[0m[38;2;255;255;255mcallback[0m[38;2;248;248;242m description[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m :: [0m[4m[38;2;102;217;239mString[0m[38;2;248;248;242m.[0m[38;2;248;248;242mt[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m

[38;2;248;248;242m  [0m[38;2;249;38;114m@[0m[38;2;255;255;255mcallback[0m[38;2;248;248;242m parse_args[0m[38;2;248;248;242m([0m[38;2;248;248;242margs :: [0m[4m[38;2;102;217;239mString[0m[38;2;248;248;242m.[0m[38;2;248;248;242mt[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m)[0m[38;2;248;248;242m :: any[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m

[38;2;248;248;242m  [0m[38;2;249;38;114m@[0m[38;2;255;255;255mcallback[0m[38;2;248;248;242m run[0m[38;2;248;248;242m([0m[38;2;248;248;242mcontext :: [0m[4m[38;2;102;217;239mMap[0m[38;2;248;248;242m.[0m[38;2;248;248;242mt[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m,[0m[38;2;248;248;242m args :: [0m[38;2;248;248;242m[[0m[4m[38;2;102;217;239mString[0m[38;2;248;248;242m.[0m[38;2;248;248;242mt[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m][0m[38;2;248;248;242m [0m[38;2;249;38;114m|[0m[38;2;248;248;242m any[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m)[0m[38;2;248;248;242m :: [0m[4m[38;2;102;217;239mString[0m[38;2;248;248;242m.[0m[38;2;248;248;242mt[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;249;38;114m|[0m[38;2;248;248;242m any[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m

[38;2;248;248;242m  [0m[38;2;249;38;114m@[0m[38;2;255;255;255moptional_callbacks[0m[38;2;248;248;242m [0m[38;2;190;132;255musage[0m[38;2;190;132;255m:[0m[38;2;248;248;242m [0m[38;2;190;132;255m0[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mdescription[0m[38;2;190;132;255m:[0m[38;2;248;248;242m [0m[38;2;190;132;255m0[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mparse_args[0m[38;2;190;132;255m:[0m[38;2;248;248;242m [0m[38;2;190;132;255m1[0m

[38;2;248;248;242m  [0m[38;2;249;38;114mdefmacro[0m[38;2;248;248;242m [0m[38;2;166;226;46m__using__[0m[38;2;248;248;242m([0m[38;2;248;248;242m_opts[0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;249;38;114mdo[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mquote[0m[38;2;248;248;242m [0m[38;2;249;38;114mdo[0m
[38;2;248;248;242m      [0m[38;2;249;38;114m@[0m[38;2;255;255;255mbehaviour[0m[38;2;248;248;242m [0m[38;2;249;38;114munquote[0m[38;2;248;248;242m([0m[38;2;255;255;255m__MODULE__[0m[38;2;248;248;242m)[0m

[38;2;248;248;242m      [0m[38;2;249;38;114m@[0m[38;2;255;255;255mimpl[0m[38;2;248;248;242m [0m[38;2;190;132;255mtrue[0m
[38;2;248;248;242m      [0m[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46musage[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mdo:[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116mThis command is without usage, will add it soon[0m[38;2;230;219;116m"[0m

[38;2;248;248;242m      [0m[38;2;249;38;114m@[0m[38;2;255;255;255mimpl[0m[38;2;248;248;242m [0m[38;2;190;132;255mtrue[0m
[38;2;248;248;242m      [0m[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46mdescription[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mdo:[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116mThis command is without description, will add it soon[0m[38;2;230;219;116m"[0m

[38;2;248;248;242m      [0m[38;2;249;38;114m@[0m[38;2;255;255;255mimpl[0m[38;2;248;248;242m [0m[38;2;190;132;255mtrue[0m
[38;2;248;248;242m      [0m[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46mrun[0m[38;2;248;248;242m([0m[38;2;248;248;242m_context[0m[38;2;248;248;242m,[0m[38;2;248;248;242m _args[0m[38;2;248;248;242m)[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mdo:[0m[38;2;248;248;242m [0m[38;2;190;132;255m:[0m[38;2;190;132;255mok[0m

[38;2;248;248;242m      [0m[38;2;249;38;114m@[0m[38;2;255;255;255mimpl[0m[38;2;248;248;242m [0m[38;2;190;132;255mtrue[0m
[38;2;248;248;242m      [0m[38;2;249;38;114mdef[0m[38;2;248;248;242m [0m[38;2;166;226;46mparse_args[0m[38;2;248;248;242m([0m[38;2;248;248;242margs[0m[38;2;248;248;242m)[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mdo:[0m[38;2;248;248;242m args [0m[38;2;249;38;114m|>[0m[38;2;248;248;242m [0m[4m[38;2;102;217;239mString[0m[38;2;248;248;242m.[0m[38;2;248;248;242msplit[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;249;38;114m|>[0m[38;2;248;248;242m [0m[4m[38;2;102;217;239mKernel[0m[38;2;248;248;242m.[0m[38;2;248;248;242mtl[0m[38;2;248;248;242m([0m[38;2;248;248;242m)[0m

[38;2;248;248;242m      [0m[38;2;249;38;114mdefoverridable[0m[38;2;248;248;242m [0m[38;2;190;132;255mparse_args[0m[38;2;190;132;255m:[0m[38;2;248;248;242m [0m[38;2;190;132;255m1[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mdescription[0m[38;2;190;132;255m:[0m[38;2;248;248;242m [0m[38;2;190;132;255m0[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mrun[0m[38;2;190;132;255m:[0m[38;2;248;248;242m [0m[38;2;190;132;255m2[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255musage[0m[38;2;190;132;255m:[0m[38;2;248;248;242m [0m[38;2;190;132;255m0[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mend[0m
[38;2;248;248;242m  [0m[38;2;249;38;114mend[0m
[38;2;249;38;114mend[0m
