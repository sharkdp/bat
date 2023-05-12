[38;2;249;38;114m\[0m[38;2;249;38;114mdocumentclass[0m[38;2;255;255;255m{[0m[3m[38;2;166;226;46ma[0m[3m[38;2;166;226;46mr[0m[3m[38;2;166;226;46mt[0m[3m[38;2;166;226;46mi[0m[3m[38;2;166;226;46mc[0m[3m[38;2;166;226;46ml[0m[3m[38;2;166;226;46me[0m[38;2;255;255;255m}[0m

[38;2;249;38;114m\[0m[38;2;249;38;114mbegin[0m[38;2;255;255;255m{[0m[3m[38;2;253;151;31mdocument[0m[38;2;255;255;255m}[0m

[38;2;102;217;239m\[0m[38;2;102;217;239msection*[0m[38;2;255;255;255m{[0m[38;2;166;226;46mIntroduction[0m[38;2;255;255;255m}[0m

[38;2;248;248;242mText outside code environments should follow TeX/LaTeX highlighting.[0m

[38;2;248;248;242mThe code environment delimiters themselves should be highlighted.[0m

[38;2;248;248;242mText inside code environments should follow regular Haskell highlighting.[0m

[38;2;102;217;239m\[0m[38;2;102;217;239mbegin[0m[38;2;255;255;255m{[0m[3m[38;2;253;151;31mcode[0m[38;2;255;255;255m}[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m           [0m[38;2;248;248;242mData.List[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m           [0m[38;2;248;248;242mSystem.Environment[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m           [0m[38;2;248;248;242mText.Printf[0m

[38;2;166;226;46mtwoSumN[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3m[38;2;102;217;239mInt[0m[38;2;248;248;242m [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [[0m[3m[38;2;102;217;239mInt[0m[38;2;248;248;242m] [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [[0m[3m[38;2;102;217;239mInt[0m[38;2;248;248;242m][0m
[38;2;248;248;242mtwoSumN _ [0m[38;2;190;132;255m[][0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255m[][0m
[38;2;248;248;242mtwoSumN n (x [0m[38;2;249;38;114m:[0m[38;2;248;248;242m xs) [0m[38;2;249;38;114m|[0m[38;2;248;248;242m (n [0m[38;2;249;38;114m-[0m[38;2;248;248;242m x) [0m[38;2;249;38;114m`[0m[38;2;249;38;114melem[0m[38;2;249;38;114m`[0m[38;2;248;248;242m xs [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [x[0m[38;2;248;248;242m,[0m[38;2;248;248;242m n [0m[38;2;249;38;114m-[0m[38;2;248;248;242m x][0m
[38;2;248;248;242m                   [0m[38;2;249;38;114m|[0m[38;2;248;248;242m otherwise         [0m[38;2;249;38;114m=[0m[38;2;248;248;242m twoSumN n xs[0m

[38;2;166;226;46mthreeSumN[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3m[38;2;102;217;239mInt[0m[38;2;248;248;242m [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [[0m[3m[38;2;102;217;239mInt[0m[38;2;248;248;242m] [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [[0m[3m[38;2;102;217;239mInt[0m[38;2;248;248;242m][0m
[38;2;248;248;242mthreeSumN _ [0m[38;2;190;132;255m[][0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255m[][0m
[38;2;248;248;242mthreeSumN n (x [0m[38;2;249;38;114m:[0m[38;2;248;248;242m xs) [0m[38;2;249;38;114m|[0m[38;2;248;248;242m null partial [0m[38;2;249;38;114m=[0m[38;2;248;248;242m threeSumN n xs[0m
[38;2;248;248;242m                     [0m[38;2;249;38;114m|[0m[38;2;248;248;242m otherwise    [0m[38;2;249;38;114m=[0m[38;2;248;248;242m x [0m[38;2;249;38;114m:[0m[38;2;248;248;242m partial[0m
[38;2;248;248;242m  [0m[38;2;249;38;114mwhere[0m[38;2;248;248;242m partial [0m[38;2;249;38;114m=[0m[38;2;248;248;242m twoSumN (n [0m[38;2;249;38;114m-[0m[38;2;248;248;242m x) xs[0m
[38;2;102;217;239m\[0m[38;2;102;217;239mend[0m[38;2;255;255;255m{[0m[3m[38;2;253;151;31mcode[0m[38;2;255;255;255m}[0m

[38;2;248;248;242mText in-between code environments.[0m
[38;2;117;113;94m% LaTeX comment.[0m

[38;2;102;217;239m\[0m[38;2;102;217;239mbegin[0m[38;2;255;255;255m{[0m[3m[38;2;253;151;31mcode[0m[38;2;255;255;255m}[0m
[38;2;166;226;46moutput[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3m[38;2;102;217;239mString[0m[38;2;248;248;242m [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [0m[3m[38;2;102;217;239mIO[0m[38;2;248;248;242m [0m[38;2;102;217;239m()[0m
[38;2;248;248;242moutput path [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;249;38;114mdo[0m
[38;2;248;248;242m  input [0m[38;2;249;38;114m<-[0m[38;2;248;248;242m sort [0m[38;2;249;38;114m.[0m[38;2;248;248;242m map read [0m[38;2;249;38;114m.[0m[38;2;248;248;242m filter (not [0m[38;2;249;38;114m.[0m[38;2;248;248;242m null) [0m[38;2;249;38;114m.[0m[38;2;248;248;242m lines [0m[38;2;249;38;114m<$>[0m[38;2;248;248;242m readFile path[0m
[38;2;248;248;242m  printf [0m[38;2;230;219;116m"[0m[38;2;230;219;116mFile: %s[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m"[0m[38;2;248;248;242m path[0m
[38;2;248;248;242m  printf [0m[38;2;230;219;116m"[0m[38;2;230;219;116m  Part 1: %d[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m.[0m[38;2;248;248;242m product [0m[38;2;249;38;114m.[0m[38;2;248;248;242m twoSumN [0m[38;2;190;132;255m2020[0m[38;2;248;248;242m [0m[38;2;249;38;114m$[0m[38;2;248;248;242m input[0m
[38;2;248;248;242m  printf [0m[38;2;230;219;116m"[0m[38;2;230;219;116m  Part 2: %d[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m.[0m[38;2;248;248;242m product [0m[38;2;249;38;114m.[0m[38;2;248;248;242m threeSumN [0m[38;2;190;132;255m2020[0m[38;2;248;248;242m [0m[38;2;249;38;114m$[0m[38;2;248;248;242m input[0m

[38;2;117;113;94m--[0m[38;2;117;113;94m Haskell comment inside code environment.[0m

[38;2;166;226;46mmain[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3m[38;2;102;217;239mIO[0m[38;2;248;248;242m [0m[38;2;102;217;239m()[0m
[38;2;248;248;242mmain [0m[38;2;249;38;114m=[0m[38;2;248;248;242m getArgs [0m[38;2;249;38;114m>>=[0m[38;2;248;248;242m mapM_ output[0m
[38;2;102;217;239m\[0m[38;2;102;217;239mend[0m[38;2;255;255;255m{[0m[3m[38;2;253;151;31mcode[0m[38;2;255;255;255m}[0m

[38;2;249;38;114m\[0m[38;2;249;38;114mend[0m[38;2;255;255;255m{[0m[3m[38;2;253;151;31mdocument[0m[38;2;255;255;255m}[0m
