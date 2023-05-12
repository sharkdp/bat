[38;2;248;248;242mversion = [0m[38;2;230;219;116m"0.1.0"[0m
[38;2;248;248;242mauthor = [0m[38;2;230;219;116m"creator_name"[0m
[38;2;248;248;242mdescription = [0m[38;2;230;219;116m"Tests nimble syntax highlighting"[0m
[38;2;248;248;242mlicense = [0m[38;2;230;219;116m"custom"[0m

[38;2;248;248;242mbin = @[[0m[38;2;230;219;116m"test"[0m[38;2;248;248;242m][0m
[38;2;248;248;242msrcdir = [0m[38;2;230;219;116m"src"[0m
[38;2;248;248;242minstallExt = @[[0m[38;2;230;219;116m"nim"[0m[38;2;248;248;242m][0m

[38;2;117;113;94m# [0m[38;2;117;113;94mDependencies[0m

[38;2;102;217;239mrequires[0m[38;2;248;248;242m [0m[38;2;230;219;116m"nim >= 1.6.2"[0m

[38;2;249;38;114mwhen[0m[38;2;248;248;242m [0m[38;2;249;38;114mdefined[0m[38;2;248;248;242m(nimdistros)[0m[38;2;249;38;114m:[0m
[38;2;248;248;242m  [0m[38;2;249;38;114mimport[0m[38;2;248;248;242m distros[0m
[38;2;248;248;242m  [0m[38;2;249;38;114mif[0m[38;2;248;248;242m [0m[38;2;102;217;239mdetectOs[0m[38;2;248;248;242m([0m[3m[38;2;166;226;46mUbuntu[0m[38;2;248;248;242m)[0m[38;2;249;38;114m:[0m
[38;2;248;248;242m    [0m[38;2;102;217;239mforeignDep[0m[38;2;248;248;242m [0m[38;2;230;219;116m"external_lib"[0m
[38;2;248;248;242m  [0m[38;2;249;38;114melse[0m[38;2;248;248;242m:[0m
[38;2;248;248;242m    [0m[38;2;102;217;239mforeignDep[0m[38;2;248;248;242m [0m[38;2;230;219;116m"other_lib"[0m

[38;2;102;217;239mtask[0m[38;2;248;248;242m test, [0m[38;2;230;219;116m"Runs a task called 'test'"[0m[38;2;249;38;114m:[0m
[38;2;248;248;242m  [0m[38;2;102;217;239mwithDir[0m[38;2;248;248;242m [0m[38;2;230;219;116m"tests"[0m[38;2;249;38;114m:[0m
[38;2;248;248;242m    [0m[38;2;102;217;239mexec[0m[38;2;248;248;242m [0m[38;2;230;219;116m"nim c -r tester"[0m
