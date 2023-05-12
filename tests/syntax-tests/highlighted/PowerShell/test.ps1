[38;2;117;113;94m#[0m[38;2;117;113;94m PowerShell script for testing syntax highlighting[0m

[3m[38;2;102;217;239mfunction[0m[38;2;248;248;242m [0m[38;2;166;226;46mGet-FutureTime[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mparam[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m
[38;2;248;248;242m        [0m[38;2;248;248;242m[[0m[3m[38;2;102;217;239mInt32[0m[38;2;248;248;242m][0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;255;255;255mMinutes[0m
[38;2;248;248;242m    [0m[38;2;248;248;242m)[0m
[38;2;248;248;242m    [0m
[38;2;248;248;242m    [0m[38;2;255;255;255m$[0m[38;2;255;255;255mtime[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;102;217;239mGet-Date[0m[38;2;248;248;242m [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;249;38;114m%[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;248;248;242m_[0m[38;2;255;255;255m.AddMinutes[0m[38;2;248;248;242m([0m[38;2;255;255;255m$[0m[38;2;255;255;255mMinutes[0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m}[0m
[38;2;248;248;242m    [0m[38;2;230;219;116m"[0m[38;2;230;219;116m{0:d2}:{1:d2}:{2:d2}[0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m-f[0m[38;2;248;248;242m [0m[38;2;249;38;114m@[0m[38;2;248;248;242m([0m[38;2;255;255;255m$[0m[38;2;255;255;255mtime[0m[38;2;255;255;255m.hour[0m[38;2;249;38;114m,[0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;255;255;255mtime[0m[38;2;255;255;255m.minute[0m[38;2;249;38;114m,[0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;255;255;255mtime[0m[38;2;255;255;255m.second[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m}[0m

[38;2;249;38;114mif[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[38;2;255;255;255m$[0m[38;2;255;255;255menv:[0m[38;2;255;255;255mPATH[0m[38;2;248;248;242m [0m[38;2;249;38;114m-match[0m[38;2;248;248;242m [0m[38;2;230;219;116m'[0m[38;2;230;219;116m.*rust.*[0m[38;2;230;219;116m'[0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m   [0m[38;2;230;219;116m'[0m[38;2;230;219;116mPath contains Rust[0m[38;2;230;219;116m'[0m
[38;2;248;248;242m    [0m[38;2;249;38;114mtry[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m        [0m[38;2;249;38;114m&[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116mcargo[0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116m--version[0m[38;2;230;219;116m"[0m
[38;2;248;248;242m    [0m[38;2;248;248;242m}[0m[38;2;248;248;242m [0m[38;2;249;38;114mcatch[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m        [0m[38;2;102;217;239mWrite-Error[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116mFailed to run cargo[0m[38;2;230;219;116m"[0m
[38;2;248;248;242m    [0m[38;2;248;248;242m}[0m
[38;2;248;248;242m}[0m[38;2;248;248;242m [0m[38;2;249;38;114melse[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m
[38;2;248;248;242m   [0m[38;2;230;219;116m'[0m[38;2;230;219;116mPath does not contain Rust[0m[38;2;230;219;116m'[0m
[38;2;248;248;242m}[0m

[38;2;248;248;242m([0m[38;2;190;132;255m5[0m[38;2;249;38;114m..[0m[38;2;190;132;255m30[0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;249;38;114m?[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;248;248;242m_[0m[38;2;248;248;242m [0m[38;2;249;38;114m%[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[38;2;190;132;255m2[0m[38;2;248;248;242m [0m[38;2;249;38;114m*[0m[38;2;248;248;242m [0m[38;2;190;132;255m2[0m[38;2;248;248;242m [0m[38;2;249;38;114m+[0m[38;2;248;248;242m [0m[38;2;190;132;255m1[0m[38;2;248;248;242m)[0m[38;2;248;248;242m [0m[38;2;249;38;114m-eq[0m[38;2;248;248;242m [0m[38;2;190;132;255m0[0m[38;2;248;248;242m [0m[38;2;248;248;242m}[0m[38;2;248;248;242m [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;249;38;114m%[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;230;219;116m"[0m[38;2;230;219;116mIn {0} minutes, the time will be {1}.[0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m-f[0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;248;248;242m_[0m[38;2;249;38;114m,[0m[38;2;248;248;242m [0m[38;2;249;38;114m$[0m[38;2;248;248;242m([0m[38;2;248;248;242m [0m[38;2;102;217;239mGet-FutureTime[0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;248;248;242m_[0m[38;2;248;248;242m [0m[38;2;248;248;242m)[0m[38;2;248;248;242m}[0m
[38;2;255;255;255m$[0m[38;2;255;255;255mlater[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m  [0m[38;2;102;217;239mGet-FutureTime[0m[38;2;248;248;242m [0m[38;2;249;38;114m-[0m[38;2;248;248;242mMinutes [0m[38;2;249;38;114m$[0m[38;2;248;248;242m([0m[38;2;248;248;242m [0m[38;2;102;217;239mGet-Random[0m[38;2;248;248;242m [0m[38;2;249;38;114m-[0m[38;2;248;248;242mMinimum [0m[38;2;190;132;255m60[0m[38;2;248;248;242m [0m[38;2;249;38;114m-[0m[38;2;248;248;242mMaximum [0m[38;2;190;132;255m120[0m[38;2;248;248;242m [0m[38;2;248;248;242m)[0m
[38;2;230;219;116m"[0m[38;2;230;219;116mThe time will be [0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m+[0m[38;2;248;248;242m [0m[38;2;255;255;255m$[0m[38;2;255;255;255mlater[0m[38;2;248;248;242m [0m[38;2;249;38;114m+[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116m later.[0m[38;2;230;219;116m"[0m
