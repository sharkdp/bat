[38;2;249;38;114mARG [0m[38;2;248;248;242marchitecture=amd64[0m
[38;2;249;38;114mFROM[0m[38;2;248;248;242m $architecture/centos:[0m[38;2;166;226;46m7[0m
[38;2;249;38;114mLABEL [0m[38;2;248;248;242mcom.example.version=[0m[38;2;230;219;116m"[0m[38;2;230;219;116m0.2.1-beta[0m[38;2;230;219;116m"[0m
[38;2;249;38;114mARG [0m[38;2;248;248;242marchitecture[0m

[38;2;249;38;114mENV [0m[38;2;248;248;242mINTERESTING_PATH /usr/bin/interesting-software[0m


[38;2;249;38;114mCOPY[0m[38;2;248;248;242m entrypoint.sh /usr/bin/entrypoint.sh[0m

[38;2;249;38;114mRUN [0m[38;2;248;248;242mif [ [0m[38;2;230;219;116m"[0m[38;2;230;219;116m$architecture[0m[38;2;230;219;116m"[0m[38;2;248;248;242m = [0m[38;2;230;219;116m"[0m[38;2;230;219;116mi386[0m[38;2;230;219;116m"[0m[38;2;248;248;242m ]; then echo [0m[38;2;230;219;116m"[0m[38;2;230;219;116mBuilding i386 docker image[0m[38;2;230;219;116m"[0m[38;2;248;248;242m && \[0m
[38;2;248;248;242m    linux32 yum update -y && linux32 yum install -y mysql ; \[0m
[38;2;248;248;242m    else yum update -y && yum install -y mysql[0m

[38;2;249;38;114mEXPOSE [0m[38;2;248;248;242m80/tcp[0m

[38;2;249;38;114mVOLUME [0m[38;2;248;248;242m[/var/lib/mysql/data][0m

[38;2;249;38;114mENTRYPOINT [0m[38;2;248;248;242m[[0m[38;2;230;219;116m"[0m[38;2;230;219;116m/usr/bin/entrypoint.sh[0m[38;2;230;219;116m"[0m[38;2;248;248;242m][0m
