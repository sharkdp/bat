[38;2;117;113;94m//[0m[38;2;117;113;94m The hugeparam command identifies by-value parameters that are larger than n bytes.[0m
[38;2;117;113;94m//[0m[38;2;117;113;94m[0m
[38;2;117;113;94m//[0m[38;2;117;113;94m Example:[0m
[38;2;117;113;94m//[0m[38;2;117;113;94m	$ ./hugeparams encoding/xml[0m
[38;2;249;38;114mpackage[0m[38;2;248;248;242m main[0m
[38;2;248;248;242m[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;255;255;255m([0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;230;219;116m"[0m[38;2;230;219;116mflag[0m[38;2;230;219;116m"[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;230;219;116m"[0m[38;2;230;219;116mfmt[0m[38;2;230;219;116m"[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;230;219;116m"[0m[38;2;230;219;116mgo/ast[0m[38;2;230;219;116m"[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;230;219;116m"[0m[38;2;230;219;116mgo/token[0m[38;2;230;219;116m"[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;230;219;116m"[0m[38;2;230;219;116mgo/types[0m[38;2;230;219;116m"[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;230;219;116m"[0m[38;2;230;219;116mlog[0m[38;2;230;219;116m"[0m[38;2;248;248;242m[0m
[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;230;219;116m"[0m[38;2;230;219;116mgolang.org/x/tools/go/loader[0m[38;2;230;219;116m"[0m[38;2;248;248;242m[0m
[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m[0m
[38;2;117;113;94m//[0m[38;2;117;113;94m!+[0m
[3;38;2;102;217;239mvar[0m[38;2;248;248;242m [0m[38;2;255;255;255mbytesFlag[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m flag[0m[38;2;248;248;242m.[0m[38;2;248;248;242mInt[0m[38;2;255;255;255m([0m[38;2;230;219;116m"[0m[38;2;230;219;116mbytes[0m[38;2;230;219;116m"[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255m48[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;230;219;116m"[0m[38;2;230;219;116mmaximum parameter size in bytes[0m[38;2;230;219;116m"[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m[38;2;248;248;242m[0m
[38;2;248;248;242m[0m
[3;38;2;102;217;239mvar[0m[38;2;248;248;242m [0m[38;2;255;255;255msizeof[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;255;255;255m([0m[38;2;249;38;114m&[0m[38;2;248;248;242mtypes[0m[38;2;248;248;242m.[0m[38;2;255;255;255mStdSizes[0m[38;2;248;248;242m{[0m[38;2;190;132;255m8[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255m8[0m[38;2;248;248;242m}[0m[38;2;255;255;255m)[0m[38;2;248;248;242m.[0m[38;2;255;255;255mSizeof[0m[38;2;248;248;242m [0m[38;2;117;113;94m//[0m[38;2;117;113;94m the sizeof function[0m
[38;2;248;248;242m[0m
[3;38;2;102;217;239mfunc[0m[38;2;248;248;242m [0m[38;2;166;226;46mPrintHugeParams[0m[38;2;255;255;255m([0m[3;38;2;253;151;31mfset[0m[38;2;248;248;242m [0m[38;2;249;38;114m*[0m[38;2;248;248;242mtoken.FileSet[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[3;38;2;253;151;31minfo[0m[38;2;248;248;242m [0m[38;2;249;38;114m*[0m[38;2;248;248;242mtypes.Info[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[3;38;2;253;151;31mfiles[0m[38;2;248;248;242m [][0m[38;2;249;38;114m*[0m[38;2;248;248;242mast.File[0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;255;255;255mcheckTuple[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mfunc[0m[38;2;255;255;255m([0m[3;38;2;253;151;31mdescr[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mstring[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[3;38;2;253;151;31mtuple[0m[38;2;248;248;242m [0m[38;2;249;38;114m*[0m[38;2;248;248;242mtypes.Tuple[0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		[0m[38;2;249;38;114mfor[0m[38;2;248;248;242m [0m[38;2;255;255;255mi[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m [0m[38;2;190;132;255m0[0m[38;2;248;248;242m; i [0m[38;2;249;38;114m<[0m[38;2;248;248;242m tuple[0m[38;2;248;248;242m.[0m[38;2;248;248;242mLen[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m; i[0m[38;2;249;38;114m+[0m[38;2;249;38;114m+[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m			[0m[38;2;255;255;255mv[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m tuple[0m[38;2;248;248;242m.[0m[38;2;248;248;242mAt[0m[38;2;255;255;255m([0m[38;2;248;248;242mi[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m			[0m[38;2;249;38;114mif[0m[38;2;248;248;242m [0m[38;2;255;255;255msz[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m [0m[38;2;248;248;242msizeof[0m[38;2;255;255;255m([0m[38;2;248;248;242mv[0m[38;2;248;248;242m.[0m[38;2;248;248;242mType[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;255;255;255m)[0m[38;2;248;248;242m; sz [0m[38;2;249;38;114m>[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mint64[0m[38;2;255;255;255m([0m[38;2;249;38;114m*[0m[38;2;248;248;242mbytesFlag[0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m				fmt[0m[38;2;248;248;242m.[0m[38;2;248;248;242mPrintf[0m[38;2;255;255;255m([0m[38;2;230;219;116m"[0m[38;2;190;132;255m%s[0m[38;2;230;219;116m: [0m[38;2;190;132;255m%q[0m[38;2;230;219;116m [0m[38;2;190;132;255m%s[0m[38;2;230;219;116m: [0m[38;2;190;132;255m%s[0m[38;2;230;219;116m = [0m[38;2;190;132;255m%d[0m[38;2;230;219;116m bytes[0m[38;2;190;132;255m\n[0m[38;2;230;219;116m"[0m[38;2;248;248;242m,[0m[38;2;248;248;242m[0m
[38;2;248;248;242m					fset[0m[38;2;248;248;242m.[0m[38;2;248;248;242mPosition[0m[38;2;255;255;255m([0m[38;2;248;248;242mv[0m[38;2;248;248;242m.[0m[38;2;248;248;242mPos[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;255;255;255m)[0m[38;2;248;248;242m,[0m[38;2;248;248;242m[0m
[38;2;248;248;242m					v[0m[38;2;248;248;242m.[0m[38;2;248;248;242mName[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m,[0m[38;2;248;248;242m descr[0m[38;2;248;248;242m,[0m[38;2;248;248;242m v[0m[38;2;248;248;242m.[0m[38;2;248;248;242mType[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m,[0m[38;2;248;248;242m sz[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m			[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;255;255;255mcheckSig[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mfunc[0m[38;2;255;255;255m([0m[3;38;2;253;151;31msig[0m[38;2;248;248;242m [0m[38;2;249;38;114m*[0m[38;2;248;248;242mtypes.Signature[0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		[0m[38;2;248;248;242mcheckTuple[0m[38;2;255;255;255m([0m[38;2;230;219;116m"[0m[38;2;230;219;116mparameter[0m[38;2;230;219;116m"[0m[38;2;248;248;242m,[0m[38;2;248;248;242m sig[0m[38;2;248;248;242m.[0m[38;2;248;248;242mParams[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		[0m[38;2;248;248;242mcheckTuple[0m[38;2;255;255;255m([0m[38;2;230;219;116m"[0m[38;2;230;219;116mresult[0m[38;2;230;219;116m"[0m[38;2;248;248;242m,[0m[38;2;248;248;242m sig[0m[38;2;248;248;242m.[0m[38;2;248;248;242mResults[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;249;38;114mfor[0m[38;2;248;248;242m [0m[38;2;255;255;255m_[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;255;255;255mfile[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m [0m[38;2;249;38;114mrange[0m[38;2;248;248;242m files [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		ast[0m[38;2;248;248;242m.[0m[38;2;248;248;242mInspect[0m[38;2;255;255;255m([0m[38;2;248;248;242mfile[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mfunc[0m[38;2;255;255;255m([0m[38;2;248;248;242mn ast[0m[38;2;248;248;242m.[0m[38;2;255;255;255mNode[0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mbool[0m[38;2;248;248;242m {[0m
[38;2;248;248;242m			[0m[38;2;249;38;114mswitch[0m[38;2;248;248;242m n [0m[38;2;248;248;242m:[0m[38;2;249;38;114m=[0m[38;2;248;248;242m n.[0m[38;2;255;255;255m([0m[3;38;2;102;217;239mtype[0m[38;2;255;255;255m)[0m[38;2;248;248;242m {[0m
[38;2;248;248;242m			[0m[38;2;249;38;114mcase[0m[38;2;248;248;242m [0m[38;2;249;38;114m*[0m[38;2;248;248;242mast[0m[38;2;248;248;242m.[0m[38;2;255;255;255mFuncDecl[0m[38;2;248;248;242m:[0m[38;2;248;248;242m[0m
[38;2;248;248;242m				[0m[38;2;248;248;242mcheckSig[0m[38;2;255;255;255m([0m[38;2;248;248;242minfo[0m[38;2;248;248;242m.[0m[38;2;255;255;255mDefs[0m[38;2;248;248;242m[[0m[38;2;248;248;242mn[0m[38;2;248;248;242m.[0m[38;2;255;255;255mName[0m[38;2;248;248;242m][0m[38;2;248;248;242m.[0m[38;2;248;248;242mType[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m.[0m[38;2;255;255;255m([0m[38;2;249;38;114m*[0m[38;2;248;248;242mtypes[0m[38;2;248;248;242m.[0m[38;2;255;255;255mSignature[0m[38;2;255;255;255m)[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m			[0m[38;2;249;38;114mcase[0m[38;2;248;248;242m [0m[38;2;249;38;114m*[0m[38;2;248;248;242mast[0m[38;2;248;248;242m.[0m[38;2;255;255;255mFuncLit[0m[38;2;248;248;242m:[0m[38;2;248;248;242m[0m
[38;2;248;248;242m				[0m[38;2;248;248;242mcheckSig[0m[38;2;255;255;255m([0m[38;2;248;248;242minfo[0m[38;2;248;248;242m.[0m[38;2;255;255;255mTypes[0m[38;2;248;248;242m[[0m[38;2;248;248;242mn[0m[38;2;248;248;242m.[0m[38;2;255;255;255mType[0m[38;2;248;248;242m][0m[38;2;248;248;242m.[0m[38;2;255;255;255mType[0m[38;2;248;248;242m.[0m[38;2;255;255;255m([0m[38;2;249;38;114m*[0m[38;2;248;248;242mtypes[0m[38;2;248;248;242m.[0m[38;2;255;255;255mSignature[0m[38;2;255;255;255m)[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m			}[0m
[38;2;248;248;242m			[0m[38;2;249;38;114mreturn[0m[38;2;248;248;242m [0m[38;2;190;132;255mtrue[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		}[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m[0m
[38;2;117;113;94m//[0m[38;2;117;113;94m!-[0m
[38;2;248;248;242m[0m
[3;38;2;102;217;239mfunc[0m[38;2;248;248;242m [0m[38;2;166;226;46mmain[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	flag[0m[38;2;248;248;242m.[0m[38;2;248;248;242mParse[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;117;113;94m//[0m[38;2;117;113;94m The loader loads a complete Go program from source code.[0m
[38;2;248;248;242m	[0m[3;38;2;102;217;239mvar[0m[38;2;248;248;242m [0m[38;2;255;255;255mconf[0m[38;2;248;248;242m loader[0m[38;2;248;248;242m.[0m[38;2;255;255;255mConfig[0m[38;2;248;248;242m[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;255;255;255m_[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;255;255;255merr[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m conf[0m[38;2;248;248;242m.[0m[38;2;248;248;242mFromArgs[0m[38;2;255;255;255m([0m[38;2;248;248;242mflag[0m[38;2;248;248;242m.[0m[38;2;248;248;242mArgs[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;190;132;255mfalse[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;249;38;114mif[0m[38;2;248;248;242m err [0m[38;2;249;38;114m!=[0m[38;2;248;248;242m [0m[38;2;190;132;255mnil[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		log[0m[38;2;248;248;242m.[0m[38;2;248;248;242mFatal[0m[38;2;255;255;255m([0m[38;2;248;248;242merr[0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;117;113;94m//[0m[38;2;117;113;94m command syntax error[0m
[38;2;248;248;242m	[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;255;255;255mlprog[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;255;255;255merr[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m conf[0m[38;2;248;248;242m.[0m[38;2;248;248;242mLoad[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;249;38;114mif[0m[38;2;248;248;242m err [0m[38;2;249;38;114m!=[0m[38;2;248;248;242m [0m[38;2;190;132;255mnil[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		log[0m[38;2;248;248;242m.[0m[38;2;248;248;242mFatal[0m[38;2;255;255;255m([0m[38;2;248;248;242merr[0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;117;113;94m//[0m[38;2;117;113;94m load error[0m
[38;2;248;248;242m	[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;249;38;114mfor[0m[38;2;248;248;242m [0m[38;2;255;255;255m_[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;255;255;255minfo[0m[38;2;248;248;242m [0m[38;2;249;38;114m:=[0m[38;2;248;248;242m [0m[38;2;249;38;114mrange[0m[38;2;248;248;242m lprog[0m[38;2;248;248;242m.[0m[38;2;248;248;242mInitialPackages[0m[38;2;255;255;255m([0m[38;2;255;255;255m)[0m[38;2;248;248;242m [0m[38;2;248;248;242m{[0m[38;2;248;248;242m[0m
[38;2;248;248;242m		[0m[38;2;248;248;242mPrintHugeParams[0m[38;2;255;255;255m([0m[38;2;248;248;242mlprog[0m[38;2;248;248;242m.[0m[38;2;255;255;255mFset[0m[38;2;248;248;242m,[0m[38;2;248;248;242m [0m[38;2;249;38;114m&[0m[38;2;248;248;242minfo[0m[38;2;248;248;242m.[0m[38;2;255;255;255mInfo[0m[38;2;248;248;242m,[0m[38;2;248;248;242m info[0m[38;2;248;248;242m.[0m[38;2;255;255;255mFiles[0m[38;2;255;255;255m)[0m[38;2;248;248;242m[0m
[38;2;248;248;242m	[0m[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m}[0m[38;2;248;248;242m[0m
[38;2;248;248;242m[0m
[38;2;117;113;94m/*[0m[38;2;117;113;94m[0m
[38;2;117;113;94m//!+output[0m
[38;2;117;113;94m% ./hugeparam encoding/xml[0m
[38;2;117;113;94m/go/src/encoding/xml/marshal.go:167:50: "start" parameter: encoding/xml.StartElement = 56 bytes[0m
[38;2;117;113;94m/go/src/encoding/xml/marshal.go:734:97: "" result: encoding/xml.StartElement = 56 bytes[0m
[38;2;117;113;94m/go/src/encoding/xml/marshal.go:761:51: "start" parameter: encoding/xml.StartElement = 56 bytes[0m
[38;2;117;113;94m/go/src/encoding/xml/marshal.go:781:68: "start" parameter: encoding/xml.StartElement = 56 bytes[0m
[38;2;117;113;94m/go/src/encoding/xml/xml.go:72:30: "" result: encoding/xml.StartElement = 56 bytes[0m
[38;2;117;113;94m//!-output[0m
[38;2;117;113;94m*/[0m[38;2;248;248;242m[0m
