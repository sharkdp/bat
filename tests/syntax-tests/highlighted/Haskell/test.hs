[38;2;190;132;255m{-# [0m[38;2;249;38;114mLANGUAGE[0m[38;2;190;132;255m OverloadedStrings #-}[0m

[38;2;117;113;94m--[0m[38;2;117;113;94m simple parser for a Lisp-like syntax I wrote some time ago[0m

[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mData.Void[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[3;38;2;102;217;239mVoid[0m[38;2;248;248;242m)[0m

[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mData.Text[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[3;38;2;102;217;239mText[0m[38;2;248;248;242m)[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;249;38;114mqualified[0m[38;2;248;248;242m [0m[38;2;248;248;242mData.Text[0m[38;2;248;248;242m [0m[38;2;249;38;114mas[0m[38;2;248;248;242m [0m[38;2;248;248;242mT[0m

[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mText.Megaparsec.Char[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mText.Megaparsec.Error[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[38;2;166;226;46merrorBundlePretty[0m[38;2;248;248;242m)[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;248;248;242mText.Megaparsec[0m[38;2;248;248;242m [0m[38;2;249;38;114mhiding[0m[38;2;248;248;242m [0m[38;2;248;248;242m([0m[3;38;2;102;217;239mState[0m[38;2;248;248;242m)[0m
[38;2;249;38;114mimport[0m[38;2;248;248;242m [0m[38;2;249;38;114mqualified[0m[38;2;248;248;242m [0m[38;2;248;248;242mText.Megaparsec.Char.Lexer[0m[38;2;248;248;242m [0m[38;2;249;38;114mas[0m[38;2;248;248;242m [0m[38;2;248;248;242mL[0m

[38;2;249;38;114mdata[0m[38;2;248;248;242m [0m[38;2;190;132;255mLispVal[0m
[38;2;248;248;242m  [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mSymbol[0m[38;2;248;248;242m [0m[38;2;190;132;255mText[0m
[38;2;248;248;242m  [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;190;132;255mList[0m[38;2;248;248;242m [[0m[38;2;190;132;255mLispVal[0m[38;2;248;248;242m][0m
[38;2;248;248;242m  [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;190;132;255mNumber[0m[38;2;248;248;242m [0m[38;2;190;132;255mInteger[0m
[38;2;248;248;242m  [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;190;132;255mString[0m[38;2;248;248;242m [0m[38;2;190;132;255mText[0m
[38;2;248;248;242m  [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;190;132;255mLispTrue[0m
[38;2;248;248;242m  [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;190;132;255mLispFalse[0m
[38;2;248;248;242m  [0m[38;2;249;38;114m|[0m[38;2;248;248;242m [0m[38;2;190;132;255mNil[0m
[38;2;248;248;242m  [0m[38;2;249;38;114mderiving[0m[38;2;248;248;242m ([0m[3;4;38;2;166;226;46mShow[0m[38;2;248;248;242m, [0m[3;4;38;2;166;226;46mEq[0m[38;2;248;248;242m)[0m

[38;2;249;38;114mtype[0m[38;2;248;248;242m [0m[38;2;190;132;255mParser[0m[38;2;248;248;242m [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mParsec[0m[38;2;248;248;242m [0m[38;2;190;132;255mVoid[0m[38;2;248;248;242m [0m[38;2;190;132;255mText[0m

[38;2;166;226;46mreadStr[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mText[0m[38;2;248;248;242m [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mEither[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mString[0m[38;2;248;248;242m [[0m[3;38;2;102;217;239mLispVal[0m[38;2;248;248;242m][0m
[38;2;248;248;242mreadStr t [0m[38;2;249;38;114m=[0m
[38;2;248;248;242m  [0m[38;2;249;38;114mcase[0m[38;2;248;248;242m parse pLisp [0m[38;2;230;219;116m"[0m[38;2;230;219;116mf[0m[38;2;230;219;116m"[0m[38;2;248;248;242m t [0m[38;2;249;38;114mof[0m
[38;2;248;248;242m    [0m[38;2;190;132;255mRight[0m[38;2;248;248;242m parsed [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [0m[38;2;190;132;255mRight[0m[38;2;248;248;242m parsed[0m
[38;2;248;248;242m    [0m[38;2;190;132;255mLeft[0m[38;2;248;248;242m err [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [0m[38;2;190;132;255mLeft[0m[38;2;248;248;242m [0m[38;2;249;38;114m$[0m[38;2;248;248;242m errorBundlePretty err[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINABLE[0m[38;2;190;132;255m readStr #-}[0m

[38;2;166;226;46msc[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[38;2;102;217;239m()[0m
[38;2;248;248;242msc [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mL[0m[38;2;249;38;114m.[0m[38;2;248;248;242mspace space1 ([0m[38;2;190;132;255mL[0m[38;2;249;38;114m.[0m[38;2;248;248;242mskipLineComment [0m[38;2;230;219;116m"[0m[38;2;230;219;116m;[0m[38;2;230;219;116m"[0m[38;2;248;248;242m) empty[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINABLE[0m[38;2;190;132;255m sc #-}[0m

[38;2;166;226;46mlexeme[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m[38;2;248;248;242m [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[38;2;255;255;255ma[0m
[38;2;248;248;242mlexeme [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mL[0m[38;2;249;38;114m.[0m[38;2;248;248;242mlexeme sc[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m lexeme #-}[0m

[38;2;166;226;46msymbol[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mText[0m[38;2;248;248;242m [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mText[0m
[38;2;248;248;242msymbol [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mL[0m[38;2;249;38;114m.[0m[38;2;248;248;242msymbol sc[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m symbol #-}[0m

[38;2;166;226;46msymbol'[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mText[0m[38;2;248;248;242m [0m[38;2;249;38;114m->[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mText[0m
[38;2;248;248;242msymbol' [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mL[0m[38;2;249;38;114m.[0m[38;2;248;248;242msymbol' sc[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m symbol' #-}[0m

[38;2;166;226;46mpNil[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mLispVal[0m
[38;2;248;248;242mpNil [0m[38;2;249;38;114m=[0m[38;2;248;248;242m symbol' [0m[38;2;230;219;116m"[0m[38;2;230;219;116mnil[0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m>>[0m[38;2;248;248;242m return [0m[38;2;190;132;255mNil[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m pNil #-}[0m

[38;2;166;226;46minteger[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mInteger[0m
[38;2;248;248;242minteger [0m[38;2;249;38;114m=[0m[38;2;248;248;242m lexeme [0m[38;2;190;132;255mL[0m[38;2;249;38;114m.[0m[38;2;248;248;242mdecimal[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m integer #-}[0m

[38;2;166;226;46mlispSymbols[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mChar[0m
[38;2;248;248;242mlispSymbols [0m[38;2;249;38;114m=[0m[38;2;248;248;242m oneOf ([0m[38;2;230;219;116m"[0m[38;2;230;219;116m#$%&|*+-/:<=>?@[0m[38;2;190;132;255m^_[0m[38;2;230;219;116m~[0m[38;2;230;219;116m"[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[38;2;190;132;255mString[0m[38;2;248;248;242m)[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m lispSymbols #-}[0m

[38;2;166;226;46mpLispVal[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mLispVal[0m
[38;2;248;248;242mpLispVal [0m[38;2;249;38;114m=[0m[38;2;248;248;242m choice [pList[0m[38;2;248;248;242m,[0m[38;2;248;248;242m pNumber[0m[38;2;248;248;242m,[0m[38;2;248;248;242m pSymbol[0m[38;2;248;248;242m,[0m[38;2;248;248;242m pNil[0m[38;2;248;248;242m,[0m[38;2;248;248;242m pString][0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m pLispVal #-}[0m

[38;2;166;226;46mpSymbol[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mLispVal[0m
[38;2;248;248;242mpSymbol [0m[38;2;249;38;114m=[0m[38;2;248;248;242m ([0m[38;2;190;132;255mSymbol[0m[38;2;248;248;242m [0m[38;2;249;38;114m.[0m[38;2;248;248;242m [0m[38;2;190;132;255mT[0m[38;2;249;38;114m.[0m[38;2;248;248;242mpack [0m[38;2;249;38;114m<$>[0m[38;2;248;248;242m lexeme (some (letterChar [0m[38;2;249;38;114m<|>[0m[38;2;248;248;242m lispSymbols)))[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINABLE[0m[38;2;190;132;255m pSymbol #-}[0m

[38;2;166;226;46mpList[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mLispVal[0m
[38;2;248;248;242mpList [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mList[0m[38;2;248;248;242m [0m[38;2;249;38;114m<$>[0m[38;2;248;248;242m between (symbol [0m[38;2;230;219;116m"[0m[38;2;230;219;116m([0m[38;2;230;219;116m"[0m[38;2;248;248;242m) (symbol [0m[38;2;230;219;116m"[0m[38;2;230;219;116m)[0m[38;2;230;219;116m"[0m[38;2;248;248;242m) (many pLispVal)[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINABLE[0m[38;2;190;132;255m pList #-}[0m

[38;2;166;226;46mpLisp[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [[0m[3;38;2;102;217;239mLispVal[0m[38;2;248;248;242m][0m
[38;2;248;248;242mpLisp [0m[38;2;249;38;114m=[0m[38;2;248;248;242m some pLispVal[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m pLisp #-}[0m

[38;2;166;226;46mpNumber[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mLispVal[0m
[38;2;248;248;242mpNumber [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;190;132;255mNumber[0m[38;2;248;248;242m [0m[38;2;249;38;114m<$>[0m[38;2;248;248;242m integer[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINE[0m[38;2;190;132;255m pNumber #-}[0m

[38;2;166;226;46mpString[0m[38;2;248;248;242m [0m[38;2;249;38;114m::[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mParser[0m[38;2;248;248;242m [0m[3;38;2;102;217;239mLispVal[0m
[38;2;248;248;242mpString [0m[38;2;249;38;114m=[0m[38;2;248;248;242m [0m[38;2;249;38;114mdo[0m
[38;2;248;248;242m  str [0m[38;2;249;38;114m<-[0m[38;2;248;248;242m char [0m[38;2;230;219;116m'[0m[38;2;190;132;255m\"[0m[38;2;230;219;116m'[0m[38;2;248;248;242m *[0m[38;2;249;38;114m>[0m[38;2;248;248;242m manyTill [0m[38;2;190;132;255mL[0m[38;2;249;38;114m.[0m[38;2;248;248;242mcharLiteral (char [0m[38;2;230;219;116m'[0m[38;2;190;132;255m\"[0m[38;2;230;219;116m'[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  return [0m[38;2;249;38;114m$[0m[38;2;248;248;242m [0m[38;2;190;132;255mString[0m[38;2;248;248;242m ([0m[38;2;190;132;255mT[0m[38;2;249;38;114m.[0m[38;2;248;248;242mpack str)[0m
[38;2;190;132;255m{-# [0m[38;2;249;38;114mINLINABLE[0m[38;2;190;132;255m pString #-}[0m
