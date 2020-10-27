[38;2;253;151;31m=====[0m
[38;2;248;248;242mTitle[0m
[38;2;253;151;31m=====[0m

[38;2;248;248;242mSubtitle[0m
[38;2;253;151;31m--------[0m
[38;2;248;248;242mTitles are underlined (or over-[0m
[38;2;248;248;242mand underlined) with a printing[0m
[38;2;248;248;242mnonalphanumeric 7-bit ASCII[0m
[38;2;248;248;242mcharacter. Recommended choices[0m
[38;2;248;248;242mare "[0m[38;2;248;248;242m``[0m[38;2;248;248;242m= - ` : ' " ~ ^ _ * + # < >[0m[38;2;248;248;242m``[0m[38;2;248;248;242m".[0m
[38;2;248;248;242mThe underline/overline must be at[0m
[38;2;248;248;242mleast as long as the title text.[0m

[38;2;248;248;242mA lone top-level (sub)section[0m
[38;2;248;248;242mis lifted up to be the document's[0m
[38;2;248;248;242m(sub)title.[0m

[38;2;248;248;242mInline syntaxes[0m
[38;2;253;151;31m---------------[0m

[3;38;2;228;46;112m*[0m[3;38;2;228;46;112memphasis[0m[3;38;2;228;46;112m*[0m[38;2;248;248;242m	[0m
[1;38;2;249;38;114m**[0m[1;38;2;249;38;114mstrong emphasis[0m[1;38;2;249;38;114m**[0m
[38;2;248;248;242m`[0m[38;2;248;248;242minterpreted text[0m[38;2;248;248;242m`[0m
[38;2;248;248;242m``[0m[38;2;248;248;242minline literal[0m[38;2;248;248;242m``[0m
[38;2;248;248;242mhttp://docutils.sf.net/[0m

[38;2;248;248;242mBullet lists[0m
[38;2;253;151;31m------------[0m

[38;2;248;248;242m- This is item 1[0m
[38;2;248;248;242m- This is item 2[0m

[38;2;248;248;242m- Bullets are "-", "*" or "+".[0m
[38;2;248;248;242m  Continuing text must be aligned[0m
[38;2;248;248;242m  after the bullet and whitespace.[0m

[38;2;248;248;242mNote that a blank line is required[0m
[38;2;248;248;242mbefore the first item and after the[0m
[38;2;248;248;242mlast, but is optional between items.[0m

[38;2;248;248;242mEnumerated lists[0m
[38;2;253;151;31m----------------[0m
[38;2;248;248;242m3. This is the first item[0m
[38;2;248;248;242m4. This is the second item[0m
[38;2;248;248;242m5. Enumerators are arabic numbers,[0m
[38;2;248;248;242m   single letters, or roman numerals[0m
[38;2;248;248;242m6. List items should be sequentially[0m
[38;2;248;248;242m   numbered, but need not start at 1[0m
[38;2;248;248;242m   (although not all formatters will[0m
[38;2;248;248;242m   honour the first index).[0m
[38;2;248;248;242m#. This item is auto-enumerated[0m

[38;2;248;248;242mDefinition lists[0m
[38;2;253;151;31m----------------[0m

[38;2;248;248;242mwhat[0m
[38;2;248;248;242m  Definition lists associate a term with[0m
[38;2;248;248;242m  a definition.[0m

[38;2;248;248;242mhow[0m
[38;2;248;248;242m  The term is a one-line phrase, and the[0m
[38;2;248;248;242m  definition is one or more paragraphs or[0m
[38;2;248;248;242m  body elements, indented relative to the[0m
[38;2;248;248;242m  term. Blank lines are not allowed[0m
[38;2;248;248;242m  between term and definition.[0m

[38;2;248;248;242mField lists[0m
[38;2;253;151;31m-----------[0m
[38;2;249;38;114m:[0m[38;2;249;38;114mAuthors[0m[38;2;249;38;114m:[0m
[38;2;248;248;242m    Tony J. (Tibs) Ibbs,[0m
[38;2;248;248;242m    David Goodger[0m
[38;2;248;248;242m    (and sundry other good-natured folks)[0m

[38;2;249;38;114m:[0m[38;2;249;38;114mVersion[0m[38;2;249;38;114m:[0m[38;2;248;248;242m 1.0 of 2001/08/08[0m
[38;2;249;38;114m:[0m[38;2;249;38;114mDedication[0m[38;2;249;38;114m:[0m[38;2;248;248;242m To my father.[0m

[38;2;248;248;242mOptions lists[0m
[38;2;253;151;31m-------------[0m
[38;2;248;248;242m-a            command-line option "a"[0m
[38;2;248;248;242m-b file       options can have arguments[0m
[38;2;248;248;242m              and long descriptions[0m
[38;2;248;248;242m--long        options can be long also[0m
[38;2;248;248;242m--input=file  long options can also have[0m
[38;2;248;248;242m              arguments[0m
[38;2;248;248;242m/V            DOS/VMS-style options too[0m

[38;2;248;248;242mLiteral Blocks[0m
[38;2;253;151;31m--------------[0m

[38;2;248;248;242mA paragraph containing only two colons[0m
[38;2;248;248;242mindicates that the following indented[0m
[38;2;248;248;242mor quoted text is a literal block.[0m

[38;2;248;248;242m::[0m

[38;2;248;248;242m  Whitespace, newlines, blank lines, and[0m
[38;2;248;248;242m  all kinds of markup (like *this* or[0m
[38;2;248;248;242m  \this) is preserved by literal blocks.[0m

[38;2;248;248;242m  The paragraph containing only '::'[0m
[38;2;248;248;242m  will be omitted from the result.[0m

[38;2;248;248;242mThe ``[0m[38;2;248;248;242m::[0m[38;2;248;248;242m`` may be tacked onto the very[0m
[38;2;248;248;242mend of any paragraph. The ``[0m[38;2;248;248;242m::[0m[38;2;248;248;242m`` will be[0m
[38;2;248;248;242momitted if it is preceded by whitespace.[0m
[38;2;248;248;242mThe ``[0m[38;2;248;248;242m::[0m[38;2;248;248;242m`` will be converted to a single[0m
[38;2;248;248;242mcolon if preceded by text, like this[0m[38;2;248;248;242m::[0m

[38;2;248;248;242m  It's very convenient to use this form.[0m

[38;2;248;248;242mLiteral blocks end when text returns to[0m
[38;2;248;248;242mthe preceding paragraph's indentation.[0m
[38;2;248;248;242mThis means that something like this[0m
[38;2;248;248;242mis possible[0m[38;2;248;248;242m::[0m

[38;2;248;248;242m      We start here[0m
[38;2;248;248;242m    and continue here[0m
[38;2;248;248;242m  and end here.[0m

[38;2;248;248;242mPer-line quoting can also be used on[0m
[38;2;248;248;242munindented literal blocks[0m[38;2;248;248;242m::[0m

[38;2;248;248;242m> Useful for quotes from email and[0m
[38;2;248;248;242m> for Haskell literate programming.[0m

[38;2;248;248;242mLine blocks[0m
[38;2;253;151;31m-----------[0m

[38;2;248;248;242mA paragraph containing only two colons[0m
[38;2;248;248;242mindicates that the following indented[0m
[38;2;248;248;242mor quoted text is a literal block.[0m

[38;2;248;248;242m::[0m

[38;2;248;248;242m  Whitespace, newlines, blank lines, and[0m
[38;2;248;248;242m  all kinds of markup (like *this* or[0m
[38;2;248;248;242m  \this) is preserved by literal blocks.[0m

[38;2;248;248;242m  The paragraph containing only '::'[0m
[38;2;248;248;242m  will be omitted from the result.[0m

[38;2;248;248;242mThe ``[0m[38;2;248;248;242m::[0m[38;2;248;248;242m`` may be tacked onto the very[0m
[38;2;248;248;242mend of any paragraph. The ``[0m[38;2;248;248;242m::[0m[38;2;248;248;242m`` will be[0m
[38;2;248;248;242momitted if it is preceded by whitespace.[0m
[38;2;248;248;242mThe ``[0m[38;2;248;248;242m::[0m[38;2;248;248;242m`` will be converted to a single[0m
[38;2;248;248;242mcolon if preceded by text, like this[0m[38;2;248;248;242m::[0m

[38;2;248;248;242m  It's very convenient to use this form.[0m

[38;2;248;248;242mLiteral blocks end when text returns to[0m
[38;2;248;248;242mthe preceding paragraph's indentation.[0m
[38;2;248;248;242mThis means that something like this[0m
[38;2;248;248;242mis possible[0m[38;2;248;248;242m::[0m

[38;2;248;248;242m      We start here[0m
[38;2;248;248;242m    and continue here[0m
[38;2;248;248;242m  and end here.[0m

[38;2;248;248;242mPer-line quoting can also be used on[0m
[38;2;248;248;242munindented literal blocks[0m[38;2;248;248;242m::[0m

[38;2;248;248;242m> Useful for quotes from email and[0m
[38;2;248;248;242m> for Haskell literate programming.[0m

[38;2;248;248;242mBlock quotes[0m
[38;2;253;151;31m------------[0m

[38;2;248;248;242mBlock quotes are just:[0m
[38;2;248;248;242m    Indented paragraphs,[0m

[38;2;248;248;242m        and they may nest.[0m

[38;2;248;248;242mDoctest blocks[0m
[38;2;253;151;31m--------------[0m
[38;2;248;248;242mDoctest blocks are interactive[0m
[38;2;248;248;242mPython sessions. They begin with[0m
[38;2;248;248;242m"[0m[38;2;248;248;242m``[0m[38;2;248;248;242m>>>[0m[38;2;248;248;242m``[0m[38;2;248;248;242m" and end with a blank line.[0m

[38;2;248;248;242m>>> print "This is a doctest block."[0m
[38;2;248;248;242mThis is a doctest block.[0m

[38;2;248;248;242mTables[0m
[38;2;253;151;31m------[0m

[38;2;248;248;242mGrid table:[0m

[38;2;248;248;242m+------------+------------+-----------+[0m
[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m Header 1   |[0m[38;2;248;248;242m Header 2   [0m[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m Header 3  |[0m
[38;2;248;248;242m+============+============+===========+[0m
[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m body row 1 |[0m[38;2;248;248;242m column 2   [0m[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m column 3  |[0m
[38;2;248;248;242m+------------+------------+-----------+[0m
[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m body row 2 |[0m[38;2;248;248;242m Cells may span columns.|[0m
[38;2;248;248;242m+------------+------------+-----------+[0m
[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m body row 3 |[0m[38;2;248;248;242m Cells may  [0m[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m - Cells   |[0m
[38;2;248;248;242m+------------+[0m[38;2;248;248;242m span rows. [0m[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m - contain |[0m
[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m body row 4 |[0m[38;2;248;248;242m            [0m[4;38;2;166;226;46m|[0m[4;38;2;166;226;46m - blocks. |[0m
[38;2;248;248;242m+------------+------------+-----------+[0m

[38;2;248;248;242mSimple table:[0m

[38;2;248;248;242m=====  =====  ======[0m
[38;2;248;248;242m   Inputs     Output[0m
[38;2;248;248;242m------------  ------[0m
[38;2;248;248;242m  A      B    A or B[0m
[38;2;248;248;242m=====  =====  ======[0m
[38;2;248;248;242mFalse  False  False[0m
[38;2;248;248;242mTrue   False  True[0m
[38;2;248;248;242mFalse  True   True[0m
[38;2;248;248;242mTrue   True   True[0m
[38;2;248;248;242m=====  =====  ======[0m

[38;2;248;248;242mTransitions[0m
[38;2;253;151;31m-----------[0m

[38;2;248;248;242mA transition marker is a horizontal line[0m
[38;2;248;248;242mof 4 or more repeated punctuation[0m
[38;2;248;248;242mcharacters.[0m

[38;2;253;151;31m------------[0m

[38;2;248;248;242mA transition should not begin or end a[0m
[38;2;248;248;242msection or document, nor should two[0m
[38;2;248;248;242mtransitions be immediately adjacent.[0m

[38;2;248;248;242mFootnotes[0m
[38;2;253;151;31m---------[0m

[38;2;248;248;242mFootnote references, like [0m[38;2;190;132;255m[[0m[38;2;190;132;255m5[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m
[38;2;248;248;242mNote that footnotes may get[0m
[38;2;248;248;242mrearranged, e.g., to the bottom of[0m
[38;2;248;248;242mthe "page".[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255m5[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116mA numerical footnote. Note there's no colon after the ``]``.[0m

[38;2;248;248;242mAutonumbered footnotes are[0m
[38;2;248;248;242mpossible, like using [0m[38;2;190;132;255m[#[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m and [0m[38;2;190;132;255m[#[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255m#[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116mThis is the first one.[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255m#[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116mThis is the second one.[0m

[38;2;248;248;242mThey may be assigned 'autonumber[0m
[38;2;248;248;242mlabels' - for instance,[0m
[38;2;190;132;255m[#[0m[38;2;190;132;255mfourth[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m and [0m[38;2;190;132;255m[#[0m[38;2;190;132;255mthird[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m

[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255m#[0m[38;2;190;132;255mthird[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116ma.k.a. third_[0m

[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255m#[0m[38;2;190;132;255mfourth[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116ma.k.a. fourth_[0m

[38;2;248;248;242mAuto-symbol footnotes are also[0m
[38;2;248;248;242mpossible, like this: [0m[38;2;190;132;255m[[0m[38;2;190;132;255m*[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m and [0m[38;2;190;132;255m[[0m[38;2;190;132;255m*[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255m*[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116mThis is the first one.[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255m*[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116mThis is the second one.[0m

[38;2;248;248;242mCitations[0m
[38;2;253;151;31m---------[0m

[38;2;248;248;242mCitation references, like [0m[38;2;190;132;255m[[0m[38;2;190;132;255mCIT2002[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m
[38;2;248;248;242mNote that citations may get[0m
[38;2;248;248;242mrearranged, e.g., to the bottom of[0m
[38;2;248;248;242mthe "page".[0m

[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255mCIT2002[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116mA citation (as often used in journals).[0m

[38;2;248;248;242mCitation labels contain alphanumerics,[0m
[38;2;248;248;242munderlines, hyphens and fullstops.[0m
[38;2;248;248;242mCase is not significant.[0m

[38;2;248;248;242mGiven a citation like [0m[38;2;190;132;255m[[0m[38;2;190;132;255mthis[0m[38;2;190;132;255m][0m[38;2;248;248;242m_[0m[38;2;248;248;242m, one[0m
[38;2;248;248;242mcan also refer to it like [0m[38;2;230;219;116mthis[0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m

[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;190;132;255m[[0m[38;2;190;132;255mthis[0m[38;2;190;132;255m][0m[38;2;248;248;242m [0m[38;2;230;219;116mhere.[0m

[38;2;248;248;242mHyperlink Targets[0m
[38;2;253;151;31m-----------------[0m

[38;2;248;248;242mExternal hyperlinks, like [0m[38;2;230;219;116mPython[0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;248;248;242m_[0m[38;2;230;219;116mPython[0m[38;2;248;248;242m:[0m[38;2;248;248;242m [0m[4;38;2;166;226;46mhttp://www.python.org/[0m

[38;2;248;248;242mExternal hyperlinks, like `Python[0m
[38;2;248;248;242m<http://www.python.org/>`_.[0m

[38;2;248;248;242mInternal crossreferences, like [0m[38;2;230;219;116mexample[0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;248;248;242m_[0m[38;2;230;219;116mexample[0m[38;2;248;248;242m:[0m

[38;2;248;248;242mThis is an example crossreference target.[0m

[38;2;230;219;116mPython[0m[38;2;248;248;242m_[0m[38;2;248;248;242m is `my favourite[0m
[38;2;248;248;242mprogramming language`[0m[38;2;230;219;116m_[0m[38;2;248;248;242m_[0m[38;2;248;248;242m.[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m [0m[38;2;248;248;242m_[0m[38;2;230;219;116mPython[0m[38;2;248;248;242m:[0m[38;2;248;248;242m [0m[4;38;2;166;226;46mhttp://www.python.org/[0m

[38;2;230;219;116m_[0m[38;2;248;248;242m_[0m[38;2;248;248;242m [0m[38;2;230;219;116mPython[0m[38;2;248;248;242m_[0m

[38;2;248;248;242mTitles are targets, too[0m
[38;2;253;151;31m=======================[0m
[38;2;248;248;242mImplict references, like `Titles are[0m
[38;2;248;248;242mtargets, too`_.[0m

[38;2;248;248;242mDirectives[0m
[38;2;253;151;31m----------[0m

[38;2;248;248;242mFor instance:[0m
[38;2;248;248;242m..[0m[38;2;248;248;242m image[0m[38;2;248;248;242m::[0m[38;2;248;248;242m images/ball1.gif[0m

[38;2;248;248;242mThe [0m[4;38;2;166;226;46m|[0m[4;38;2;166;226;46mbiohazard|[0m[38;2;248;248;242m symbol must be used on containers used to dispose of medical waste.[0m
[38;2;248;248;242m.. |biohazard| image[0m[38;2;248;248;242m::[0m[38;2;248;248;242m biohazard.png[0m

[38;2;248;248;242mComments[0m
[38;2;253;151;31m--------[0m

[38;2;117;113;94m..[0m[38;2;117;113;94m This text will not be shown[0m
[38;2;117;113;94m   (but, for instance, in HTML might be[0m
[38;2;117;113;94m   rendered as an HTML comment)[0m

[38;2;248;248;242mAn "empty comment" does not[0m
[38;2;248;248;242mconsume following blocks.[0m
[38;2;248;248;242m(An empty comment is ".." with[0m
[38;2;248;248;242mblank lines before and after.)[0m
[38;2;117;113;94m..[0m

[38;2;117;113;94m        So this block is not "lost",[0m
[38;2;117;113;94m        despite its indentation.[0m
