#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# bat-extras | Copyright (C) 2020 eth-p and contributors | MIT License
#
# Repository: https://github.com/eth-p/bat-extras
# Issues:     https://github.com/eth-p/bat-extras/issues
# -----------------------------------------------------------------------------
printc(){
printf "$(sed "$_PRINTC_PATTERN" <<<"$1")" "${@:2}"
}
printc_init(){
case "$1" in
true)_PRINTC_PATTERN="$_PRINTC_PATTERN_ANSI";;
false)_PRINTC_PATTERN="$_PRINTC_PATTERN_PLAIN";;
"[DEFINE]"){
_PRINTC_PATTERN_ANSI=""
_PRINTC_PATTERN_PLAIN=""
local name
local ansi
while read -r name ansi;do
if [[ -z $name && -z $ansi ]]||[[ ${name:0:1} == "#" ]];then
continue
fi
ansi="${ansi/\\/\\\\}"
_PRINTC_PATTERN_PLAIN="${_PRINTC_PATTERN_PLAIN}s/%{$name}//g;"
_PRINTC_PATTERN_ANSI="${_PRINTC_PATTERN_ANSI}s/%{$name}/$ansi/g;"
done
if [ -t 1 ];then
_PRINTC_PATTERN="$_PRINTC_PATTERN_ANSI"
else
_PRINTC_PATTERN="$_PRINTC_PATTERN_PLAIN"
fi
}
esac
}
print_warning(){
printc "%{YELLOW}[%s warning]%{CLEAR}: $1%{CLEAR}\n" "batgrep" "${@:2}" 1>&2
}
print_error(){
printc "%{RED}[%s error]%{CLEAR}: $1%{CLEAR}\n" "batgrep" "${@:2}" 1>&2
}
printc_init "[DEFINE]" <<END
	CLEAR	\x1B[0m
	RED		\x1B[31m
	GREEN	\x1B[32m
	YELLOW	\x1B[33m
	BLUE	\x1B[34m
	MAGENTA	\x1B[35m
	CYAN	\x1B[36m

	DEFAULT \x1B[39m
	DIM		\x1B[2m
END
is_pager_less(){
[[ "$(pager_name)" == "less" ]]
return $?
}
is_pager_disabled(){
[[ -z "$(pager_name)" ]]
return $?
}
pager_name(){
_detect_pager 1>&2
echo "$_SCRIPT_PAGER_NAME"
}
pager_version(){
_detect_pager 1>&2
echo "$_SCRIPT_PAGER_VERSION"
}
pager_exec(){
if [[ -n $SCRIPT_PAGER_CMD ]];then
"$@"|pager_display
return $?
else
"$@"
return $?
fi
}
pager_display(){
if [[ -n $SCRIPT_PAGER_CMD ]];then
if [[ -n $SCRIPT_PAGER_ARGS ]];then
"${SCRIPT_PAGER_CMD[@]}" "${SCRIPT_PAGER_ARGS[@]}"
return $?
else
"${SCRIPT_PAGER_CMD[@]}"
return $?
fi
else
cat
return $?
fi
}
_detect_pager(){
if [[ $_SCRIPT_PAGER_DETECTED == "true" ]];then return;fi
_SCRIPT_PAGER_DETECTED=true
if [[ -z ${SCRIPT_PAGER_CMD[0]} ]];then
_SCRIPT_PAGER_VERSION=0
_SCRIPT_PAGER_NAME=""
return
fi
local output
local output1
output="$("${SCRIPT_PAGER_CMD[0]}" --version 2>&1)"
output1="$(head -n 1 <<<"$output")"
if [[ $output1 =~ ^less[[:blank:]]([[:digit:]]+) ]];then
_SCRIPT_PAGER_VERSION="${BASH_REMATCH[1]}"
_SCRIPT_PAGER_NAME="less"
else
_SCRIPT_PAGER_VERSION=0
_SCRIPT_PAGER_NAME="$(basename "${SCRIPT_PAGER_CMD[0]}")"
fi
}
_configure_pager(){
SCRIPT_PAGER_CMD=($PAGER)
SCRIPT_PAGER_ARGS=()
if [[ -n ${BAT_PAGER+x} ]];then
SCRIPT_PAGER_CMD=($BAT_PAGER)
SCRIPT_PAGER_ARGS=()
return
fi
if is_pager_less;then
SCRIPT_PAGER_CMD=("${SCRIPT_PAGER_CMD[0]}" -R --quit-if-one-screen)
if [[ "$(pager_version)" -lt 500 ]];then
SCRIPT_PAGER_CMD+=(--no-init)
fi
fi
}
if [[ -t 1 ]];then
_configure_pager
else
SCRIPT_PAGER_CMD=()
SCRIPT_PAGER_ARGS=()
fi
SHIFTOPT_HOOKS=()
setargs(){
_ARGV=("$@")
_ARGV_LAST="$((${#_ARGV[@]}-1))"
_ARGV_INDEX=0
}
shiftopt(){
[[ $_ARGV_INDEX -gt $_ARGV_LAST ]]&&return 1
OPT="${_ARGV[$_ARGV_INDEX]}"
unset OPT_VAL
if [[ $OPT =~ ^--[a-zA-Z0-9_-]+=.* ]];then
OPT_VAL="${OPT#*=}"
OPT="${OPT%%=*}"
fi
((_ARGV_INDEX++))
local hook
for hook in "${SHIFTOPT_HOOKS[@]}";do
if "$hook";then
shiftopt
return $?
fi
done
return 0
}
shiftval(){
if [[ -n ${OPT_VAL+x} ]];then
return 0
fi
if [[ $OPT =~ ^-[[:alpha:]][[:digit:]]{1,}$ ]];then
OPT_VAL="${OPT:2}"
return
fi
OPT_VAL="${_ARGV[$_ARGV_INDEX]}"
((_ARGV_INDEX++))
if [[ $OPT_VAL =~ -.* ]];then
printc "%{RED}%s: '%s' requires a value%{CLEAR}\n" "batgrep" "$ARG"
exit 1
fi
}
setargs "$@"
hook_color(){
SHIFTOPT_HOOKS+=("__shiftopt_hook__color")
__shiftopt_hook__color(){
case "$OPT" in
--no-color)OPT_COLOR=false;;
--color){
case "$OPT_VAL" in
"")OPT_COLOR=true;;
always|true)OPT_COLOR=true;;
never|false)OPT_COLOR=false;;
auto)return 0;;
*)printc "%{RED}%s: '--color' expects value of 'auto', 'always', or 'never'%{CLEAR}\n" "batgrep"
exit 1
esac
};;
*)return 1
esac
printc_init "$OPT_COLOR"
return 0
}
if [[ -z $OPT_COLOR ]];then
if [[ -t 1 ]];then
OPT_COLOR=true
else
OPT_COLOR=false
fi
printc_init "$OPT_COLOR"
fi
}
hook_pager(){
SHIFTOPT_HOOKS+=("__shiftopt_hook__pager")
__shiftopt_hook__pager(){
case "$OPT" in
\
--no-pager)shiftval
SCRIPT_PAGER_CMD=''
;;
--paging){
shiftval
case "$OPT_VAL" in
auto):;;
always):;;
never)SCRIPT_PAGER_CMD='';;
*)printc "%{RED}%s: '--paging' expects value of 'auto', 'always', or 'never'%{CLEAR}\n" "batgrep"
exit 1
esac
};;
\
--pager){
shiftval
{
SCRIPT_PAGER_CMD=($OPT_VAL)
PAGER_ARGS=()
}
};;
*)return 1
esac
}
}
hook_version(){
SHIFTOPT_HOOKS+=("__shiftopt_hook__version")
__shiftopt_hook__version(){
if [[ $OPT == "--version" ]];then
printf "%s %s\n\n%s\n%s\n" \
"batgrep" \
"2020.10.04" \
"Copyright (C) 2019-2020 eth-p | MIT License" \
"https://github.com/eth-p/bat-extras"
exit 0
fi
return 1
}
}
term_width(){
local width="$({ stty size 2>/dev/null||echo "22 80";}|cut -d ' ' -f2)"
if [[ $width -ne 0 ]];then
echo "$width"
else
echo "80"
fi
return 0
}
hook_width(){
SHIFTOPT_HOOKS+=("__shiftopt_hook__width")
__shiftopt_hook__width(){
case "$OPT" in
--terminal-width)shiftval
OPT_TERMINAL_WIDTH="$OPT_VAL"
;;
*)return 1
esac
return 0
}
OPT_TERMINAL_WIDTH="$(term_width)"
}
bat_version(){
"bat" --version|cut -d ' ' -f 2
return
}
version_compare(){
local version="$1"
local compare="$3"
if ! [[ $version =~ \.$ ]];then
version="$version."
fi
if ! [[ $compare =~ \.$ ]];then
compare="$compare."
fi
version_compare__recurse "$version" "$2" "$compare"
return $?
}
version_compare__recurse(){
local version="$1"
local operator="$2"
local compare="$3"
local v_major="${version%%.*}"
local c_major="${compare%%.*}"
local v_minor="${version#*.}"
local c_minor="${compare#*.}"
if [[ -z $v_minor && -z $c_minor ]];then
[ "$v_major" $operator "$c_major" ]
return $?
fi
if [[ -z $v_minor ]];then
v_minor="0."
fi
if [[ -z $c_minor ]];then
c_minor="0."
fi
case "$operator" in
-eq)[[ $v_major -ne $c_major ]]&&return 1;;
-ne)[[ $v_major -ne $c_major ]]&&return 0;;
-ge|-gt)[[ $v_major -lt $c_major ]]&&return 1
[[ $v_major -gt $c_major ]]&&return 0
;;
-le|-lt)[[ $v_major -gt $c_major ]]&&return 1
[[ $v_major -lt $c_major ]]&&return 0
esac
version_compare__recurse "$v_minor" "$operator" "$c_minor"
}
hook_color
hook_pager
hook_version
hook_width
RG_ARGS=()
BAT_ARGS=()
PATTERN=""
FILES=()
OPT_CASE_SENSITIVITY=''
OPT_CONTEXT_BEFORE=2
OPT_CONTEXT_AFTER=2
OPT_FOLLOW=true
OPT_SNIP=""
OPT_HIGHLIGHT=true
OPT_SEARCH_PATTERN=false
OPT_FIXED_STRINGS=false
BAT_STYLE="header,numbers"
if version_compare "$(bat_version)" -gt "0.12";then
OPT_SNIP=",snip"
fi
if [[ -n $RIPGREP_CONFIG_PATH && -e $RIPGREP_CONFIG_PATH ]];then
for arg in $(cat "$RIPGREP_CONFIG_PATH");do
case "$arg" in
--context=*)val="${arg:10}"
OPT_CONTEXT_BEFORE="$val"
OPT_CONTEXT_AFTER="$val"
;;
--before-context=*)val="${arg:17}"
OPT_CONTEXT_BEFORE="$val"
;;
--after-context=*)val="${arg:16}"
OPT_CONTEXT_AFTER="$val"
;;
-C*)val="${arg:2}"
OPT_CONTEXT_BEFORE="$val"
OPT_CONTEXT_AFTER="$val"
;;
-B*)val="${arg:2}"
OPT_CONTEXT_BEFORE="$val"
;;
-A*)val="${arg:2}"
OPT_CONTEXT_AFTER="$val"
esac
done
fi
while shiftopt;do
case "$OPT" in
\
-i|--ignore-case)OPT_CASE_SENSITIVITY="--ignore-case";;
-s|--case-sensitive)OPT_CASE_SENSITIVITY="--case-sensitive";;
-S|--smart-case)OPT_CASE_SENSITIVITY="--smart-case";;
-A*|--after-context)shiftval
OPT_CONTEXT_AFTER="$OPT_VAL"
;;
-B*|--before-context)shiftval
OPT_CONTEXT_BEFORE="$OPT_VAL"
;;
-C*|--context)shiftval
OPT_CONTEXT_BEFORE="$OPT_VAL"
OPT_CONTEXT_AFTER="$OPT_VAL"
;;
-F|--fixed-strings)OPT_FIXED_STRINGS=true
RG_ARGS+=("$OPT")
;;
-U|--multiline|\
-P|--pcre2|\
-z|--search-zip|\
-w|--word-regexp|\
--one-file-system|\
--multiline-dotall|\
--ignore|--no-ignore|\
--crlf|--no-crlf|\
--hidden|--no-hidden)RG_ARGS+=("$OPT")
;;
-E|--encoding|\
-g|--glob|\
-t|--type|\
-T|--type-not|\
-m|--max-count|\
--max-depth|\
--iglob|\
--ignore-file)shiftval
RG_ARGS+=("$OPT" "$OPT_VAL")
;;
\
\
\
--no-follow)OPT_FOLLOW=false;;
--no-snip)OPT_SNIP="";;
--no-highlight)OPT_HIGHLIGHT=false;;
-p|--search-pattern)OPT_SEARCH_PATTERN=true;;
--no-search-pattern)OPT_SEARCH_PATTERN=false;;
\
--rg:*){
if [[ ${OPT:5:1} == "-" ]];then
RG_ARGS+=("${OPT:5}")
else
RG_ARGS+=("--${OPT:5}")
fi
if [[ -n $OPT_VAL ]];then
RG_ARGS+=("$OPT_VAL")
fi
};;
\
-*){
printc "%{RED}%s: unknown option '%s'%{CLEAR}\n" "batgrep" "$OPT" 1>&2
exit 1
};;
\
*){
if [ -z "$PATTERN" ];then
PATTERN="$OPT"
else
FILES+=("$OPT")
fi
}
esac
done
if [[ -z $PATTERN ]];then
print_error "no pattern provided"
exit 1
fi
SEP="$(printc "%{DIM}%${OPT_TERMINAL_WIDTH}s%{CLEAR}"|sed "s/ /─/g")"
if [[ -n $OPT_CASE_SENSITIVITY ]];then
RG_ARGS+=("$OPT_CASE_SENSITIVITY")
fi
if "$OPT_FOLLOW";then
RG_ARGS+=("--follow")
fi
if "$OPT_COLOR";then
BAT_ARGS+=("--color=always")
else
BAT_ARGS+=("--color=never")
fi
if [[ $OPT_CONTEXT_BEFORE -eq 0 && $OPT_CONTEXT_AFTER -eq 0 ]];then
OPT_SNIP=""
OPT_HIGHLIGHT=false
fi
if "$OPT_SEARCH_PATTERN";then
if is_pager_less;then
if "$OPT_FIXED_STRINGS";then
SCRIPT_PAGER_ARGS+=(-p $'\x12'"$PATTERN")
else
SCRIPT_PAGER_ARGS+=(-p "$PATTERN")
fi
elif is_pager_disabled;then
print_error "%s %s %s" \
"The -p/--search-pattern option requires a pager, but" \
'the pager was explicitly disabled by $BAT_PAGER or the' \
"--paging option."
exit 1
else
print_error "Unsupported pager '%s' for option -p/--search-pattern" \
"$(pager_name)"
exit 1
fi
fi
main(){
FOUND_FILES=()
FOUND=0
FIRST_PRINT=true
LAST_LR=()
LAST_LH=()
LAST_FILE=''
do_print(){
[[ -z $LAST_FILE ]]&&return 0
"$FIRST_PRINT"&&echo "$SEP"
FIRST_PRINT=false
"bat" "${BAT_ARGS[@]}" \
"${LAST_LR[@]}" \
"${LAST_LH[@]}" \
--style="$BAT_STYLE$OPT_SNIP" \
--paging=never \
--terminal-width="$OPT_TERMINAL_WIDTH" \
"$LAST_FILE"
echo "$SEP"
}
while IFS=':' read -r file line column text;do
((FOUND++))
if [[ $LAST_FILE != "$file" ]];then
do_print
LAST_FILE="$file"
LAST_LR=()
LAST_LH=()
fi
line_start=$((line-OPT_CONTEXT_BEFORE))
line_end=$((line+OPT_CONTEXT_AFTER))
[[ $line_start -gt 0 ]]||line_start=''
LAST_LR+=("--line-range=$line_start:$line_end")
[[ $OPT_HIGHLIGHT == "true" ]]&&LAST_LH+=("--highlight-line=$line")
done < <(rg --with-filename --vimgrep "${RG_ARGS[@]}" --context=0 --no-context-separator --sort path "$PATTERN" "${FILES[@]}")
do_print
if [[ $FOUND -eq 0 ]];then
exit 2
fi
}
pager_exec main
exit $?
