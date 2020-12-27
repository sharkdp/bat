if &compatible
  set nocompatible
endif

if has('win32') || has ('win64')
  let $VIMHOME = $HOME . "/vimfiles"
elseif v:false && v:true
  echo "Can't get here"
else
  let $VIMHOME = $HOME . "/.vim"
endif

" show existing tab with 2 spaces width
set tabstop=2
" when indenting with '>', use 2 spaces width
set shiftwidth=2
" always set autoindenting on
set autoindent

autocmd VimEnter * echo "Hello Vim"

" Allow :W and :Wq to save too
command! Wq :wq
command! W :w

augroup vimrc
    autocmd!
    autocmd FileType * echo "New filetype"
augroup END

function! s:echo(what)
  return a:what
endfunction

function! HelloWorld(name)
  let l:function_local = "function_local_var"
  let l:parts = split(l:function_local, "_")
  let l:greeting = "Hello " . a:name
  return s:echo(l:greeting)
endfunction

function! source#Hello()
  return "Hello from namespace"
endfunction

function! EchoFunc(...)
  for s in a:000
    echon ' ' . s
  endfor
endfunction

imap <C-h> <C-r>=HelloWorld("World")<CR>

command! -nargs=? Echo :call EchoFunc(<args>)

" TODO test stuff
let g:global = "global var"
let s:script_var = "script var"
let w:window_var = "window war"
let b:buffer_var = "buffer war"
let t:tab_var = "tab war"
echo v:false

3 + 5

echo "Hello" ==# "Hello2"
echo "Hello" ==? "Hello2"
echo "Hello" == "Hello2"
echo "Hello" is "Hello2"
echo "Hello" isnot "Hello2"
echo "Hello" =~ 'xx*'
echo "Hello" !~ "Hello2"
echo "Hello" !~ "Hello2"

echo "/This/should/not/be/a/regex"

" Error case from issue #1604 (https://github.com/sharkdp/bat/issues/1064)
set runtimepath=~/foo/bar

silent g/Aap/p

let g:dict = {}
let g:dict.item = ['l1', 'l2']

let g:dict2 = {'dict_item': ['l1', 'l2'], 'di2': 'x'}

silent g/regex/
silent v/regex/
silent %s/regex/not_regex/

filetype plugin indent on
syntax enable
