let s:path = fnamemodify(resolve(expand('<sfile>:p')), ':h')

let &packpath = s:path . '/vim,' . &packpath

exe 'source ' . s:path . '/vim/vimrc'
