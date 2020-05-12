exe 'lcd ' . g:vim_config_dir

let g:project_vimrc_layout = 0

function s:setup_layout()
    let tabpage = tabpagenr()
    " Setup notes tab
    tabnew
    T vimrc notes
    edit NOTES.md
    " Setup git tab
    tabnew
    T vimrc git
    Gstatus
    only
    "back to main tab
    exe tabpage . 'tabnext'
    " open file finder
    Denite -start-filter file/rec/git
    let g:project_vimrc_layout = 1
endfun

if !g:project_vimrc_layout 
    call <SID>setup_layout()
endif
