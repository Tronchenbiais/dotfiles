let g:project_root='/home/tronchy/code/web/afdl'
call setenv("BASH_ENV", "/home/tronchy/dotfiles/projects/afdl/project_env.sh")

exe 'lcd '.g:project_root

let g:did_afdl_layout=0

function s:open_logs()
    T! afdl logs
    terminal tail -f server.log
    nmap <silent><buffer> <LocalLeader>r :silent !AfdlServerRestart<Cr>
endfun

function s:open_git()
    T! afdl git
    Gstatus
    only
endfun

command AfdlOpenLogs call s:open_logs()
command AfdlOpenGit call s:open_git()

function s:do_afdl_layout()
    let tabpage = tabpagenr()
    " Setup log tab
    AfdlOpenLogs
    " Setup git tab
    AfdlOpenGit
    "back to main tab
    exe tabpage . 'tabnext'
    let g:did_afdl_layout = 1
endfunction

if g:did_afdl_layout==0
    call s:do_afdl_layout()
endif

call lsp#register_server({
            \ 'name': 'pyls',
            \ 'cmd': ['/home/tronchy/dotfiles/vim/.venv/bin/pyls'],
            \ 'whitelist': ['python'],
            \ })

let g:zv_file_types =
            \{
            \  'python': 'Python_3,Django',
            \  'html': 'Html,Django',
            \  'css': 'CSS',
            \}

