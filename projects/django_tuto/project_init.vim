let g:project_root='/home/tronchy/code/web/django_tuto'
exe 'lcd ' . g:project_root

call lsp#register_server({
            \ 'name': 'pyls',
            \ 'cmd': ['/home/tronchy/.local/bin/pyls'],
            \ 'whitelist': ['python'],
            \ })

