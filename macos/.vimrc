" Set relative line numbers
set number
set relativenumber

" Color syntax
syntax on

" Highlight search
set hlsearch

" Make searches case-insensitive unless uppercase letters are included
set ignorecase
set smartcase

" Enable real-time search
set incsearch

" Expand tabs into spaces
set expandtab

" Set tab and indent size
set tabstop=4
set shiftwidth=4
set softtabstop=4

" Enable line wrapping
set wrap

" Auto indentation
set autoindent

" Allow vim to use system clipboard for yank, delete, change, and put operations
set clipboard=unnamedplus
 
" Map 'jk' to 'Esc'
inoremap jk <Esc>:w<CR>

"Map 'jj' to ':wq'
inoremap jj <Esc>:wq<CR>

"Disable back and swap files
set nobackup
set nowritebackup
set noswapfile
