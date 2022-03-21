
" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2008 Dec 17
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" {{ shortcuts
" jk => esc
inoremap jk <ESC>
" ; => :
nnoremap ; :

let mapleader = '\'

nnoremap <silent><leader>ev :vsp $MYVIMRC<CR>

nnoremap <silent><leader>sv :source $MYVIMRC<CR>

" terminal normal mapping
tnoremap jk <C-\><C-n>
tnoremap <esc> <C-\><C-n>
tnoremap <C-w><C-h> <C-\><C-n><C-w>h
"tnoremap <C-j> <C-\><C-n><C-w>j
"tnoremap <C-k> <C-\><C-n><C-w>k
"tnoremap <C-l> <C-\><C-n><C-w>l
" split terminal
cnoremap ts split term://$SHELL \| startinsert
cnoremap tv vsp term://$SHELL \| startinsert
" true color
set termguicolors
" autocomplete menu provided by zsh, then <C-n> <C-p> to scroll forward and
" backword
set wildmenu
set wildmode=full
" insert ["] before and after current text.
nnoremap <silent><leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <silent><leader>' viw<esc>a'<esc>hbi'<esc>lel
nnoremap <leader>t :MakeGreen %<CR>
" }}
" extract args {{
nmap <silent><leader>ea dt(dsb
" }}
" wrap args {{
nmap <silent><leader>wa ysvf;f
" }}
"
"
map <Leader>ri :wa<CR> :CargoBuild<CR>
map <Leader>rc :wa<CR> :CargoRun<CR>
map <Leader>ra :wa<CR> :CargoTestAll<CR>
map <Leader>rb :wa<CR> :CargoUnitTestCurrentFile<CR>
map <Leader>rf :wa<CR> :CargoUnitTestFocused<CR>

" Elixir vimix enable
let g:vimix_map_keys = 1

" trave buffer
nnoremap <silent> [b :bprevious<CR> 
nnoremap <silent> ]b :bnext<CR> 
nnoremap <silent> [B :bfirst<CR> 
nnoremap <silent> ]B :blast<CR>

let g:makegreen_stay_on_file = 1
au BufNewFile,BufRead *.js set filetype=javascript

" show statusline
set laststatus=2          " show status bar
set statusline=%f         " Path to the file
set statusline+=%=        " Switch to the right side
set statusline+=%y        " Filetype of the file
set statusline+=%*        " Filetype of the file
set statusline+=%l        " Current line
set statusline+=/         " Separator
set statusline+=%L        " Total lines

" indent
set shiftwidth=2   " autoindent length
set tabstop=2    " define a tab length with space's numbers
set expandtab      " blackspace instead of tab

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" relative number
set relativenumber

set splitright
set splitbelow
" {{ nerdTree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
map <C-n> :NERDTreeToggle<CR>
" }}

call plug#begin()
Plug 'rizzatti/dash.vim'
Plug 'mattn/emmet-vim'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'kana/vim-fakeclip'
Plug 'paredit.vim'
Plug 'wycats/nerdtree'
Plug 'kien/ctrlp.vim'
Plug 'tomasr/molokai'
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-fugitive'
Plug 'klen/python-mode'
Plug 'myhere/vim-nodejs-complete'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'altercation/vim-colors-solarized'
Plug 'freeo/vim-kalisi'
Plug 'lambdalisue/nodeunit.vim'
Plug 'reinh/vim-makegreen'
Plug 'airblade/vim-gitgutter'
Plug 'easymotion/vim-easymotion'
Plug 'tomlion/vim-solidity'
" for tmux
Plug 'christoomey/vim-tmux-navigator'
Plug 'benmills/vimux'
" for golang
Plug 'fatih/vim-go'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'euclio/vim-markdown-composer'
" for elixir
Plug 'elixir-editors/vim-elixir'
Plug 'slashmili/alchemist.vim'
Plug 'spiegela/vimix'
" for rust
Plug 'qianyan/vimux-cargo'
" powerline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" for theme
Plug 'hzchirs/vim-material'
" for dart
Plug 'dart-lang/dart-vim-plugin'
" for fzf
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
call plug#end()

" python
let g:python_host_prog  = '/usr/local/bin/python'                                  
let g:python3_host_prog = '/usr/local/bin/python3'

syntax enable
set updatetime=750
set background=light
colorscheme vim-material
let g:airline_theme='material'

"colorscheme tomorrow
"colorscheme molokai

let g:gitgutter_realtime = 1
set nobackup		" do not keep a backup file 
set noswapfile
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif
