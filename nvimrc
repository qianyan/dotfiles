
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
"tnoremap <C-h> <C-\><C-n><C-w>h
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
nnoremap <silent><leader>" viw<esc>a"<esc>hbi"<esc>lel nnoremap
nnoremap <silent><leader>' viw<esc>a'<esc>hbi'<esc>lel
nnoremap <leader>t :MakeGreen %<CR>
" }}

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
set shiftwidth=4   " autoindent length
set tabstop=4    " define a tab length with space's numbers
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

set nocompatible              " be iMproved, required
filetype off                  " required
" {{ set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'rizzatti/dash.vim'
Plugin 'mattn/emmet-vim'
Plugin 'othree/html5.vim'
Plugin 'kana/vim-fakeclip'
Plugin 'paredit.vim'
Plugin 'wycats/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'tomasr/molokai'
Plugin 'mileszs/ack.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'klen/python-mode'
Plugin 'myhere/vim-nodejs-complete'
Plugin 'majutsushi/tagbar'
Plugin 'tpope/vim-surround'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'altercation/vim-colors-solarized'
Plugin 'freeo/vim-kalisi'
Plugin 'lambdalisue/nodeunit.vim'
Plugin 'reinh/vim-makegreen'
Plugin 'airblade/vim-gitgutter'
Plugin 'easymotion/vim-easymotion'
Plugin 'tomlion/vim-solidity'
Plugin 'scrooloose/syntastic'
call vundle#end()
filetype plugin indent on

" syntastic
"
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_solidity_checkers = ['solc', 'solium']

"colorscheme tomorrow
syntax enable
set updatetime=750
set background=dark
colorscheme molokai

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
