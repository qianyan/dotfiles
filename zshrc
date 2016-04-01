# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="fino"
#ZSH_THEME="arrow"
#ZSH_THEME="awesomepanda"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias glw="./gradlew"
alias sr='source ~/.zshrc'
alias run='./tools/ant/bin/ant'
alias vi='nvim'
alias tm='tmux -2'
alias -s md='subl'
alias gon="cat .git/config | awk '/url/{print \$3}' | xargs open"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git mercurial svn z osx lein colored-man zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh
source ~/.svn_support.sh
# User configuration

export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin"
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mv<F37><F37><F37>im'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
export JBAKE_HOME=/Users/qianyan/bin/jbake-2.5.0-SNAPSHOT
export POSTGRE_HOME=/Applications/Postgres.app/Contents/Versions/9.3

export PATH="$PATH:$HOME/.rvm/bin:$JBAKE_HOME/bin:$POSTGRE_HOME/bin" 
# Add RVM to PATH for scripting
source $HOME/.rvm/scripts/rvm
export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home
PATH=/Users/qianyan/.rvm/gems/ruby-1.9.3-p547/bin:/Users/qianyan/.rvm/gems/ruby-1.9.3-p547@global/bin:/Users/qianyan/.rvm/rubies/ruby-1.9.3-p547/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/qianyan/.rvm/bin:/Users/qianyan/bin/jbake-2.5.0-SNAPSHOT/bin:/Applications/Postgres.app/Contents/Versions/9.3/bin:~/bin

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
