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
alias mw="./mvnw"
alias dk="docker"
alias vi='nvim'
alias tm='tmux -2'
alias fire='python -m SimpleHTTPServer'
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
plugins=(git z zsh-syntax-highlighting emacs zsh-autosuggestions lein)

source $ZSH/oh-my-zsh.sh
# User configuration

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
   export EDITOR='nvim'
# else
#   export EDITOR='mv<F37><F37><F37>im'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

function code {  
    if [[ $# = 0 ]]
    then
        open -a "Visual Studio Code"
    else
        local argPath="$1"
        [[ $1 = /* ]] && argPath="$1" || argPath="$PWD/${1#./}"
        open -a "Visual Studio Code" "$argPath"
    fi
}

# encoding
export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8

# Add RVM to PATH for scripting
source $HOME/.rvm/scripts/rvm
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_161.jdk/Contents/Home
#export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home
PATH=~/bin:$JAVA_HOME/bin:$PATH
export TOMCAT_HOME=/usr/local/Cellar/tomcat@8.0/8.0.39/libexec

## zplug
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi

zplug load

## pure prompt
autoload -U promptinit; promptinit
prompt pure

# tabtab source for jhipster package
# uninstall by removing these lines or running `tabtab uninstall jhipster`
[[ -f /Users/qianyan/.config/yarn/global/node_modules/tabtab/.completions/jhipster.zsh ]] && . /Users/qianyan/.config/yarn/global/node_modules/tabtab/.completions/jhipster.zsh
#export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152
export PATH="/usr/local/opt/mysql-client/bin:$PATH"
