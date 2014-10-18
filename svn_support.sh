local svn_info='$(svn_prompt_info)'
local color_svn_info=%{$reset_color%}${svn_info}%{$reset_color%}
if [[ $PROMPT != *${svn_info}* ]]
then
PROMPT=$PROMPT${color_svn_info}
fi

ZSH_THEME_SVN_PROMPT_PREFIX="%{$fg_bold[blue]%}svn:("
ZSH_THEME_SVN_PROMPT_SUFFIX="%{$fg_bold[blue]%})"
ZSH_THEME_SVN_PROMPT_DIRTY="%{$fg[red]%} ✘ %{$reset_color%}"
ZSH_THEME_SVN_PROMPT_CLEAN="%{$FG[040]%} ✔"
