# general
export LC_ALL='ja_JP.UTF-8'

# ruby setting
eval "$(rbenv init -)"
alias s='source ~/.zshrc'
alias vi='vim'
alias la='ls -a'
PROMPT='%n@%m %c> '
RPROMPT='[%~]'

# ls color
autoload -U compinit
compinit

export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

alias ls="ls -GF"
alias gls="gls --color"

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

# history
export HISTFILE=${HOME}/.zsh_history
export HISTSIZE=10000
export SAVEHIST=100000
setopt hist_ignore_dups
setopt EXTENDED_HISTORY
setopt share_history

# tmux fix
alias subl='reattach-to-user-namespace subl'
alias stree='reattach-to-user-namespace stree'
alias open='reattach-to-user-namespace open'

# pyenv
export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:$PATH"
eval "$(pyenv init -)"

# postgresql@9.5
export PATH="/usr/local/opt/postgresql@9.5/bin:$PATH"
