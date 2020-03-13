# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -lah'
alias la='ls -A'
alias vicd='cd $(vifm --choose-dir -)'
alias vihelp='vim -c ":help | only"'
alias vimrc='vim ~/.vimrc'
alias vit='vim -c "Gstatus | only"'
alias ,sdir='pwd | xclip'
alias ,pdir='cd $(xclip -o)'

