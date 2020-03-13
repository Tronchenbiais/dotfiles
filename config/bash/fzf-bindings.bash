# Key bindings
# ------------
__fzf_select__() {
  local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | cut -b3-"}"
  eval "$cmd" | FZF_DEFAULT_OPTS="--height 40% --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" fzf -m "$@" | while read -r item; do
    printf '%q ' "$item"
  done
  echo
}

if [[ $- =~ i ]]; then

fzf-file-widget() {
    local selected="$(__fzf_select__)"
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
}

__fzf_cd__() {
  local cmd dir
  cmd="${FZF_ALT_C_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type d -print 2> /dev/null | cut -b3-"}"
  dir=$(eval "$cmd" | FZF_DEFAULT_OPTS="--height 40% --reverse $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS" fzf +m) && printf 'cd %q' "$dir"
}

__fzf_history__() (
  local line
  shopt -u nocaseglob nocasematch
  line=$(
    HISTTIMEFORMAT= builtin history |
    FZF_DEFAULT_OPTS="--height 40% $FZF_DEFAULT_OPTS --tac --sync -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m" fzf |
    command grep '^ *[0-9]') &&
    if [[ $- =~ H ]]; then
      sed 's/^ *\([0-9]*\)\** .*/!\1/' <<< "$line"
    else
      sed 's/^ *\([0-9]*\)\** *//' <<< "$line"
    fi
)

# We'd usually use "\e" to enter vi-movement-mode so we can do our magic,
# but this incurs a very noticeable delay of a half second or so,
# because many other commands start with "\e".
# Instead, we bind an unused key, "\C-x\C-a",
# to also enter vi-movement-mode,
# and then use that thereafter.
# (We imagine that "\C-x\C-a" is relatively unlikely to be in use.)
bind '"\C-x\C-a": vi-movement-mode'

bind '"\C-x\C-e": shell-expand-line'
bind '"\C-x\C-r": redraw-current-line'
bind '"\C-x^": history-expand-line'

# CTRL-T - Paste the selected file path into the command line
# - FIXME: Selected items are attached to the end regardless of cursor position
if [ $BASH_VERSINFO -gt 3 ]; then
  bind -x '"\C-t": "fzf-file-widget"'
else
  bind '"\C-t": "\C-x\C-a$a \C-x\C-addi`__fzf_select__`\C-x\C-e\C-x\C-a0Px$a \C-x\C-r\C-x\C-axa "'
fi
bind -m vi-command '"\C-t": "i\C-t"'

# CTRL-R - Paste the selected command from history into the command line
bind '"\C-r": "\C-x\C-addi`__fzf_history__`\C-x\C-e\C-x\C-r\C-x^\C-x\C-a$a"'
bind -m vi-command '"\C-r": "i\C-r"'

# CTRL-F - Change directory
bind '"\C-f": "\C-x\C-addi`__fzf_cd__`\C-x\C-e\C-x\C-r\C-m"'
bind -m vi-command '"\C-f": "ddi`__fzf_cd__`\C-x\C-e\C-x\C-r\C-m"'
fi
