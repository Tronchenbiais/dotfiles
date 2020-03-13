PROJECTS_DIR="$DOTFILES_DIR/vim/projects"

fzf-projects-widget() {
    local cmd project
    cmd="ls -1 "$PROJECTS_DIR""
    project=$(eval "$cmd" | FZF_DEFAULT_OPTS="--height 40% --reverse $FZF_DEFAULT_OPTS" fzf +m) && nvim +"Project $project"
}

# CTRL+P : open project with nvim
bind -x '"\C-p": "fzf-projects-widget"'
