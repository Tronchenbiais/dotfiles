export PROJECTS_DIR="$DOTFILES_DIR/projects"

function load-project {
    local project
    project=$1
    if [ -f "$PROJECTS_DIR/$project/project_init.vim" ]
    then
        alias ide="nvim -c 'Project $project'"
    fi
    if [ -f "$PROJECTS_DIR/$project/project_env.sh" ]
    then
        source "$PROJECTS_DIR/$project/project_env.sh"
    fi
    export PROJECT_NAME=$project
    PS1="[$project] $BASE_PROMPT"
    export PS1
    clear
}

fzf-projects-widget() {
    local cmd project
    cmd="ls -1 "$PROJECTS_DIR""
    project=$(eval "$cmd" | FZF_DEFAULT_OPTS="--height 40% --reverse $FZF_DEFAULT_OPTS" fzf +m) && load-project $project
}

# CTRL+P : open project with nvim
bind -x '"\C-p": "fzf-projects-widget"'
