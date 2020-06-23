export PROJECTS_DIR="$DOTFILES_DIR/projects"

function LoadProject {
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

function Project {
    local cmd project
    cmd="ls -1 "$PROJECTS_DIR""
    project=$(eval "$cmd" | FZF_DEFAULT_OPTS="--height 40% --reverse $FZF_DEFAULT_OPTS" fzf +m) && LoadProject $project
}

function AddProject {
    local project_name=$1
    local project_root=`realpath $2`

    mkdir "$PROJECTS_DIR/$project_name"
    echo -e "export PROJECT_ROOT=\"$project_root\"\n\ncd \$PROJECT_ROOT" > "$PROJECTS_DIR/$project_name/project_env.sh"
    echo -e "let g:project_root='$project_root'\n\nexe 'lcd '.g:project_root" > "$PROJECTS_DIR/$project_name/project_init.vim"
}

# CTRL+P : open project with nvim
bind '"\C-p": "\eccProject\C-m"'
