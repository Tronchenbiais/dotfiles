export PROJECTS_DIR="$DOTFILES_DIR/projects"

function __project_load {
    local project
    project=$1
    if [ -f "$PROJECTS_DIR/$project/project_init.vim" ]
    then
        alias ide="$VIM_CLONE -c 'Project $project'"
    fi
    if [ -f "$PROJECTS_DIR/$project/project_env.sh" ]
    then
        source "$PROJECTS_DIR/$project/project_env.sh"
    fi
    export PROJECT_NAME=$project
    export PROMPT_PROJECT="[$PROJECT_NAME]"
    if [ $PROJECT_DIR ]
    then
        export PROMPT_BASE_DIR=$PROJECT_DIR
    fi
    clear
}


function __project_select {
    local cmd project
    cmd="ls -1 "$PROJECTS_DIR""
    project=$(eval "$cmd" | FZF_DEFAULT_OPTS="--height 40% --reverse $FZF_DEFAULT_OPTS" fzf +m)
    echo $project
}

function ProjectOpen {
    PROJECT_NAME=$1 bash
}

function __project_open {
    ProjectOpen $(__project_select)
}

function ProjectEdit {
    local project=$1
    PROJECT_NAME=$project $VIM_CLONE -c "Project $project"
}

function __project_edit {
    ProjectEdit $(__project_select)
}

function ProjectCreate {
    local project_name=$1
    if [ $2 ]
    then 
        local project_root=`realpath $2`
    else
        local project_root=`realpath $pwd`
    fi

    mkdir "$PROJECTS_DIR/$project_name"
    echo -e "export PROJECT_ROOT=\"$project_root\"\n\ncd \$PROJECT_ROOT" > "$PROJECTS_DIR/$project_name/project_env.sh"
    echo -e "let g:project_root='$project_root'\n\nexe 'lcd '.g:project_root" > "$PROJECTS_DIR/$project_name/project_init.vim"
}

# CTRL+P : open project with nvim
bind '"\C-p": "\ecc__project_open\C-m"'
bind '"\C-o": "\ecc__project_edit\C-m"'

if [ $PROJECT_NAME ]
then
    __project_load $PROJECT_NAME
fi
