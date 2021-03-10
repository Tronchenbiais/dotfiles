export PROJECT_ROOT="/home/tronchy/code/web/afdl"

cd $PROJECT_ROOT

export VIRTUAL_ENV_DISABLE_PROMPT=1
source .venv/bin/activate

export PROJECT_SERVER_PID=0
export PROJECT_SERVER_PID_FILE="/home/tronchy/dotfiles/projects/afdl/server_pid"

function AfdlServerStart
{
    nohup python "$PROJECT_ROOT/manage.py" runserver 0.0.0.0:8000 &> "$PROJECT_ROOT/server.log" &
    PROJECT_SERVER_PID=$!
    echo $PROJECT_SERVER_PID > "$PROJECT_SERVER_PID_FILE"
}

function AfdlServerStop
{
    if [ -f $PROJECT_SERVER_PID_FILE ]
    then
        kill `cat "$PROJECT_SERVER_PID_FILE"`
        rm "$PROJECT_SERVER_PID_FILE"
    fi
}

function AfdlServerRestart
{
    AfdlServerStop
    AfdlServerStart
}
