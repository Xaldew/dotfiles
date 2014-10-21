#!/bin/bash
# Some utilities to make tmux easier to handle.

# Create a new tmux-session that is saved upon exit.
function tmux-session()
{

    # Set defaults.
    session_name="tmux_default"
    session_file="$HOME/.tmux_session"

    # Apply positional arguments.
    if [ $# -eq 1 ]; then
	session_name="$1"
    elif [ $# -eq 2 ]; then
	session_file="$2"
    fi

    # Resume or start session.
    tmux has-session -t $session_name &> /dev/null
    if [ $? != 0 ]; then
	tmux new-session -d -s $session_name
	tmux send-keys -t $session_name $session_file C-m
    fi

    tmux attach -t $session_name
}
