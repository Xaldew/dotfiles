#!/usr/bin/env bash
# Utilities for integrating readline and the X clipboards.

# Verify that xclip is available, and that bash is at least version 4.
if command -v xclip > /dev/null 2>&1 && [ $BASH_VERSINFO -ge 4 ]; then

    function _xdiscard()
    {
	echo -n "${READLINE_LINE:0:$READLINE_POINT}" | \
	    xclip -selection clipboard
	READLINE_LINE="${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=0
    }
    function _xkill()
    {
	echo -n "${READLINE_LINE:$READLINE_POINT}" | \
	    xclip -selection clipboard
	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}"
    }
    function _xyank()
    {
	clip=$(xclip -selection clipboard -out)
	count=$(echo -n "$clip" | wc -c)
	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}"
	READLINE_LINE+="${clip}${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=$(($READLINE_POINT + $count))
    }

    # Add keybindings M-k/M-k/M-u to discard/kill/yank to/from the clipboard
    # through readline.
    if [ -z "$INSIDE_EMACS" ]; then
	bind -m emacs -x '"\eu": _xdiscard'
	bind -m emacs -x '"\ek": _xkill'
	bind -m emacs -x '"\ey": _xyank'
    fi
fi
