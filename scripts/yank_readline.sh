#!/usr/bin/env bash
# Utilities for integrating readline and the X clipboards.

# Verify that `anyclip` is available, and that bash is at least version 4.
if command -v anyclip > /dev/null 2>&1 && [ ${BASH_VERSINFO:-0} -ge 4 ]; then

    function _cli_discard()
    {
        echo -n "${READLINE_LINE:0:$READLINE_POINT}" | anyclip
        READLINE_LINE="${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=0
    }

    function _cli_kill()
    {
        echo -n "${READLINE_LINE:$READLINE_POINT}" | anyclip
	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}"
    }

    function _cli_yank()
    {
        clip=$(anypaste)
	count=$(echo -n "$clip" | wc -c)
	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}"
	READLINE_LINE+="${clip}${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=$(($READLINE_POINT + $count))
    }

    # Add keybindings M-u/M-k/M-y to discard/kill/yank to/from the clipboard
    # through readline.
    if [ -z "$INSIDE_EMACS" ]; then
	bind -m emacs -x '"\eu": _cli_discard'
	bind -m emacs -x '"\ek": _cli_kill'
	bind -m emacs -x '"\ey": _cli_yank'
    fi

fi


if command -v anyclip > /dev/null 2>&1 && [ -n "${ZSH_VERSION}" ]; then

    anyclip-copy-region-as-kill()
    {
        zle copy-region-as-kill
        print -rn $CUTBUFFER | anyclip
    }
    anyclip-kill-region()
    {
        zle kill-region
        print -rn $CUTBUFFER | anyclip
    }
    anyclip-kill-line()
    {
        zle kill-line
        print -rn $CUTBUFFER | anyclip
    }
    anyclip-yank()
    {
        CUTBUFFER=$(anypaste)
        zle yank
    }

    zle -N anyclip-copy-region-as-kill
    zle -N anyclip-kill-region
    zle -N anyclip-kill-line
    zle -N anyclip-yank
    if [ -z "$INSIDE_EMACS" ]; then
        bindkey -e '\ew' anyclip-copy-region-as-kill
        bindkey -e '\eu' anyclip-kill-region
        bindkey -e '\ek' anyclip-kill-line
        bindkey -e '\ey' anyclip-yank
    fi

fi
