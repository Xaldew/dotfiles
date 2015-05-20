#!/usr/bin/env sh
# Adds useful coloration and svn/git information to the prompt.

. $local_prefix_dir/bin/git-prompt.sh
. $dotfiles_dir/scripts/setup_terminal_colors.sh

# Reset
Color_Off='\[\e[0m\]'       # Text Reset

# Regular Colors
Black='\[\e[0;30m\]'        # Black
Red='\[\e[0;31m\]'          # Red
Green='\[\e[0;32m\]'        # Green
Yellow='\[\e[0;33m\]'       # Yellow
Blue='\[\e[0;34m\]'         # Blue
Purple='\[\e[0;35m\]'       # Purple
Cyan='\[\e[0;36m\]'         # Cyan
White='\[\e[0;37m\]'        # White
Brown='\[\e[0;33m\]'        # Brown

# Bold
BBlack='\[\e[1;30m\]'       # Black
BRed='\[\e[1;31m\]'         # Red
BGreen='\[\e[1;32m\]'       # Green
BYellow='\[\e[1;33m\]'      # Yellow
BBlue='\[\e[1;34m\]'        # Blue
BPurple='\[\e[1;35m\]'      # Purple
BCyan='\[\e[1;36m\]'        # Cyan
BWhite='\[\e[1;37m\]'       # White

# Underline
UBlack='\[\e[4;30m\]'       # Black
URed='\[\e[4;31m\]'         # Red
UGreen='\[\e[4;32m\]'       # Green
UYellow='\[\e[4;33m\]'      # Yellow
UBlue='\[\e[4;34m\]'        # Blue
UPurple='\[\e[4;35m\]'      # Purple
UCyan='\[\e[4;36m\]'        # Cyan
UWhite='\[\e[4;37m\]'       # White

# Background
On_Black='\[\e[40m\]'       # Black
On_Red='\[\e[41m\]'         # Red
On_Green='\[\e[42m\]'       # Green
On_Yellow='\[\e[43m\]'      # Yellow
On_Blue='\[\e[44m\]'        # Blue
On_Purple='\[\e[45m\]'      # Purple
On_Cyan='\[\e[46m\]'        # Cyan
On_White='\[\e[47m\]'       # White

# High Intensity
IBlack='\[\e[0;90m\]'       # Black
IRed='\[\e[0;91m\]'         # Red
IGreen='\[\e[0;92m\]'       # Green
IYellow='\[\e[0;93m\]'      # Yellow
IBlue='\[\e[0;94m\]'        # Blue
IPurple='\[\e[0;95m\]'      # Purple
ICyan='\[\e[0;96m\]'        # Cyan
IWhite='\[\e[0;97m\]'       # White

# Bold High Intensity
BIBlack='\[\e[1;90m\]'      # Black
BIRed='\[\e[1;91m\]'        # Red
BIGreen='\[\e[1;92m\]'      # Green
BIYellow='\[\e[1;93m\]'     # Yellow
BIBlue='\[\e[1;94m\]'       # Blue
BIPurple='\[\e[1;95m\]'     # Purple
BICyan='\[\e[1;96m\]'       # Cyan
BIWhite='\[\e[1;97m\]'      # White

# High Intensity backgrounds
On_IBlack='\[\e[0;100m\]'   # Black
On_IRed='\[\e[0;101m\]'     # Red
On_IGreen='\[\e[0;102m\]'   # Green
On_IYellow='\[\e[0;103m\]'  # Yellow
On_IBlue='\[\e[0;104m\]'    # Blue
On_IPurple='\[\e[0;105m\]'  # Purple
On_ICyan='\[\e[0;106m\]'    # Cyan
On_IWhite='\[\e[0;107m\]'   # White


bzr_prompt_info()
{
    bzr_cb=$(bzr nick 2> /dev/null | \
		    grep -v "ERROR" | \
		    cut -d ":" -f2 | \
		    awk -F / '{print " (bzr::"$1")"}')
    if [ -n "$bzr_cb" ]; then
	bzr_dirty=""
	[[ -n `bzr status` ]] && bzr_dirty="${Red} *"
	printf "%s" "$bzr_cb$bzr_dirty"
    fi
}

git_prompt_info()
{
    git name-rev HEAD 2> /dev/null 1> /dev/null && __git_ps1 " (git::%s)"
}

parse_svn_url()
{
    svn info 2>/dev/null | sed -ne 's#^URL: ##p'
}

parse_svn_repository_root()
{
    svn info 2>/dev/null | sed -ne 's#^Repository Root: ##p'
}

parse_svn_branch()
{
    parse_svn_url |
	sed -e 's#^'"$(parse_svn_repository_root)"'##g' |
	sed "s|^/branches/||" |
	awk '{print $1}'
}

parse_svn_rev()
{
    svn info 2>/dev/null | \
	sed -ne 's#^Revision: ##p' | \
	awk '{print $1}'
}

svn_prompt_info()
{
    branch=$(parse_svn_branch)
    rev=$(parse_svn_rev)
    if [ -n "$rev" ]; then
	printf " (svn::%s::%s)" $rev $branch
    fi
}

hg_prompt_info()
{
    hg prompt " (hg::{branch}{::{bookmark}}{status})" 2> /dev/null
}

scm_status()
{
    git=$(git_prompt_info)
    svn=$(svn_prompt_info)
    bzr=$(bzr_prompt_info)
    hg=$(hg_prompt_info)
    printf "${Green}%s${Color_Off}" "${git}${svn}${bzr}${hg}"
}


# Display the status of the last command.
# A red sign means failed, blue means success.
cmd_status()
{
    if [ $1 -eq 0 ]; then
	printf "${Cyan}\$${Color_Off}"
    else
	printf "${Red}\$${Color_Off}"
    fi
}

# Set the prompt.
set_prompt()
{
    last_status=$?
    PS1="["
    PS1+="$(fg_rgb 155 155 155)\u$(color_off)"
    PS1+="$(fg_rgb 140 0 140)@$(color_off)"
    PS1+="$(fg_rgb 100 100 100)\h$(color_off)"
    PS1+="(\j)"
    PS1+=" \W"
    PS1+=$(scm_status)
    PS1+="]"
    PS1+="$(cmd_status ${last_status}) "
}

if [[ $(tty) =~ .*tty^s.* ]] || [[ "$TERM" =~ ^linux$ ]]; then
    export PROMPT_COMMAND='set_prompt'
else
    export PROMPT_COMMAND='set_prompt'
fi
