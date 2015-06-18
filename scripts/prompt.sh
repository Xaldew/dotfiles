#!/usr/bin/env sh
# Adds useful coloration and svn/git information to the prompt.

source_if $local_prefix_dir/bin/git-prompt.sh
source_if $dotfiles_dir/scripts/setup_terminal_colors.sh

sh_fg_rgb()
{
    printf "\[\33[38;%sm\]" `rgb $1 $2 $3`
}

sh_bg_rgb()
{
    printf "\[\33[48;%sm\]" `rgb $1 $2 $3`
}

sh_bold()
{
    printf "\[\33[1m\]"
}

sh_underline()
{
    printf "\[\33[4m\]"
}

sh_reverse-video()
{
    printf "\[\33[7m\]"
}

sh_color_off()
{
    printf "\[\33[0m\]"
}


bzr_prompt_info()
{
    bzr_cb=$(bzr nick 2> /dev/null | \
		    grep -v "ERROR" | \
		    cut -d ":" -f2 | \
		    awk -F / '{print " (bzr::"$1")"}')
    if [ -n "$bzr_cb" ]; then
	bzr_dirty=""
	[[ -n `bzr status` ]] && bzr_dirty="$(sh_fg_rgb 155 0 0) *"
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
    branch=$(parse_svn_url |
		    sed -e 's#^'"$(parse_svn_repository_root)"'##g' |
		    sed "s|^/branches/||" |
		    awk '{print $1}' |
		    sed -e 's|\([^/]\+\)/\?.*|\1|')
    if [[ $branch != *"trunk"* ]]; then
	printf "::%s" $branch
    else
	printf ""
    fi
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
	printf " (svn::%s%s)" $rev $branch
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
    printf "$(sh_fg_rgb 0 155 0)%s$(sh_color_off)" "${git}${svn}${bzr}${hg}"
}


# Display the status of the last command.
# A red sign means failed, blue means success.
cmd_status()
{
    if [ $1 -eq 0 ]; then
	printf "$(sh_fg_rgb 0 155 155)\$$(sh_color_off)"
    else
	printf "$(sh_fg_rgb 155 0 0)\$$(sh_color_off)"
    fi
}

job_count()
{
    stopped=$(jobs -sp | wc -l)
    running=$(jobs -rp | wc -l)
    ((running+stopped)) &&
	printf "($(sh_fg_rgb 0 100 0)${running}r$(sh_color_off)/" &&
	printf "$(sh_fg_rgb 155 40 0)${stopped}s$(sh_color_off))"
}

# Set the prompt.
set_prompt()
{
    last_status=$?
    PS1="["
    PS1+="$(sh_fg_rgb 135 135 135)\u$(sh_color_off)"
    PS1+="$(sh_fg_rgb 220 220 220)@$(sh_color_off)"
    PS1+="$(sh_fg_rgb 120 120 120)\h$(sh_color_off)"
    PS1+="$(job_count) "
    PS1+="\W"
    PS1+=$(scm_status)
    PS1+="]"
    PS1+="$(cmd_status ${last_status}) "
}

if [[ $(tty) =~ .*tty^s.* ]] || [[ "$TERM" =~ ^linux$ ]]; then
    export PROMPT_COMMAND='set_prompt'
else
    export PROMPT_COMMAND='set_prompt'
fi
