#!/usr/bin/env bash
# Try to figure out which distribution is being used.
# Currently tested on:
#    Ubuntu 13.10,15.04
#    Fedora 20
#    CentOS 6.6

function get_distribution ()
{
    dist="Unknown"
    release="Unknown"
    code_name="Unknown"

    if command -v lsb_release > /dev/null 2>&1; then

	# If the lsb_release command exists, use it.
    	dist=$(lsb_release --id | awk '{ print tolower($3) }')
    	release=$(lsb_release --release | awk '{ print tolower($2) }')
    	code_name=$(lsb_release --codename | awk '{ print tolower($2) }')


    elif [ -r /etc/lsb-release ]; then

	# Test if the lsb-release file exists with info, use it instead.
	source /etc/lsb-release
	dist=$DISTRIB_ID
	release=$DISTRIB_RELEASE
	code_name=$DISTRIB_CODENAME

    elif [ -r /etc/redhat-release ]; then

	# Check if we are on a redhat distribution.
	content=$(cat /etc/redhat-release)
	dist=$(echo "$content" | awk '{ print tolower($1) }')
	release=$(echo "$content" | awk '{ print tolower($3) }')
	code_name=$(echo "$content" | awk '{ print tolower($4) }')
	# Remove parantheses.
	code_name=${code_name:1:-1}

    elif [ -r /etc/os-release ]; then

	# No LSB or normal release files found.
	source /etc/os-release
	dist=$ID
	release=$VERSION_ID
	code_name=$(echo $PRETTY_NAME | awk '{ print tolower($3) }')
	code_name=${code_name:1:-1}

    fi



    while :; do
        case $1 in
            -h|-\?|--help)
                printf "get_distribution [-i|--id][-c|--codename][-r|--release]"
                exit
                ;;
            -i|--id)
                printf "%s\n" $dist
                ;;
            -c|--codename)
                printf "%s\n" $code_name
                ;;
            -r|--release)
                printf "%s\n" $release
                ;;
            --)        # End of all options.
                shift
                break
                ;;
            -?*)
                printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2
                ;;
            *)        # Default case.
                break
        esac
        shift
    done

    return 0
}

# Create a short alias for the above.
alias get_dist=get_distribution
