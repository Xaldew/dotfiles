# Configuration for all new Powershell windows.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1

# Set $HOME variable and make powershell recognize ~/ as $HOME.
set-variable -name HOME -value (resolve-path $env:Home) -force
(get-psprovider FileSystem).Home = $HOME

# Global variables and core env variables.
$HOME_ROOT = [IO.Path]::GetPathRoot($HOME)
$TOOLS = "$HOME_ROOT/tools"
$SCRIPTS = "$HOME/scripts"
$env:EDITOR = "emacs"


# Define our prompt. Show '~' instead of $HOME.
function shorten-path([string] $path)
{
    $loc = $path.Replace($HOME, '~')
    # Remove prefix for UNC paths.
    $loc = $loc -replace '^[^:]+::', ''
    # Shorten path like tabs in Vim and handle paths with \\ and . correctly.
    return ($loc -replace '\\(\.?)([^\\])[^\\]*(?=\\)','\$1$2')
}


if (!$profile.Contains("NuGet_profile"))
{
    function prompt
    {
        $cdelim = [ConsoleColor]::DarkCyan
        $chost = [ConsoleColor]::Green
        $cpref = [ConsoleColor]::Cyan
        $cloc = [ConsoleColor]::DarkYellow

        if (Test-Administrator)
        {
            $cpref = [ConsoleColor]::Yellow
        }
        write-host "`[" -n -f $cpref
        write-host "$($env:USERNAME.ToLower())" -n -f $chost
        write-host "@" -n -f $cloc
        write-host "$($env:COMPUTERNAME.ToLower())" -n -f $chost
        write-host " | " -n -f $cdelim
        write-host (shorten-path (pwd).Path) -n -f $cloc
        write-host "" -n -f $cdelim
        write-host " `]" -n -f $cpref
        write-host "`$" -n -f $chost
        return " "
    }
}

# Move to HOME directory by default.
set-location $HOME

# OS default location needs to be set as well.
[System.Environment]::CurrentDirectory = $HOME
