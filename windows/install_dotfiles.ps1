# Compute script and dotfiles base directory.
$scriptPath  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$dotfilesDir = Split-Path -Parent $scriptPath

# Source common utilities.
. $dotfilesDir/windows/install_utils.ps1


# Add various environment variables.
[Environment]::SetEnvironmentVariable("HOME", $HOME, "User")
[Environment]::SetEnvironmentVariable("ALTERNATE_EDITOR", "runemacs", "User")


# Add various file associations.
if (Test-Administrator)
{
    cmd /c ftype txtfile=emacsclientw --no-wait --alternate-editor=runemacs "%1"
    cmd /c ftype EmacsLisp=emacsclientw --no-wait --alternate-editor=runemacs "%1"
    cmd /c ftype CodeFile=emacsclientw --no-wait --alternate-editor=runemacs "%1"
    cmd /c assoc .txt=txtfile
    cmd /c assoc .text=txtfile
    cmd /c assoc .log=txtfile
    cmd /c assoc .el=EmacsLisp
    cmd /c assoc .c=CodeFile
    cmd /c assoc .cc=CodeFile
    cmd /c assoc .cpp=CodeFile
    cmd /c assoc .cxx=CodeFile
    cmd /c assoc .h=CodeFile
    cmd /c assoc .hpp=CodeFile
}

# Copy all Bash configuration.
Copy-Item -Path $dotfilesDir/configs/bashrc -Destination $HOME/.bashrc -Force
Copy-Item -Path $dotfilesDir/configs/bash_aliases -Destination $HOME/.bash_aliases -Force


# Copy all Git configuration.
Copy-Item -Path $dotfilesDir/windows/gitconfig -Destination $HOME/.gitconfig -Force
Copy-Item -Path $dotfilesDir/configs/gitignore -Destination $HOME/.gitignore -Force


# Copy all Emacs configuration.
# Note that it is placed in both APPDATA and HOME since that is where Emacs
# looks for configuration files by default.
Copy-Item $dotfilesDir/configs/emacs.d/ -Destination $HOME/.emacs.d/ -Force -Recurse
Copy-Item $dotfilesDir/configs/emacs.d/ -Destination $Env:APPDATA/.emacs.d/ -Force -Recurse


# Copy AutoHotkey configuration to the default load path.
$docDir = [environment]::GetFolderPath("MyDocuments")
Copy-Item -Path $dotfilesDir/windows/AutoHotkey.ahk -Destination $docDir/AutoHotkey.ahk -Force -Recurse


# Add Autostart files.
function create-shortcut([string]$dstPath, [string]$exeSrc, [string]$exeArgs)
{
    $Wsh = New-Object -comObject WScript.Shell
    $shortcut = $Wsh.CreateShortcut($dstPath)
    $shortcut.TargetPath = $exeSrc
    if ($exeArgs -ne $null)
    {
        $shortcut.Arguments = $exeArgs
    }
    $shortcut.Save()
}

function pin-command([string]$dstExe, [string]$exeArgs)
{
    $sa = new-object -c shell.application
    $pn = $sa.namespace($env:windir).parsename($dstExe)
    $pn.invokeverb('taskbarpin')
}


$startupDir = [environment]::GetFolderPath("StartUp")
$ahkExe    = "$Env:ProgramFiles/AutoHotkey/AutoHotkey.exe"
create-shortcut $startupDir/AutoHotkey.lnk $ahkExe

# $emacsExe = Get-Command runemacs
# create-shortcut $startupDir/emacs.lnk $emacsExe.Path "--daemon"
