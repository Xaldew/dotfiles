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

# Copy AutoHotkey configuration to the default load path.
Write-Output "`$dotfilesDir = '$dotfilesdir'" | Out-File $profile
Write-Output ". `$dotfilesDir'/windows/profile.ps1'" | Out-File -Append $profile

# Copy all Emacs configuration to APPDATA as a backup.
New-Item -Path $Env:APPDATA/.emacs.d -ItemType Directory -Force | Out-Null
Copy-Item $dotfilesDir/configs/emacs.d/* -Destination $Env:APPDATA/.emacs.d/ -Force -Recurse

# Copy AutoHotkey configuration to the default load path.
$docDir = [environment]::GetFolderPath("MyDocuments")
Copy-Item -Path $dotfilesDir/windows/AutoHotkey.ahk -Destination $docDir/AutoHotkey.ahk -Force -Recurse


# Add Autostart files.
$startupDir = [environment]::GetFolderPath("StartUp")
$ahkExe    = "$Env:ProgramFiles/AutoHotkey/AutoHotkey.exe"
create-shortcut $startupDir/AutoHotkey.lnk $ahkExe

# $emacsExe = Get-Command runemacs
# create-shortcut $startupDir/emacs.lnk $emacsExe.Path

# Install additional dictionaries and grammar tools.
if (!(Test-Path -Path $Env:HOMEDRIVE/Hunspell -PathType Container))
{
    git clone git://gerrit.libreoffice.org/dictionaries $HOME/git/dictionaries
    New-Item -Path $Env:HOMEDRIVE/Hunspell -ItemType Directory -Force | Out-Null
    Get-ChildItem $HOME/git/dictionaries -Recurse |
      Where-Object { $_.Name -Match '.*\.(dic|dat|aff)' } |
      Copy-Item -Destination $Env:HOMEDRIVE/Hunspell
}
