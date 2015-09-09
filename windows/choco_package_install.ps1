function Test-CommandExists ($cmd)
{
    try
    {
	if (Get-Command $cmd)
	{
	    return 1
	}
    }
    catch
    {
	return 0
    }
}

function Test-Administrator
{
    $user = [Security.Principal.WindowsIdentity]::GetCurrent();
    (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}

if (!(Test-Administrator))
{
    Write-Host "This script needs to be run as an administrator."
    Exit 1
}

# Install Chocolatey if not already installed.
if (!(Test-CommandExists Choco))
{
     Invoke-Expression ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1')) | Out-Null
}

$packages = @(
    "7zip",
    "firefox",
    "emacs",
    "inkscape",
    "autohotkey",
    "vlc"
)

foreach ($pkg in $packages)
{
    Write-Host "Installing $pkg"
    choco install $pkg
}
