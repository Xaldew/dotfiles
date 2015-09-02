function unzip($file, $dstPath)
{
    $shell = new-object -com shell.application
    New-Item -ItemType Directory -Force -Path $dstPath -WarningAction SilentlyContinue | Out-Null
    $shell.namespace($dstPath).copyhere($shell.namespace($file.FullName).items())
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

$packages = @(
    @{
	title='7zip Extractor';
	url='http://www.7-zip.org/a/7z1506-x64.exe';
	Arguments='';
    },
    @{
	title='Git';
	url='https://github.com/git-for-windows/git/releases/download/v2.5.0.windows.1/Git-2.5.0-64-bit.exe';
	Arguments='';
    },
    @{
	title='Emacs 24.5';
	url='http://ftp.gnu.org/gnu/emacs/windows/emacs-24.5-bin-i686-mingw32.zip';
	Arguments='';
	InstallPath="$Env:ProgramFiles";
	AddPath="bin"
    },
    @{
	title='Notepad++ 6.8.2';
	url='https://notepad-plus-plus.org/repository/6.x/6.8.2/npp.6.8.2.Installer.exe';
	Arguments='';
    },
    @{
	title='AutoHotkey';
	url='http://ahkscript.org/download/ahk-install.exe'
	Arguments='';
    }
)

$downloadDir = [io.path]::combine([Environment]::GetFolderPath("MyDocuments"), "installs")
If (!(Test-Path -Path $downloadDir -PathType Container))
{
    New-Item -Path $downloadDir -ItemType Directory | Out-Null
}

$webClient = New-Object System.Net.WebClient
foreach ($pkg in $packages)
{
    $pkgName = $pkg.title
    $fileName = [io.path]::GetFileName($pkg.url)
    $dstPath = [io.path]::combine($downloadDir, $fileName)
    If (!(Test-Path -Path $dstPath -PathType Leaf))
    {
	Write-Host "Downloading $pkgName"
	$webClient.DownloadFile($pkg.url, $dstPath)
    }
}


# Once we've downloaded all our files lets install them.
foreach ($pkg in $packages)
{
    $pkgName = $pkg.title
    $fileName = [io.path]::GetFileName($pkg.url)
    $dstPath = [io.path]::combine($downloadDir, $fileName) | Get-Item
    $Arguments = $pkg.Arguments
    if ($dstPath.Extension -eq ".zip")
    {
	Write-Output "Unzipping $pkgName..."
	$dst = [io.path]::combine($pkg.InstallPath, $pkgName)
	unzip $dstPath $dst
	if ($pkg.AddPath)
	{
	    Write-Output "Adding $newPath to Env:Path..."
	    $newPath = [io.path]::combine($dst, $pkg.AddPath)
	    [Environment]::SetEnvironmentVariable("Path",
						  $Env:Path + ";$newPath",
						  "User")
	}
    }
    else
    {
	Write-Output "Installing $pkgName..."
	Invoke-Expression -Command "$dstPath $Arguments"
    }
}

Exit 0
