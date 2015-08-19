$source = 'C:\source'
If (!(Test-Path -Path $source -PathType Container))
{
    New-Item -Path $source -ItemType Directory | Out-Null
}

$packages = @(
    @{
	title='7zip Extractor';
	url='http://downloads.sourceforge.net/sevenzip/7z920-x64.msi';
	Arguments=' /qn';
	Destination=$source
    },
    @{
	title='Putty 0.63';
	url='http://the.earth.li/~sgtatham/putty/latest/x86/putty-0.63-installer.exe';
	Arguments=' /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-';
	Destination=$source
    },
    @{
	title='Notepad++ 6.6.8';
	url='http://download.tuxfamily.org/notepadplus/6.6.8/npp.6.6.8.Installer.exe';
	Arguments=' /Q /S';
	Destination=$source
    }
)

foreach ($pkg in $packages)
{
    $pkgName = $pkg.title
    $fileName = [io.path]::GetFileName($pkg.url)
    $dstPath = [io.path]::combine($pkg.Destination, $fileName)
    If (!(Test-Path -Path $dstPath -PathType Leaf))
    {
	Write-Host "Downloading $pkgName"
	$webClient = New-Object System.Net.WebClient
	$webClient.DownloadFile($pkg.url, $dstPath)
    }
}


# Once we've downloaded all our files lets install them.
foreach ($pkg in $packages)
{
    $pkgName = $pkg.title
    $fileName = [io.path]::GetFileName($pkg.url)
    $dstPath = [io.path]::combine($pkg.Destination, $fileName)
    $Arguments = $pkg.Arguments
    Write-Output "Installing $pkgName"
    #    Invoke-Expression -Command "$dstPath $Arguments"
}
