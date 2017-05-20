function unzip($file, $destination)
{
    New-Item -ItemType Directory -Force -Path $destination -WarningAction SilentlyContinue | Out-Null
    $shell = new-object -com shell.application
    $zip = $shell.Namespace($file)
    foreach ($item in $zip.items())
    {
        $shell.Namespace($destination).copyhere($item)
    }
}


function Test-Administrator
{
    $user = [Security.Principal.WindowsIdentity]::GetCurrent();
    (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}


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
