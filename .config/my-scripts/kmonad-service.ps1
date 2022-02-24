
param (
[string] $action = $null
)



function restart-kmonad {
[int32] $old_pid = (get-process -name kmonad).id
Start-Kmonad
echo killing $old_pid >> kmonad.log.txt
Stop-Process -id $old_pid -Force
}

function kill-kmonad {
Stop-Process -Name kmonad -Force
}

function start-kmonad {
$scriptFile = $MyInvocation.MyCommand.Path
$scriptDir = (Split-Path $scriptFile -Parent) | Out-String
echo $scriptDir
$processOptions = @{
  FilePath = "kmonad.exe"
  WindowStyle = "hidden"
  ArgumentList = "C`:/Users/Niv/.config/kmonad/config.kbd","-l","debug","-c"
  Verb = "RunAs"
  WorkingDirectory = $scriptDir
  # RedirectStandardOutput = "config.kbd.log"
}
echo ----
echo $processOptions
echo ----

Start-Process @processOptions
}

# (Get-CimInstance Win32_Process -Filter \"name = 'kmonad.exe'\").commandline"

# ${function:$action}
$sb = (get-command $action -CommandType Function).ScriptBlock
invoke-command -scriptblock $sb