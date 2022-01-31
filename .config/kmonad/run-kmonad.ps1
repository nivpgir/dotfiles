$processOptions = @{
  FilePath = "kmonad.exe"
  WindowStyle = "hidden"
  ArgumentList = "C`:/Users/Niv/.config/kmonad/config.kbd","-l","debug","-c"
  # Verb = "RunAs"
  RedirectStandardOutput = "config.kbd.log"
}
echo ----
echo $processOptions

echo ----
Start-Process @processOptions