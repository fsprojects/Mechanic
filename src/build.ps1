$ErrorActionPreference = "Stop"

function Find-Exe() {
  param([string]$name, [string]$root)
  return Get-ChildItem -Recurse -Include "$name" "$root" | Select-Object -First 1 -ExpandProperty FullName
}

$PAKET_EXE = Find-Exe -name 'paket.exe' -root '.paket'
& "$PAKET_EXE" restore

$FAKE_EXE = Find-Exe -name 'FAKE.exe' -root 'packages\build'
& "$FAKE_EXE" build.fsx $args
