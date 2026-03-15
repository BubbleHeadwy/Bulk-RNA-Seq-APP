param(
  [string]$PortableRPath = $env:BULKSEQ_PORTABLE_R,
  [string]$SourceLibraryPath = $env:BULKSEQ_SOURCE_LIB,
  [switch]$InstallMissing
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot ".."))
$prepareScript = Join-Path $repoRoot "scripts\\prepare_runtime.ps1"
$electronDir = Join-Path $repoRoot "electron"

if (-not (Test-Path -Path $prepareScript -PathType Leaf)) {
  throw "prepare_runtime.ps1 not found."
}
if (-not (Test-Path -Path $electronDir -PathType Container)) {
  throw "electron directory not found."
}

$prepareArgs = @("-ExecutionPolicy", "Bypass", "-File", $prepareScript, "-PortableRPath", $PortableRPath)
if (-not [string]::IsNullOrWhiteSpace($SourceLibraryPath)) {
  $prepareArgs += @("-SourceLibraryPath", $SourceLibraryPath)
}
if ($InstallMissing) {
  $prepareArgs += "-InstallMissing"
}

powershell @prepareArgs
if ($LASTEXITCODE -ne 0) {
  throw "prepare_runtime.ps1 failed with exit code $LASTEXITCODE."
}

# Keep env vars for downstream npm scripts/tools in this session.
$env:BULKSEQ_PORTABLE_R = $PortableRPath
if (-not [string]::IsNullOrWhiteSpace($SourceLibraryPath)) {
  $env:BULKSEQ_SOURCE_LIB = $SourceLibraryPath
}

Push-Location $electronDir
try {
  npm install
  if ($LASTEXITCODE -ne 0) {
    throw "npm install failed with exit code $LASTEXITCODE."
  }
  npm run dist:win-offline
  if ($LASTEXITCODE -ne 0) {
    throw "npm run dist:win-offline failed with exit code $LASTEXITCODE."
  }
} finally {
  Pop-Location
}

Write-Host "Offline Windows installer build finished."
