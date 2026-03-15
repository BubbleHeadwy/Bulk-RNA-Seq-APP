param(
  [string]$PortableRPath = $env:BULKSEQ_PORTABLE_R,
  [string]$SourceLibraryPath = $env:BULKSEQ_SOURCE_LIB,
  [string]$RuntimePath = (Join-Path $PSScriptRoot "..\\runtime"),
  [switch]$InstallMissing
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Resolve-FullPath([string]$PathText) {
  if ([string]::IsNullOrWhiteSpace($PathText)) {
    return ""
  }
  return [System.IO.Path]::GetFullPath($PathText)
}

function Ensure-LibraryJunction([string]$LinkPath, [string]$TargetPath) {
  if (Test-Path -Path $LinkPath) {
    Remove-Item -Path $LinkPath -Recurse -Force
  }

  try {
    New-Item -ItemType Junction -Path $LinkPath -Target $TargetPath | Out-Null
    return
  } catch {
    cmd /c "mklink /J `"$LinkPath`" `"$TargetPath`"" | Out-Null
    if (-not (Test-Path -Path $LinkPath)) {
      throw "Failed to create junction '$LinkPath' -> '$TargetPath'."
    }
  }
}

$repoRoot = Resolve-FullPath (Join-Path $PSScriptRoot "..")
$portableRPath = Resolve-FullPath $PortableRPath
$sourceLibraryPath = Resolve-FullPath $SourceLibraryPath
$runtimePath = Resolve-FullPath $RuntimePath

if ([string]::IsNullOrWhiteSpace($portableRPath) -or -not (Test-Path -Path $portableRPath -PathType Container)) {
  throw "Portable R folder not found. Set -PortableRPath or BULKSEQ_PORTABLE_R."
}

$portableRscript = Join-Path $portableRPath "bin\\Rscript.exe"
if (-not (Test-Path -Path $portableRscript -PathType Leaf)) {
  throw "Rscript.exe not found under '$portableRPath\\bin'."
}

if (Test-Path -Path $runtimePath) {
  Remove-Item -Path $runtimePath -Recurse -Force
}
New-Item -ItemType Directory -Path $runtimePath | Out-Null

$runtimeRDir = Join-Path $runtimePath "R"
$runtimeLibRealDir = Join-Path $runtimeRDir "library"
$runtimeLibLinkDir = Join-Path $runtimePath "library"
$runtimeRscript = Join-Path $runtimeRDir "bin\\Rscript.exe"

Copy-Item -Path $portableRPath -Destination $runtimeRDir -Recurse -Force
if (-not (Test-Path -Path $runtimeLibRealDir -PathType Container)) {
  New-Item -ItemType Directory -Path $runtimeLibRealDir | Out-Null
}
Ensure-LibraryJunction -LinkPath $runtimeLibLinkDir -TargetPath $runtimeLibRealDir

if (-not [string]::IsNullOrWhiteSpace($sourceLibraryPath) -and (Test-Path -Path $sourceLibraryPath -PathType Container)) {
  $minimizeScript = Join-Path $repoRoot "scripts\\sync_minimal_runtime_lib.R"
  if (-not (Test-Path -Path $minimizeScript -PathType Leaf)) {
    throw "sync_minimal_runtime_lib.R not found: $minimizeScript"
  }
  if ((Resolve-FullPath $sourceLibraryPath) -ne (Resolve-FullPath $runtimeLibRealDir)) {
    & $runtimeRscript $minimizeScript "--source-lib=$sourceLibraryPath" "--target-lib=$runtimeLibRealDir"
    if ($LASTEXITCODE -ne 0) {
      throw "Minimal runtime library sync failed with exit code $LASTEXITCODE."
    }
  }
}

if ($InstallMissing) {
  $installScript = Join-Path $repoRoot "scripts\\install_runtime_deps.R"
  if (-not (Test-Path -Path $installScript -PathType Leaf)) {
    throw "install_runtime_deps.R not found: $installScript"
  }
  & $runtimeRscript $installScript "--lib=$runtimeLibRealDir"
  if ($LASTEXITCODE -ne 0) {
    throw "Runtime dependency installation failed with exit code $LASTEXITCODE."
  }
}

$manifest = [ordered]@{
  generated_at = (Get-Date).ToString("s")
  portable_r = $portableRPath
  source_library = $sourceLibraryPath
  runtime_path = $runtimePath
  install_missing = [bool]$InstallMissing
}
$manifest | ConvertTo-Json -Depth 4 | Set-Content -Path (Join-Path $runtimePath "manifest.json") -Encoding UTF8

Write-Host "Runtime prepared at: $runtimePath"
Write-Host "Embedded R: $runtimeRDir"
Write-Host "Embedded library (real): $runtimeLibRealDir"
Write-Host "Embedded library (link): $runtimeLibLinkDir -> $runtimeLibRealDir"
