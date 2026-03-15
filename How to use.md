# How to use

## Run from source
```powershell
Rscript .\scripts\install_deps.R
Rscript .\scripts\check_env.R
Rscript .\scripts\launch_app.R
```

## Build Windows offline installer
```powershell
.\scripts\prepare_runtime.ps1 -PortableRPath "D:\tools\R-portable" -SourceLibraryPath "D:\tools\R-lib" -InstallMissing
cd .\electron
npm install
npm run dist:win-offline
```
