# Bulk RNA-seq Desktop App (Windows)

鏈」鐩槸涓€涓熀浜?`Shiny + Electron` 鐨?Bulk RNA-seq 妗岄潰搴旂敤锛屽綋鍓嶄粎鏀寔 Windows銆?
## 鐩爣鍙戝竷褰㈡€?- 婧愮爜鍙戝竷锛氭彁浜ゅ埌 GitHub 浠撳簱锛堜笉鎻愪氦瀹夎鍖呭拰 runtime 浜岃繘鍒讹級銆?- 瀹夎鍖呭彂甯冿細閫氳繃 GitHub Releases 鍒嗗彂 Windows `nsis` 瀹夎鍣ㄥ拰 `portable exe`銆?- 渚濊禆绛栫暐锛氱绾垮唴缃?`runtime/R` 涓?`runtime/library`锛屽敖閲忎笉渚濊禆鐢ㄦ埛绯荤粺鐜銆?
## 鐩綍璇存槑
```text
.
鈹溾攢 app.R
鈹溾攢 R/
鈹? 鈹溾攢 common.R
鈹? 鈹溾攢 helpers_*.R
鈹? 鈹斺攢 mod_*.R
鈹溾攢 electron/
鈹溾攢 scripts/
鈹溾攢 www/
鈹斺攢 legacy/
   鈹斺攢 cli_pipeline/
```

`legacy/cli_pipeline/` 瀛樻斁鏃х増 CLI 娴佹按绾胯剼鏈紝涓嶅弬涓庢闈㈢増杩愯涓庢墦鍖呫€?
## 绂荤嚎瀹夎鍖呮瀯寤猴紙鎺ㄨ崘锛?### 1) 鍑嗗 runtime
鍦ㄤ粨搴撴牴鐩綍鎵ц锛圥owerShell锛夛細

```powershell
.\scripts\prepare_runtime.ps1 -PortableRPath "D:\tools\R-portable" -SourceLibraryPath "D:\tools\R-lib" -InstallMissing
```

鍙傛暟璇存槑锛?- `-PortableRPath`锛氫究鎼哄紡 R 鏍圭洰褰曪紙蹇呴』锛岀洰褰曚笅搴旀湁 `bin\Rscript.exe`锛夈€?- `-SourceLibraryPath`锛氬彲閫夛紝宸叉湁 R 鍖呭簱鐩綍锛屼細澶嶅埗鍒?`runtime\library`銆?- `-InstallMissing`锛氬彲閫夛紝浣跨敤 `runtime\R\bin\Rscript.exe` 鍦ㄧ嚎琛ラ綈缂哄け鍖呫€?
### 2) 鏋勫缓 Windows 瀹夎鍖?```powershell
cd .\electron
npm install
npm run dist:win-offline
```

鏋勫缓杈撳嚭鐩綍锛歚dist/`

## 婧愮爜杩愯锛堝紑鍙戞ā寮忥級
```powershell
Rscript .\scripts\install_deps.R
Rscript .\scripts\check_env.R
Rscript .\scripts\launch_app.R
```

## 鎵撳寘涓庤繍琛屽叧閿偣
- Electron 鍚姩鏃讹紝`Rscript` 鏌ユ壘椤哄簭涓猴細
1. `resources/runtime/R/bin/Rscript.exe`锛堝畨瑁呭寘鍐呯疆锛?2. 鐜鍙橀噺 `RSCRIPT_PATH`
3. 绯荤粺 `Rscript`
- 杩愯鏃朵紭鍏堝皢 `runtime/library` 鍔犲叆 `.libPaths()`銆?- `app.R` 閲囩敤妯″潡鐧藉悕鍗曞姞杞斤紝閬垮厤 legacy 鑴氭湰琚鎵ц銆?
## GitHub 鍙戝竷寤鸿
- 浠撳簱鎻愪氦婧愮爜涓庢瀯寤鸿剼鏈紝涓嶆彁浜や互涓嬪唴瀹癸細
- `dist/`, `release/`, `runtime/`, `*.exe`, `*.zip`, `node_modules/`
- 鍦?GitHub Releases 涓婁紶瀹夎鍖咃紝骞堕檮甯︼細
- 鐗堟湰鍙?- 鍖呬綋绉?- 绯荤粺瑕佹眰锛圵indows 10/11锛?- 鏄惁鍐呯疆 runtime锛堟湰椤圭洰涓哄唴缃級
