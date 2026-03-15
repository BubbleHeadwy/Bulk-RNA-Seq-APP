# Bulk RNA-seq App (Windows)

这是一个基于 Shiny + Electron 的 Bulk RNA-seq 桌面应用，当前仅支持 Windows。

## 功能概览
- 加载已有结果目录（results）进行可视化分析。
- 从 BAM/GTF/sample sheet 运行新分析流程。
- 支持 PCA、Volcano、Heatmap、GO 富集、结果导出。
- 桌面封装后可离线运行（内置 R 与依赖库）。

## 目录说明
- app.R: 应用入口
- R/: 业务模块（helpers_*.R, mod_*.R）
- scripts/: 启动、依赖、打包脚本
- electron/: Electron 壳与打包配置
- www/: 前端样式资源
- runtime/: 内置运行环境（可重建）
- dist/: 打包产物（可删除后重建）
- legacy/cli_pipeline/: 历史 CLI 脚本归档

## 源码运行
在项目根目录执行：

```powershell
Rscript .\scripts\install_deps.R
Rscript .\scripts\check_env.R
Rscript .\scripts\launch_app.R
```

说明：
- launch_app.R 已设置 launch.browser = FALSE，不会额外弹浏览器页面。

## 打包 Windows 安装包（离线内置）
### 1) 准备 runtime
```powershell
cd "D:\Codex\Bulk RNA-seq App"
.\scripts\prepare_runtime.ps1 -PortableRPath "D:\RStudio\R-4.5.2" -SourceLibraryPath "D:\RStudio\R-4.5.2\library"
```

可选参数：
- -InstallMissing: 同步本地最小包后，再从镜像补齐缺失依赖（仅必要时）。

### 2) 打包
```powershell
cd "D:\Codex\Bulk RNA-seq App\electron"
npm install
npm run dist:win-offline
```

产物位置：
- dist\BulkSeq Visualization Setup <version>.exe
- dist\win-unpacked\

## 安装与启动
- 推荐使用 Setup.exe 安装。
- 安装后从开始菜单或桌面图标启动，无需命令行。
- 安装器支持自定义安装路径。

## 版本管理
- 版本号在 electron/package.json 的 version 字段维护。
- 发布建议：每次发布在 GitHub Releases 附带 Setup.exe 与更新说明。

## 节省磁盘空间建议
可删除并重建：
- dist/
- electron/node_modules/
- runtime/（删除后需重新 prepare_runtime）

建议保留：
- app.R, R/, scripts/, electron/main.js, electron/package.json, www/

## 常见问题
1. 启动超时（Timed out while waiting for local Shiny service）
- 检查 runtime 是否完整。
- 结束旧进程（BulkSeq Visualization.exe / Rscript.exe）后重试。

2. 新分析报 cannot open the connection
- 已兼容 config.yaml 缺失。
- 仍报错时请检查输入路径是否存在、是否可读写。

3. 新分析报 DESeq2 is missing
- 重新准备 runtime（必要时加 -InstallMissing）。