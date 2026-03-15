# Bulk RNA-seq App (Windows)

这是一个基于 Shiny + Electron 的 Bulk RNA-seq 桌面应用，当前仅支持 Windows。

## 功能概览
- 加载已有结果目录（results）进行可视化分析。
- 从 BAM/GTF/sample sheet 运行新分析流程。
- 支持 PCA、Volcano、Heatmap、GO 富集、结果导出。
- 无需额外配置环境（内置 R 与依赖库）。

## 目录说明
- app.R: 应用入口
- R/: 业务模块（helpers_*.R, mod_*.R）
- scripts/: 启动、依赖、打包脚本
- electron/: Electron 壳与打包配置
- www/: 前端样式资源
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


## 安装与启动
- 推荐使用 Setup.exe 安装。
- 安装后从开始菜单或桌面图标启动。
- 安装器支持自定义安装路径。

## 版本管理
- 版本号在 electron/package.json 的 version 字段维护。

## 常见问题
1. 启动超时（Timed out while waiting for local Shiny service）
- 检查 runtime 是否完整。
- 结束旧进程（BulkSeq Visualization.exe / Rscript.exe）后重试。

2. 新分析报 DESeq2 is missing
- 重新准备 runtime（必要时加 -InstallMissing）。
