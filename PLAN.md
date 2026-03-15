# Visualization 中文桌面应用开发计划（修订版）

## Summary
- 在 `Visualization/` 下开发一个中文界面的 Shiny 桌面应用，并以 `Shiny + Electron` 作为第一版桌面封装路线，首发平台仅限 Windows。
- 应用采用“双模式”：
  - `运行新分析`：从 `.bam` 与 `.gtf/.gtf.gz` 出发，完成计数、差异分析、注释、GO 富集与交互可视化。
  - `加载已有结果`：直接读取现有 `results/` 输出进行浏览与交互分析。
- 第一版桌面包目标是“用户无需单独安装 R 或 R 包即可运行”；因此打包版以内置便携式 R、本地包库、Shiny 应用、`samtools.exe` 和必要 Windows 依赖为默认方案。
- 为降低外部依赖复杂度，打包版的 BAM 计数优先采用 `Rsubread::featureCounts`，开发模式仍保留对外部 `featureCounts` 的兼容。
- 计划文档、应用界面、安装说明、错误提示和帮助文档统一使用中文。

## Key Changes
- 安装与依赖配置
  - 在根目录 `README.md` 增加“可视化桌面应用”章节。
  - 在 `Visualization/README.md` 提供中文安装、运行和打包说明。
  - 在 `Visualization/scripts/` 中提供：
    - `install_deps.R`
    - `check_env.R`
    - `launch_app.R`
- 错误处理与默认值
  - `mod_data_source.R` 负责输入验证，并提供中文错误提示。
  - 加载已有结果时校验 count matrix、DESeq2、注释与 GO 结果文件结构。
  - 提供默认阈值：`padj_threshold = 0.05`、`lfc_threshold = 1`。
- GUI 交互优化
  - 长任务显示进度条、状态文本、日志面板。
  - 数据源选择后提供 BAM、sample sheet、GTF、结果文件预览。
  - 提供火山图、PCA、热图和 GO 富集的交互浏览。
- 模块化设计
  - `mod_data_source.R`
  - `mod_analysis_setup.R`
  - `mod_run_status.R`
  - `mod_preview.R`
  - `mod_results_export.R`
  - `helpers_validation.R`
  - `helpers_pipeline.R`
  - `helpers_plots.R`
- 分析与数据流
  - 第一版严格使用 `.bam` 作为主原始输入，绝不以 `03.Result/` 作为主分析来源。
  - 输出仍全部写入 `results/`，并保持“不覆盖已有输出”的默认规则。
- 桌面打包
  - 采用 `Shiny + Electron`。
  - Windows 安装包或绿色版 `.exe` 通过 GitHub Releases 分发。

## Implementation Changes
- 第一阶段：文档与基础骨架
  - 写入 `Visualization/PLAN.md`。
  - 补充根目录与子目录 README。
  - 搭建应用目录骨架和启动脚本。
- 第二阶段：结果加载与验证优先
  - 实现“加载已有结果”模式。
  - 完成结果文件校验、数据预览、中文错误提示、默认阈值与交互式图表。
- 第三阶段：运行新分析模式
  - 实现 BAM/GTF/sample sheet 输入选择、预检查和预览。
  - 加入设计公式候选、自定义公式、batch correction 与对比分组联动。
  - 打通 `Rsubread + DESeq2 + 注释 + GO` 的完整流程。
- 第四阶段：桌面封装与发布
  - 建立 Electron 壳层。
  - 准备便携式 R 与本地包库打包脚本。
  - 生成 Windows 安装包。

## Test Plan
- 安装与环境
  - 在干净 Windows R 4.5.x 上能通过 `install_deps.R` 成功安装依赖。
  - `check_env.R` 能正确报告缺失包、缺失 `samtools`、缺失写权限等问题。
- 输入验证
  - 错误后缀 BAM、缺失 `.bam.bai`、错误 GTF 后缀、空 sample sheet、缺列 sample sheet 均被正确拦截。
  - `batch` 开启但 sample sheet 无 `batch` 列时，界面应明确报错。
- 交互与反馈
  - 选择文件后能看到预览表格和状态摘要。
  - 更换设计公式、对比组、阈值后，图表与结果表联动刷新。
  - 长任务过程中始终显示进度条和当前步骤说明。
- 分析功能
  - 无显著差异基因时，GO 模块显示友好提示，不报崩溃错误。
  - 仍保证只从 `.bam` 作为主输入，不写 raw data，不修改 `04.Ref/`，不覆盖已有 `results/`。
- 打包
  - 安装包在未安装 R 的 Windows 机器上可启动。
  - 打包版至少支持“加载已有结果”模式完整运行。

## Assumptions
- 第一版桌面应用只支持 Windows。
- 第一版界面、README、错误提示和计划文件全部使用中文。
- 第一版 batch correction 采用“可选 `batch` 列并入 DESeq2 设计公式”的方式，不做独立后处理去批次。
- 第一版桌面封装采用 `Shiny + Electron`，并以“内置便携式 R + 本地包库”实现免环境安装运行。
