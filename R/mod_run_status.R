mod_run_status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "app-card",
      shiny::h3("任务状态"),
      shiny::uiOutput(ns("summary"))
    ),
    shiny::div(
      class = "app-card",
      shiny::h4("最近日志"),
      shiny::verbatimTextOutput(ns("logs"))
    )
  )
}

mod_run_status_server <- function(id, data_source, settings_r, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      selected_n <- length(app_state$current_selected_samples %||% character(0))
      total_n <- if (!is.null(data_source$sample_metadata())) nrow(data_source$sample_metadata()) else 0L
      dropped_n <- max(total_n - selected_n, 0L)
      summary <- app_state$current_summary

      shiny::tagList(
        shiny::p(class = "help-text", paste0("当前状态：", app_state$status_text %||% "等待操作")),
        shiny::p(class = "help-text", app_state$status_detail %||% ""),
        shiny::tags$ul(
          shiny::tags$li(paste0("当前已选样本数：", selected_n)),
          shiny::tags$li(paste0("当前弃置样本数：", dropped_n)),
          shiny::tags$li(paste0("PCA 状态：", if (!is.null(app_state$current_pca_analysis)) "已生成" else "未生成")),
          shiny::tags$li(paste0("1v1 对比状态：", if (!is.null(app_state$current_contrast_analysis)) "已生成" else "未生成")),
          if (!is.null(summary)) shiny::tags$li(paste0("结果路径：", summary$result_path))
        ),
        if (!is.null(summary)) {
          shiny::tags$ul(
            shiny::tags$li(paste0("结果来源：", if (identical(summary$mode, "load_results")) "加载已有结果" else "运行新分析")),
            shiny::tags$li(paste0("设计公式：", summary$design_formula)),
            shiny::tags$li(paste0("对比：", summary$contrast, "（因子：", summary$factor, "）")),
            shiny::tags$li(paste0("检测基因数：", summary$tested_genes)),
            shiny::tags$li(paste0("显著差异基因数：", summary$significant_genes)),
            shiny::tags$li(paste0("GO 显著条目数：", summary$go_terms))
          )
        }
      )
    })

    output$logs <- shiny::renderText({
      if (length(app_state$logs) == 0) {
        return("暂无日志。")
      }
      paste(app_state$logs, collapse = "\n")
    })
  })
}
