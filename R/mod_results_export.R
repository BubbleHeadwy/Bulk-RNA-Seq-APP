mod_results_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-card",
    shiny::h3("结果导出"),
    shiny::p(class = "help-text", "表格导出为 CSV，图像导出为 PNG。PCA 导出会使用你当前在 PCA 页选择的视图、颜色映射和形状映射。"),
    shiny::fluidRow(
      shiny::column(6, shiny::downloadButton(ns("download_deg"), "下载差异分析结果")),
      shiny::column(6, shiny::downloadButton(ns("download_go"), "下载 GO 富集结果"))
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(6, shiny::downloadButton(ns("download_volcano"), "下载火山图 PNG")),
      shiny::column(6, shiny::downloadButton(ns("download_pca"), "下载当前 PCA PNG"))
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(6, shiny::downloadButton(ns("download_heatmap"), "下载热图 PNG")),
      shiny::column(6, shiny::downloadButton(ns("download_go_plot"), "下载 GO 图 PNG"))
    )
  )
}

mod_results_export_server <- function(id, contrast_analysis_r, pca_analysis_r, settings_r, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    current_pca_export_analysis <- shiny::reactive({
      if (identical(app_state$pca_view_mode %||% "all", "contrast") && !is.null(contrast_analysis_r())) {
        return(contrast_analysis_r())
      }
      pca_analysis_r()
    })

    output$download_deg <- shiny::downloadHandler(
      filename = function() paste0("deseq2_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        analysis <- contrast_analysis_r()
        utils::write.csv(analysis$annotated, file, row.names = FALSE)
      }
    )

    output$download_go <- shiny::downloadHandler(
      filename = function() paste0("go_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        analysis <- contrast_analysis_r()
        utils::write.csv(analysis$go, file, row.names = FALSE)
      }
    )

    output$download_volcano <- shiny::downloadHandler(
      filename = function() {
        settings <- settings_r()
        paste0("volcano_", settings$experiment_group, "_vs_", settings$control_group, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        save_volcano_png(contrast_analysis_r()$annotated, settings_r(), file)
      }
    )

    output$download_pca <- shiny::downloadHandler(
      filename = function() {
        prefix <- if (identical(app_state$pca_view_mode %||% "all", "contrast")) "pca_contrast" else "pca_all_samples"
        paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        analysis <- current_pca_export_analysis()
        title <- if (identical(app_state$pca_view_mode %||% "all", "contrast")) "当前 1v1 对比 PCA" else "全样本 PCA"
        save_pca_png(
          analysis,
          file,
          title = title,
          color_var = app_state$pca_color_var,
          shape_var = app_state$pca_shape_var
        )
      }
    )

    output$download_heatmap <- shiny::downloadHandler(
      filename = function() {
        settings <- settings_r()
        paste0("heatmap_", settings$experiment_group, "_vs_", settings$control_group, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        analysis <- contrast_analysis_r()
        analysis$comparison_label <- paste(settings_r()$experiment_group, "vs", settings_r()$control_group)
        save_heatmap_png(analysis, top_n = 50, file = file)
      }
    )

    output$download_go_plot <- shiny::downloadHandler(
      filename = function() {
        settings <- settings_r()
        paste0("go_", settings$experiment_group, "_vs_", settings$control_group, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        save_go_png(contrast_analysis_r()$go, file)
      }
    )
  })
}
