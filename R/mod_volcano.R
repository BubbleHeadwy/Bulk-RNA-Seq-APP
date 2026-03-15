mod_volcano_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-card",
    shiny::h3("火山图"),
    plotly::plotlyOutput(ns("plot"), height = "640px"),
    DT::DTOutput(ns("table"))
  )
}

mod_volcano_server <- function(id, analysis_r, settings_r) {
  shiny::moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly({
      analysis <- analysis_r()
      if (is.null(analysis) || is.null(analysis$annotated) || nrow(analysis$annotated) == 0) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              title = "暂无火山图",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(list(text = "请先完成当前 1v1 对比分析。", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
            )
        )
      }
      settings <- settings_r()
      comparison_label <- paste(settings$experiment_group %||% "", "vs", settings$control_group %||% "")
      plot_volcano_interactive(analysis$annotated, settings$lfc_threshold, settings$padj_threshold, comparison_label = comparison_label)
    })

    output$table <- DT::renderDT({
      analysis <- analysis_r()
      if (is.null(analysis) || is.null(analysis$annotated) || nrow(analysis$annotated) == 0) {
        return(DT::datatable(data.frame(提示 = "请先完成当前 1v1 对比分析。", stringsAsFactors = FALSE), options = list(dom = "t")))
      }
      table_df <- analysis$annotated[order(analysis$annotated$padj), c("gene_id", "display_label", "log2FoldChange", "padj", "baseMean"), drop = FALSE]
      DT::datatable(preview_table(table_df, 20), options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
