mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-card",
    shiny::h3("热图"),
    shiny::sliderInput(ns("top_n"), "高变基因数量", min = 20, max = 200, value = 50, step = 10),
    plotly::plotlyOutput(ns("plot"), height = "700px")
  )
}

mod_heatmap_server <- function(id, analysis_r, settings_r) {
  shiny::moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly({
      analysis <- analysis_r()
      if (is.null(analysis) || is.null(analysis$normalized) || is.null(analysis$metadata) || is.null(analysis$annotated)) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              title = "暂无热图",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(list(text = "请先完成当前 1v1 对比分析。", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
            )
        )
      }
      settings <- settings_r()
      comparison_label <- paste(settings$experiment_group %||% "", "vs", settings$control_group %||% "")
      analysis$comparison_label <- comparison_label
      plot_heatmap_interactive(analysis$normalized, analysis$metadata, analysis$annotated, top_n = input$top_n, title = comparison_label)
    })
  })
}
