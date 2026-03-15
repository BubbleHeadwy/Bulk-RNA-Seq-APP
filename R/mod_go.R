mod_go_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-card",
    shiny::h3("GO 富集"),
    shiny::textOutput(ns("message")),
    plotly::plotlyOutput(ns("plot"), height = "620px"),
    DT::DTOutput(ns("table"))
  )
}

mod_go_server <- function(id, analysis_r) {
  shiny::moduleServer(id, function(input, output, session) {
    output$message <- shiny::renderText({
      analysis <- analysis_r()
      if (is.null(analysis)) {
        return("请先完成当前 1v1 对比分析。")
      }
      analysis$go_message %||% "暂无 GO 富集结果。"
    })

    output$plot <- plotly::renderPlotly({
      analysis <- analysis_r()
      if (is.null(analysis) || is.null(analysis$go) || nrow(analysis$go) == 0) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              title = "",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(list(text = "当前没有可展示的 GO 富集结果。", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
            )
        )
      }
      tryCatch(
        plot_go_interactive(analysis$go),
        error = function(e) {
          plotly::plot_ly() |>
            plotly::layout(
              title = "",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(list(text = conditionMessage(e), x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
            )
        }
      )
    })

    output$table <- DT::renderDT({
      analysis <- analysis_r()
      if (is.null(analysis) || is.null(analysis$go) || nrow(analysis$go) == 0) {
        return(DT::datatable(data.frame(提示 = "当前没有可展示的 GO 富集结果。", stringsAsFactors = FALSE), options = list(dom = "t")))
      }
      DT::datatable(preview_table(analysis$go, 20), options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
