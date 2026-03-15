mod_preview_ui <- function(id, title = "数据预览") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-card",
    shiny::h4(title),
    DT::DTOutput(ns("table"))
  )
}

mod_preview_server <- function(id, data_r) {
  shiny::moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      preview_table(data_r())
    }, options = list(scrollX = TRUE, pageLength = 10))
  })
}
