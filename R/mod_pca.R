mod_pca_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-card",
    shiny::h3("PCA"),
    shiny::uiOutput(ns("view_mode_ui")),
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("color_ui"))),
      shiny::column(6, shiny::uiOutput(ns("shape_ui")))
    ),
    plotly::plotlyOutput(ns("plot"), height = "620px")
  )
}

mod_pca_server <- function(id, pca_analysis_r, contrast_analysis_r, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    current_pca_data <- shiny::reactive({
      if (identical(app_state$pca_view_mode %||% "all", "contrast")) {
        contrast_analysis <- contrast_analysis_r()
        if (!is.null(contrast_analysis) && nrow(contrast_analysis$pca %||% data.frame()) > 0) {
          return(contrast_analysis$pca)
        }
      }
      analysis <- pca_analysis_r()
      if (is.null(analysis)) return(data.frame())
      analysis$pca %||% data.frame()
    })

    output$view_mode_ui <- shiny::renderUI({
      choices <- c("全样本 PCA" = "all")
      contrast_analysis <- contrast_analysis_r()
      if (!is.null(contrast_analysis) && nrow(contrast_analysis$pca %||% data.frame()) > 0) {
        choices <- c(choices, "当前 1v1 对比 PCA" = "contrast")
      }
      selected_mode <- app_state$pca_view_mode %||% "all"
      if (!selected_mode %in% unname(choices)) selected_mode <- "all"
      shiny::radioButtons(session$ns("view_mode"), "PCA 视图", choices = choices, selected = selected_mode, inline = TRUE)
    })

    output$color_ui <- shiny::renderUI({
      pca_df <- current_pca_data()
      req(nrow(pca_df) > 0)
      vars <- setdiff(names(pca_df), c("sample_id", "PC1", "PC2"))
      selected_color <- app_state$pca_color_var %||% if ("condition" %in% vars) "condition" else vars[[1]]
      if (!selected_color %in% vars) selected_color <- vars[[1]]
      shiny::selectInput(session$ns("color_var"), "颜色映射", choices = vars, selected = selected_color)
    })

    output$shape_ui <- shiny::renderUI({
      pca_df <- current_pca_data()
      req(nrow(pca_df) > 0)
      vars <- setdiff(names(pca_df), c("sample_id", "PC1", "PC2"))
      selected_shape <- app_state$pca_shape_var %||% if ("genotype" %in% vars) "genotype" else vars[[1]]
      if (!selected_shape %in% vars) selected_shape <- vars[[1]]
      shiny::selectInput(session$ns("shape_var"), "形状映射", choices = vars, selected = selected_shape)
    })

    shiny::observeEvent(input$view_mode, {
      app_state$pca_view_mode <- input$view_mode
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$color_var, {
      app_state$pca_color_var <- input$color_var
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$shape_var, {
      app_state$pca_shape_var <- input$shape_var
    }, ignoreInit = TRUE)

    output$plot <- plotly::renderPlotly({
      pca_df <- current_pca_data()
      if (nrow(pca_df) == 0) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              title = "暂无 PCA 结果",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(list(text = "请先加载已有结果，或运行新分析生成全样本 PCA。", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
            )
        )
      }
      plot_pca_interactive(
        pca_df,
        app_state$pca_color_var %||% "condition",
        app_state$pca_shape_var %||% "genotype",
        title = if (identical(app_state$pca_view_mode %||% "all", "contrast")) "当前 1v1 对比 PCA" else "全样本 PCA"
      )
    })
  })
}
