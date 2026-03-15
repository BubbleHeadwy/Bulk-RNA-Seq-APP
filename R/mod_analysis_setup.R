mod_analysis_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-card",
    shiny::h3("分析设置"),
    shiny::uiOutput(ns("gate_status")),
    shiny::checkboxInput(ns("use_batch"), "启用 batch correction", value = FALSE),
    shiny::radioButtons(
      ns("formula_mode"),
      "设计公式模式",
      choices = c("推荐公式" = "recommended", "自定义公式" = "custom"),
      selected = "recommended",
      inline = TRUE
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'recommended'", ns("formula_mode")),
      shiny::uiOutput(ns("recommended_formula_ui"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'custom'", ns("formula_mode")),
      shiny::textInput(ns("custom_formula"), "自定义设计公式", value = "~ condition")
    ),
    shiny::uiOutput(ns("contrast_factor_ui")),
    shiny::uiOutput(ns("experiment_group_ui")),
    shiny::uiOutput(ns("control_group_ui")),
    shiny::uiOutput(ns("genotype_filter_ui")),
    shiny::uiOutput(ns("condition_filter_ui")),
    shiny::numericInput(ns("lfc_threshold"), "log2 fold change 阈值", value = 1, step = 0.1),
    shiny::numericInput(ns("padj_threshold"), "padj 阈值", value = 0.05, min = 0, max = 1, step = 0.01),
    shiny::p(class = "help-text", "组间比较只在 PCA 确认后的已选样本上进行。"),
    shiny::actionButton(ns("apply_settings"), "生成当前 1v1 对比分析", class = "btn-primary")
  )
}

mod_analysis_setup_server <- function(id, metadata_r, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    apply_counter <- shiny::reactiveVal(0L)

    metadata_info <- shiny::reactive({
      metadata <- metadata_r()
      if (is.null(metadata) || nrow(metadata) == 0) {
        return(list(
          metadata = NULL,
          formulas = character(0),
          contrast_candidates = character(0),
          default_factor = "",
          group_choices = character(0),
          genotype_choices = "全部",
          condition_choices = "全部"
        ))
      }

      candidate_columns <- setdiff(names(metadata), c("sample_id", "bam_file", "replicate"))
      varying_columns <- candidate_columns[vapply(candidate_columns, function(col) {
        values <- trimws(as.character(metadata[[col]]))
        values <- values[!is.na(values) & nzchar(values)]
        length(unique(values)) > 1
      }, logical(1))]

      formulas <- character(0)
      if ("condition" %in% varying_columns) formulas <- c(formulas, "~ condition")
      if ("genotype" %in% varying_columns) formulas <- c(formulas, "~ genotype")
      if (all(c("genotype", "condition") %in% varying_columns)) formulas <- c(formulas, "~ genotype + condition")
      formulas <- c(formulas, generate_design_candidates(metadata)$formula)
      formulas <- unique(formulas[nzchar(formulas)])
      if (isTRUE(input$use_batch) && "batch" %in% names(metadata)) {
        formulas <- unique(c("~ batch + condition", "~ batch + genotype", "~ batch + genotype + condition", formulas))
      }

      contrast_candidates <- varying_columns
      default_factor <- if ("condition" %in% contrast_candidates) "condition" else if ("genotype" %in% contrast_candidates) "genotype" else if (length(contrast_candidates) > 0) contrast_candidates[[1]] else ""
      group_choices <- if (nzchar(default_factor) && default_factor %in% names(metadata)) unique(metadata[[default_factor]]) else character(0)

      list(
        metadata = metadata,
        formulas = formulas,
        contrast_candidates = contrast_candidates,
        default_factor = default_factor,
        group_choices = group_choices,
        genotype_choices = c("全部", unique(metadata$genotype)),
        condition_choices = c("全部", unique(metadata$condition))
      )
    })

    output$gate_status <- shiny::renderUI({
      if (is.null(app_state$current_pca_analysis) || nrow(app_state$current_pca_analysis$pca %||% data.frame()) == 0) {
        return(shiny::p(class = "help-text", "请先生成并检查当前样本的 PCA，再进行组间对比分析。"))
      }
      if (isTRUE(app_state$sample_selection_dirty)) {
        return(shiny::p(class = "help-text", "样本选择已变化。请先回到“数据源”页更新 PCA，确认样本后再做组间对比。"))
      }
      shiny::p(class = "help-text", paste0("当前可用于对比分析的样本数：", nrow(metadata_info()$metadata)))
    })

    output$recommended_formula_ui <- shiny::renderUI({
      info <- metadata_info()
      shiny::selectInput(session$ns("recommended_formula"), "推荐设计公式", choices = info$formulas, selected = if (length(info$formulas) > 0) info$formulas[[1]] else character(0))
    })

    output$contrast_factor_ui <- shiny::renderUI({
      info <- metadata_info()
      shiny::selectInput(session$ns("contrast_factor"), "对比因子", choices = info$contrast_candidates, selected = info$default_factor)
    })

    current_groups <- shiny::reactive({
      info <- metadata_info()
      metadata <- info$metadata
      factor_name <- input$contrast_factor %||% ""
      if (is.null(metadata) || !nzchar(factor_name) || !factor_name %in% names(metadata)) {
        return(info$group_choices)
      }
      unique(metadata[[factor_name]])
    })

    output$experiment_group_ui <- shiny::renderUI({
      groups <- current_groups()
      shiny::selectInput(session$ns("experiment_group"), "实验组", choices = groups, selected = if (length(groups) > 0) groups[[1]] else character(0))
    })

    output$control_group_ui <- shiny::renderUI({
      groups <- current_groups()
      selected <- if (length(groups) >= 2) groups[[2]] else if (length(groups) == 1) groups[[1]] else character(0)
      shiny::selectInput(session$ns("control_group"), "对照组", choices = groups, selected = selected)
    })

    output$genotype_filter_ui <- shiny::renderUI({
      info <- metadata_info()
      shiny::selectInput(session$ns("genotype_filter"), "基因型筛选", choices = info$genotype_choices, selected = "全部")
    })

    output$condition_filter_ui <- shiny::renderUI({
      info <- metadata_info()
      shiny::selectInput(session$ns("condition_filter"), "条件筛选", choices = info$condition_choices, selected = "全部")
    })

    shiny::observe({
      app_state$use_batch <- isTRUE(input$use_batch)
    })

    shiny::observeEvent(input$apply_settings, {
      if (is.null(app_state$current_pca_analysis) || nrow(app_state$current_pca_analysis$pca %||% data.frame()) == 0) {
        shiny::showNotification("请先生成 PCA。", type = "error", duration = 6)
        return()
      }
      if (isTRUE(app_state$sample_selection_dirty)) {
        shiny::showNotification("样本选择已经变化，请先回到“数据源”页更新 PCA。", type = "error", duration = 6)
        return()
      }
      apply_counter(apply_counter() + 1L)
      shiny::showNotification("已开始生成当前 1v1 对比分析。", type = "message", duration = 4)
    })

    settings_r <- shiny::reactive({
      metadata <- metadata_info()$metadata
      req(!is.null(metadata), nrow(metadata) > 0)
      design_formula <- if (identical(input$formula_mode, "custom")) trimws(input$custom_formula) else (input$recommended_formula %||% "")
      if (isTRUE(input$use_batch) && "batch" %in% names(metadata) && !grepl("\\bbatch\\b", design_formula)) {
        design_formula <- paste("~ batch +", sub("^~\\s*", "", design_formula))
      }
      list(
        use_batch = isTRUE(input$use_batch),
        design_formula = design_formula,
        contrast_factor = input$contrast_factor,
        experiment_group = input$experiment_group,
        control_group = input$control_group,
        genotype_filter = input$genotype_filter,
        condition_filter = input$condition_filter,
        lfc_threshold = input$lfc_threshold,
        padj_threshold = input$padj_threshold,
        organism_db = "org.Mm.eg.db"
      )
    })

    list(settings = settings_r, apply_counter = shiny::reactive(apply_counter()))
  })
}
