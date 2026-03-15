mod_data_source_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "app-card",
      shiny::h3("Data Source"),
      shiny::radioButtons(
        ns("mode"),
        "Work Mode",
        choices = c("Load Existing Results" = "load_results", "Run New Analysis" = "run_analysis"),
        selected = "load_results",
        inline = TRUE
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'load_results'", ns("mode")),
        shiny::textInput(ns("results_dir"), "results directory", value = normalizePath("results", winslash = "/", mustWork = FALSE)),
        shiny::textInput(ns("results_sample_sheet"), "sample sheet", value = normalizePath("data-processed/sample_sheet.csv", winslash = "/", mustWork = FALSE)),
        shiny::textInput(ns("results_gtf"), "GTF file (optional)", value = ""),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton(ns("load_results"), "Load Results", class = "btn-primary")),
          shiny::column(6, shiny::actionButton(ns("refresh_pca"), "Refresh PCA", class = "btn-default"))
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === 'run_analysis'", ns("mode")),
        shiny::textInput(ns("bam_dir"), "BAM directory", value = ""),
        shiny::textInput(ns("gtf_file"), "GTF file", value = ""),
        shiny::textInput(ns("sample_sheet"), "sample sheet", value = normalizePath("data-processed/sample_sheet.csv", winslash = "/", mustWork = FALSE)),
        shiny::textInput(ns("output_dir"), "output directory", value = viz_default_output_dir()),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton(ns("validate_inputs"), "Validate Inputs", class = "btn-primary")),
          shiny::column(6, shiny::uiOutput(ns("start_button_ui")))
        )
      ),
      shiny::br(),
      shiny::uiOutput(ns("status"))
    ),
    shiny::div(
      class = "app-card",
      shiny::h4("Sample Selection"),
      shiny::p(class = "help-text", "All samples are selected by default. Unselected samples are excluded from new analysis and downstream PCA/contrast steps."),
      shiny::textOutput(ns("selection_summary")),
      DT::DTOutput(ns("sample_table"))
    ),
    shiny::fluidRow(
      shiny::column(6, mod_preview_ui(ns("bam_preview"), "BAM / results preview")),
      shiny::column(6, mod_preview_ui(ns("gtf_preview"), "GTF preview"))
    )
  )
}

mod_data_source_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- shiny::reactiveValues(
      metadata = NULL,
      bam_preview = NULL,
      gtf_preview = NULL,
      validated_run_request = NULL,
      status_message = "Choose a mode and prepare input data.",
      selected_rows = integer(0),
      results_loaded = FALSE,
      input_validated = FALSE,
      start_analysis_counter = 0L,
      refresh_pca_counter = 0L
    )

    refresh_sample_preview <- function(path, include_batch = FALSE) {
      result <- validate_sample_sheet_file(path, include_batch = include_batch)
      if (isTRUE(result$ok)) {
        state$metadata <- result$data
        if (length(state$selected_rows) == 0) {
          state$selected_rows <- seq_len(nrow(state$metadata))
        } else {
          state$selected_rows <- state$selected_rows[state$selected_rows <= nrow(state$metadata)]
          if (length(state$selected_rows) == 0) {
            state$selected_rows <- seq_len(nrow(state$metadata))
          }
        }
      }
      result
    }

    selected_sample_ids_r <- shiny::reactive({
      metadata_df <- state$metadata
      if (is.null(metadata_df) || nrow(metadata_df) == 0) {
        return(character(0))
      }
      rows <- input$sample_table_rows_selected
      if (is.null(rows) || length(rows) == 0) rows <- state$selected_rows
      rows <- rows[rows >= 1 & rows <= nrow(metadata_df)]
      metadata_df$sample_id[rows]
    })

    default_config <- tryCatch(viz_project_config(), error = function(e) NULL)
    if (!is.null(default_config)) {
      shiny::observe({
        bam_default <- resolve_path(default_config$raw_data_dir, default_config$bam_dir)
        gtf_default <- resolve_path(default_config$raw_data_dir, default_config$gtf_file)
        shiny::updateTextInput(session, "results_gtf", value = gtf_default)
        shiny::updateTextInput(session, "bam_dir", value = bam_default)
        shiny::updateTextInput(session, "gtf_file", value = gtf_default)
      })
    }

    shiny::observe({
      if (identical(input$mode, "load_results")) {
        refresh_sample_preview(input$results_sample_sheet, include_batch = FALSE)
      } else {
        refresh_sample_preview(input$sample_sheet, include_batch = isTRUE(app_state$use_batch))
      }
    })

    sample_table_proxy <- DT::dataTableProxy(ns("sample_table"))

    output$sample_table <- DT::renderDT({
      metadata_df <- state$metadata
      if (is.null(metadata_df) || nrow(metadata_df) == 0) {
        return(DT::datatable(data.frame(Message = "sample sheet not loaded", stringsAsFactors = FALSE), options = list(dom = "t")))
      }
      DT::datatable(
        metadata_df,
        selection = list(mode = "multiple", selected = state$selected_rows, target = "row"),
        options = list(scrollX = TRUE, pageLength = 20, stateSave = TRUE)
      )
    })

    shiny::observeEvent(state$metadata, {
      metadata_df <- state$metadata
      if (!is.null(metadata_df) && nrow(metadata_df) > 0) {
        DT::replaceData(sample_table_proxy, metadata_df, resetPaging = FALSE, rownames = FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$sample_table_rows_selected, {
      state$selected_rows <- input$sample_table_rows_selected
    }, ignoreInit = TRUE)

    output$selection_summary <- shiny::renderText({
      metadata_df <- state$metadata
      if (is.null(metadata_df) || nrow(metadata_df) == 0) {
        return("sample sheet not loaded.")
      }
      selected_n <- length(selected_sample_ids_r())
      paste0("Selected samples: ", selected_n, " / ", nrow(metadata_df))
    })

    output$start_button_ui <- shiny::renderUI({
      can_start <- !is.null(state$validated_run_request) && length(selected_sample_ids_r()) >= 2
      shiny::actionButton(
        ns("start_analysis"),
        "Start Analysis",
        class = if (can_start) "btn-danger" else "btn-default disabled"
      )
    })

    shiny::observeEvent(input$load_results, {
      tryCatch({
        bundle <- load_results_bundle(input$results_dir, input$results_sample_sheet, input$results_gtf, selected_sample_ids = NULL)
        app_state$current_bundle <- bundle
        app_state$current_bundle_token <- app_state$current_bundle_token + 1L
        state$results_loaded <- TRUE
        state$bam_preview <- bundle$counts
        state$gtf_preview <- if (nzchar(bundle$gtf_file)) data.frame(path = bundle$gtf_file, stringsAsFactors = FALSE) else data.frame(note = "No GTF specified", stringsAsFactors = FALSE)
        state$status_message <- paste0("Loaded results directory: ", bundle$results_dir)
        shiny::showNotification("Results loaded successfully.", type = "message")
      }, error = function(e) {
        state$status_message <- conditionMessage(e)
        shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$validate_inputs, {
      bam_check <- validate_bam_inputs(input$bam_dir, input$sample_sheet, include_batch = isTRUE(app_state$use_batch))
      gtf_check <- validate_gtf_file(input$gtf_file)
      state$metadata <- bam_check$metadata
      if (!is.null(state$metadata)) state$selected_rows <- seq_len(nrow(state$metadata))
      state$bam_preview <- bam_check$bam_table
      state$input_validated <- FALSE
      state$validated_run_request <- NULL

      if (!isTRUE(bam_check$ok)) {
        state$status_message <- bam_check$message
        shiny::showNotification(bam_check$message, type = "error", duration = 8)
        return()
      }
      if (!isTRUE(gtf_check$ok)) {
        state$status_message <- gtf_check$message
        shiny::showNotification(gtf_check$message, type = "error", duration = 8)
        return()
      }

      state$gtf_preview <- data.frame(
        field = c("path", "size", "compressed"),
        value = c(gtf_check$info$path, as.character(gtf_check$info$size), ifelse(gtf_check$info$compressed, "yes", "no")),
        stringsAsFactors = FALSE
      )
      state$validated_run_request <- list(
        bam_dir = normalizePath(input$bam_dir, winslash = "/", mustWork = FALSE),
        gtf_file = normalizePath(input$gtf_file, winslash = "/", mustWork = FALSE),
        sample_sheet = normalizePath(input$sample_sheet, winslash = "/", mustWork = FALSE),
        output_dir = normalizePath(input$output_dir, winslash = "/", mustWork = FALSE)
      )
      state$input_validated <- TRUE
      state$status_message <- "Input validation passed. Confirm sample selection and click Start Analysis."
      shiny::showNotification("Input validation passed.", type = "message")
    })

    shiny::observeEvent(input$start_analysis, {
      can_start <- !is.null(state$validated_run_request) && length(selected_sample_ids_r()) >= 2
      if (!isTRUE(can_start)) {
        shiny::showNotification("Validate inputs first and select at least 2 samples.", type = "error", duration = 6)
        return()
      }
      state$start_analysis_counter <- state$start_analysis_counter + 1L
    })

    shiny::observeEvent(input$refresh_pca, {
      if (length(selected_sample_ids_r()) < 2) {
        shiny::showNotification("Select at least 2 samples before refreshing PCA.", type = "error", duration = 6)
        return()
      }
      state$refresh_pca_counter <- state$refresh_pca_counter + 1L
    })

    output$status <- shiny::renderUI({
      shiny::p(class = "help-text", state$status_message)
    })

    mod_preview_server("bam_preview", shiny::reactive(state$bam_preview))
    mod_preview_server("gtf_preview", shiny::reactive(state$gtf_preview))

    list(
      mode = shiny::reactive(input$mode),
      validated_run_request = shiny::reactive(state$validated_run_request),
      sample_metadata = shiny::reactive(state$metadata),
      selected_sample_ids = selected_sample_ids_r,
      results_loaded = shiny::reactive(state$results_loaded),
      start_analysis_counter = shiny::reactive(state$start_analysis_counter),
      refresh_pca_counter = shiny::reactive(state$refresh_pca_counter)
    )
  })
}
