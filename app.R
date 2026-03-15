required_packages <- c("shiny", "DT", "plotly", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages:",
    paste(missing_packages, collapse = ", "),
    "\nRun: Rscript scripts/install_deps.R",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(plotly)
  library(ggplot2)
})

source("scripts/common.R")
module_files <- c(
  "R/helpers_validation.R",
  "R/helpers_pipeline.R",
  "R/helpers_plots.R",
  "R/mod_preview.R",
  "R/mod_data_source.R",
  "R/mod_run_status.R",
  "R/mod_pca.R",
  "R/mod_analysis_setup.R",
  "R/mod_volcano.R",
  "R/mod_heatmap.R",
  "R/mod_go.R",
  "R/mod_results_export.R"
)
for (file in module_files) {
  if (!file.exists(file)) {
    stop("Missing required module file: ", file, call. = FALSE)
  }
  source(file, local = FALSE)
}

build_analysis_summary <- function(bundle, analysis, settings) {
  list(
    mode = bundle$source_mode %||% "unknown",
    result_path = bundle$results_dir %||% "",
    samples = if (!is.null(analysis$metadata)) nrow(analysis$metadata) else 0L,
    tested_genes = if (!is.null(analysis$annotated)) nrow(analysis$annotated) else 0L,
    significant_genes = if (!is.null(analysis$significant)) nrow(analysis$significant) else 0L,
    go_terms = if (!is.null(analysis$go)) nrow(analysis$go) else 0L,
    design_formula = settings$design_formula %||% "",
    factor = settings$contrast_factor %||% "",
    contrast = paste(settings$experiment_group %||% "", "vs", settings$control_group %||% ""),
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

ui <- navbarPage(
  title = "Bulk RNA-seq",
  id = "main_nav",
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    div(class = "global-status-wrap", uiOutput("global_status"))
  ),
  tabPanel("Data Source", fluidPage(mod_data_source_ui("data_source"))),
  tabPanel("Run Status", fluidPage(mod_run_status_ui("run_status"))),
  tabPanel("PCA", fluidPage(mod_pca_ui("pca"))),
  tabPanel("Analysis Setup", fluidPage(mod_analysis_setup_ui("analysis_setup"))),
  tabPanel("Volcano", fluidPage(mod_volcano_ui("volcano"))),
  tabPanel("Heatmap", fluidPage(mod_heatmap_ui("heatmap"))),
  tabPanel("GO Enrichment", fluidPage(mod_go_ui("go"))),
  tabPanel("Export", fluidPage(mod_results_export_ui("results_export")))
)

server <- function(input, output, session) {
  app_state <- reactiveValues(
    current_bundle = NULL,
    current_bundle_token = 0L,
    current_pca_analysis = NULL,
    current_contrast_analysis = NULL,
    current_summary = NULL,
    current_selected_samples = character(0),
    current_pca_sample_ids = character(0),
    pca_view_mode = "all",
    pca_color_var = NULL,
    pca_shape_var = NULL,
    sample_selection_dirty = FALSE,
    logs = character(0),
    running = FALSE,
    status_text = "Waiting for action",
    status_detail = "Load existing results or validate inputs and start a new analysis.",
    status_level = "info",
    use_batch = FALSE
  )

  append_log <- function(text) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    app_state$logs <- c(app_state$logs, paste0("[", timestamp, "] ", text))
  }

  data_source <- mod_data_source_server("data_source", app_state)

  metadata_r <- reactive({
    metadata <- data_source$sample_metadata()
    if (!is.null(metadata) && nrow(metadata) > 0) metadata else NULL
  })

  analysis_setup <- mod_analysis_setup_server("analysis_setup", metadata_r, app_state)
  mod_run_status_server("run_status", data_source, reactive(analysis_setup$settings()), app_state)

  observe({
    selected_ids <- unique(as.character(data_source$selected_sample_ids()))
    app_state$current_selected_samples <- selected_ids
    available_ids <- if (!is.null(app_state$current_bundle$metadata_all)) app_state$current_bundle$metadata_all$sample_id else if (!is.null(app_state$current_bundle)) app_state$current_bundle$metadata$sample_id else character(0)
    if (!is.null(app_state$current_bundle) && length(app_state$current_pca_sample_ids) > 0) {
      app_state$sample_selection_dirty <- !setequal(selected_ids, app_state$current_pca_sample_ids)
      if (!all(selected_ids %in% available_ids)) {
        app_state$sample_selection_dirty <- TRUE
      }
      if (isTRUE(app_state$sample_selection_dirty)) {
        app_state$current_contrast_analysis <- NULL
      }
    }
  })

  observeEvent(app_state$current_bundle_token, {
    req(!is.null(app_state$current_bundle))
    selected_ids <- data_source$selected_sample_ids()
    app_state$current_contrast_analysis <- NULL
    app_state$current_summary <- NULL
    app_state$status_level <- "running"
    app_state$status_text <- "Generating PCA for selected samples"
    app_state$status_detail <- "Preparing PCA view from the currently selected samples."
    append_log(paste0("Loaded results directory: ", app_state$current_bundle$results_dir))

    withProgress(message = "Generating PCA", value = 0, {
      incProgress(0.3, detail = "Preparing selected sample subset")
      pca_analysis <- compute_selected_pca_analysis(app_state$current_bundle, selected_ids)
      incProgress(0.9, detail = "PCA computation complete")
      app_state$current_pca_analysis <- pca_analysis
      app_state$current_pca_sample_ids <- selected_ids
      app_state$sample_selection_dirty <- FALSE
    })

    app_state$status_level <- "success"
    app_state$status_text <- "PCA generated"
    app_state$status_detail <- "Review PCA first. If sample selection changes, refresh PCA before running contrast analysis."
    append_log("PCA generation completed.")
  }, ignoreInit = TRUE)

  observeEvent(data_source$refresh_pca_counter(), {
    req(!is.null(app_state$current_bundle))
    selected_ids <- data_source$selected_sample_ids()
    available_ids <- if (!is.null(app_state$current_bundle$metadata_all)) app_state$current_bundle$metadata_all$sample_id else app_state$current_bundle$metadata$sample_id

    if (!all(selected_ids %in% available_ids)) {
      showNotification("Current selection contains samples outside the loaded bundle. Please run a new analysis.", type = "error", duration = 8)
      return()
    }
    if (length(selected_ids) < 2) {
      showNotification("Select at least 2 samples before refreshing PCA.", type = "error", duration = 8)
      return()
    }
    if (setequal(selected_ids, app_state$current_pca_sample_ids)) {
      app_state$status_level <- "info"
      app_state$status_text <- "Sample selection unchanged"
      app_state$status_detail <- "Current PCA is still valid; no recalculation needed."
      showNotification("Sample selection unchanged; skipped PCA recalculation.", type = "message", duration = 5)
      return()
    }

    app_state$status_level <- "running"
    app_state$status_text <- "Recomputing PCA"
    app_state$status_detail <- "Sample selection changed. Recomputing PCA before contrast analysis."
    append_log("Sample selection changed; recomputing PCA.")

    withProgress(message = "Recomputing PCA", value = 0, {
      incProgress(0.3, detail = "Applying current sample selection")
      pca_analysis <- compute_selected_pca_analysis(app_state$current_bundle, selected_ids)
      incProgress(0.9, detail = "PCA computation complete")
      app_state$current_pca_analysis <- pca_analysis
      app_state$current_pca_sample_ids <- selected_ids
      app_state$sample_selection_dirty <- FALSE
      app_state$current_contrast_analysis <- NULL
      app_state$current_summary <- NULL
    })

    app_state$status_level <- "success"
    app_state$status_text <- "PCA updated"
    app_state$status_detail <- "Please review PCA, then proceed to 1v1 contrast analysis."
    append_log("PCA updated.")
  }, ignoreInit = TRUE)

  observeEvent(data_source$start_analysis_counter(), {
    req(identical(data_source$mode(), "run_analysis"))
    request <- data_source$validated_run_request()
    req(!is.null(request))
    selected_ids <- data_source$selected_sample_ids()
    if (length(selected_ids) < 2) {
      showNotification("Select at least 2 samples before starting analysis.", type = "error", duration = 8)
      return()
    }

    app_state$running <- TRUE
    app_state$status_level <- "running"
    app_state$status_text <- "Running new analysis"
    app_state$status_detail <- "Generating counts, normalized matrix, and all-sample PCA for selected samples."
    app_state$logs <- character(0)
    append_log("Started new analysis run.")
    append_log(paste0("Output directory: ", normalizePath(request$output_dir, winslash = "/", mustWork = FALSE)))

    tryCatch({
      withProgress(message = "Running new analysis", value = 0, {
        bundle <- run_precompute_pipeline(
          bam_dir = request$bam_dir,
          gtf_file = request$gtf_file,
          sample_sheet = request$sample_sheet,
          output_dir = request$output_dir,
          selected_sample_ids = selected_ids,
          log_callback = append_log,
          progress_callback = function(value, detail) {
            setProgress(value = value, detail = detail)
            app_state$status_text <- "Running new analysis"
            app_state$status_detail <- detail
          }
        )
        app_state$current_bundle <- bundle
        app_state$current_bundle_token <- app_state$current_bundle_token + 1L
        app_state$current_selected_samples <- selected_ids
        app_state$current_pca_sample_ids <- selected_ids
        app_state$current_pca_analysis <- list(
          metadata = bundle$metadata,
          normalized = bundle$normalized,
          pca = bundle$pca,
          pca_kind = "all_samples"
        )
        app_state$current_contrast_analysis <- NULL
        app_state$current_summary <- list(
          mode = "run_analysis",
          result_path = bundle$results_dir,
          samples = nrow(bundle$metadata),
          tested_genes = 0L,
          significant_genes = 0L,
          go_terms = 0L,
          design_formula = "1v1 contrast not started",
          factor = "",
          contrast = "1v1 contrast not started",
          generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
        app_state$sample_selection_dirty <- FALSE
      })
      app_state$status_level <- "success"
      app_state$status_text <- "New analysis completed"
      app_state$status_detail <- "All-sample PCA has been generated. Review PCA before running 1v1 contrast analysis."
      append_log("New analysis completed; all-sample PCA is ready.")
      updateNavbarPage(session, "main_nav", selected = "PCA")
    }, error = function(e) {
      app_state$status_level <- "error"
      app_state$status_text <- "New analysis failed"
      app_state$status_detail <- conditionMessage(e)
      append_log(paste0("New analysis failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error", duration = 10)
    })

    app_state$running <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(analysis_setup$apply_counter(), {
    req(!is.null(app_state$current_bundle))
    settings <- analysis_setup$settings()
    selected_ids <- app_state$current_selected_samples

    app_state$running <- TRUE
    app_state$status_level <- "running"
    app_state$status_text <- "Running 1v1 contrast analysis"
    app_state$status_detail <- "Building PCA, volcano, heatmap, and GO results from current selection and contrast settings."
    append_log(paste0("Started 1v1 contrast: ", settings$experiment_group, " vs ", settings$control_group, " (factor: ", settings$contrast_factor, ")"))

    tryCatch({
      withProgress(message = "Running 1v1 contrast analysis", value = 0, {
        analysis <- run_contrast_analysis(
          app_state$current_bundle,
          selected_ids,
          settings,
          log_callback = append_log,
          progress_callback = function(value, detail) {
            setProgress(value = value, detail = detail)
            app_state$status_text <- "Running 1v1 contrast analysis"
            app_state$status_detail <- detail
          }
        )
        app_state$current_contrast_analysis <- analysis
        app_state$current_summary <- build_analysis_summary(app_state$current_bundle, analysis, settings)
      })
      app_state$status_level <- "success"
      app_state$status_text <- "1v1 contrast analysis completed"
      app_state$status_detail <- "You can now review volcano, heatmap, GO enrichment, and contrast-specific PCA."
      append_log("1v1 contrast analysis completed.")
    }, error = function(e) {
      app_state$current_contrast_analysis <- NULL
      app_state$current_summary <- NULL
      app_state$status_level <- "error"
      app_state$status_text <- "1v1 contrast analysis failed"
      app_state$status_detail <- conditionMessage(e)
      append_log(paste0("1v1 contrast analysis failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error", duration = 10)
    })

    app_state$running <- FALSE
  }, ignoreInit = TRUE)

  output$global_status <- renderUI({
    status_class <- switch(
      app_state$status_level %||% "info",
      running = "status-running",
      success = "status-success",
      error = "status-error",
      "status-info"
    )
    div(
      class = paste("global-status", status_class),
      div(class = "global-status-title", app_state$status_text %||% "Waiting"),
      div(class = "global-status-detail", app_state$status_detail %||% "")
    )
  })

  pca_analysis_r <- reactive(app_state$current_pca_analysis)
  contrast_analysis_r <- reactive(app_state$current_contrast_analysis)

  mod_pca_server("pca", pca_analysis_r, contrast_analysis_r, app_state)
  mod_volcano_server("volcano", contrast_analysis_r, reactive(analysis_setup$settings()))
  mod_heatmap_server("heatmap", contrast_analysis_r, reactive(analysis_setup$settings()))
  mod_go_server("go", contrast_analysis_r)
  mod_results_export_server("results_export", contrast_analysis_r, pca_analysis_r, reactive(analysis_setup$settings()), app_state)
}

shinyApp(ui, server)

