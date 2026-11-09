library(shiny)

# runApp("app") sets the working directory to app/. Tests source this file
# from the project root instead.
project_root <- if (file.exists("R/data.R")) "." else ".."
source(file.path(project_root, "R", "data.R"), local = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2)

empty_message <- function(title, message) {
  div(class = "empty-state", div(class = "empty-symbol", "↗"),
      h3(title), p(message))
}
panel_header <- function(title, subtitle = NULL, extra = NULL) {
  div(class = "panel-header", div(h2(class = "panel-title", title),
      if (!is.null(subtitle)) p(class = "panel-subtitle", subtitle)), extra)
}
metric <- function(label, value, note = NULL) {
  div(class = "metric-card", div(class = "metric-label", label),
      div(class = "metric-value", value), if (!is.null(note)) div(class = "metric-note", note))
}
format_number <- function(x) {
  if (!length(x) || is.na(x)) "—" else format(round(x, 2), big.mark = ",", trim = TRUE)
}

ui <- fluidPage(
  title = "Statistical Analysis Hub",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
            tags$script(src = "app.js", defer = NA),
            tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
  div(class = "app-shell",
    tags$aside(class = "sidebar",
      div(class = "brand", div(class = "brand-name", "Analysis Hub")),
      tags$nav(class = "nav-list", `aria-label` = "Workspace navigation",
        actionLink("nav_data", label = "Data", class = "nav-item"),
        actionLink("nav_groups", label = "Nested groups", class = "nav-item"),
        actionLink("nav_stats", label = "Summary", class = "nav-item")),
      div(class = "sidebar-footer", "Statistical Analysis Hub", tags$br(),
          tags$a(href = "https://github.com/arkieft735/statistical-analysis-hub", target = "_blank", rel = "noopener noreferrer", "View source ↗"))),
    tags$main(class = "workspace",
      div(class = "topbar", div(class = "breadcrumb", "Workspace", span(class = "separator", "/"), strong("Data"))),
      div(class = "page-heading", h1(class = "page-title", "Explore your data"),
        p(class = "page-description", "Import a dataset, inspect its columns, and explore nested groups.")),
      div(class = "import-layout",
        div(class = "panel import-card",
          panel_header("Import data"),
          div(class = "import-body",
            div(class = "upload-zone",
                fileInput("file", label = "Data file", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".json", ".jsonl", ".ndjson", ".rds"),
                          buttonLabel = "Choose file", placeholder = "No file selected"),
                p("CSV, TSV, TXT, Excel, JSON, JSON Lines, or RDS")),
            uiOutput("import_options"),
            p(class = "hint", "Up to 50 MB, 100,000 rows, and 200 columns."),
            actionButton("import", "Import data", class = "btn-primary"),
            uiOutput("import_status"),
            div(class = "sample-callout",
                actionButton("sample", "Use sample data", class = "btn-secondary"),
                p(class = "hint", "36 students · 6 classes · 3 schools")),
            downloadButton("sample_download", "Download sample CSV", class = "btn-link"))),
        div(class = "data-workspace",
          uiOutput("metrics"),
          div(class = "panel preview-panel",
            panel_header(uiOutput("dataset_title", inline = TRUE), extra = uiOutput("dataset_badge", inline = TRUE)),
            div(class = "section-tabs",
              tabsetPanel(id = "view", type = "tabs",
                tabPanel("Data preview", value = "data",
                  div(class = "preview-toolbar", textInput("search", "Filter rows", placeholder = "Search across all columns…"),
                      selectInput("page_size", "Rows per page", c(10, 25, 50), 10)),
                  div(class = "table-wrap", tableOutput("preview")),
                  div(class = "table-footer", uiOutput("preview_caption"),
                      div(class = "pagination-controls", actionButton("previous", "← Previous", class = "btn-secondary"),
                          actionButton("next_page", "Next →", class = "btn-secondary"))),
                  div(class = "panel-body", downloadButton("download", "Export imported data", class = "btn-secondary"))),
                tabPanel("Nested groups", value = "groups",
                  div(class = "panel-body",
                    div(class = "hierarchy-controls",
                      selectizeInput("groups", "Build your hierarchy", choices = NULL, multiple = TRUE,
                        options = list(plugins = list("remove_button"), maxItems = 8L, placeholder = "Choose outer group, then inner groups…")),
                      p(class = "hint", "Select up to 8 columns from outermost to innermost. Remove and reselect a column to change its order. Repeated class names remain separate within each parent.")),
                    uiOutput("hierarchy_path"), uiOutput("hierarchy"),
                    downloadButton("groups_download", "Export group counts", class = "btn-secondary"))),
                tabPanel("Summary", value = "stats",
                  div(class = "panel-body",
                    selectInput("variable", "Numeric variable", choices = NULL),
                    uiOutput("statistics"),
                    div(class = "chart-panel", h3(class = "panel-title", "Distribution"),
                        plotOutput("distribution", height = "255px")),
                    p(class = "hint", "Descriptive summaries use finite observations. Missing and non-finite values are excluded. These summaries do not adjust for clustering or imply independent observations."))))),
            div(class = "footer-note", "Your source file stays unchanged. Data is held in this Shiny session and is cleared when the session ends."))))))
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  status <- reactiveVal(NULL)
  page <- reactiveVal(1L)
  file_extension <- reactive({
    if (is.null(input$file)) "csv" else tolower(tools::file_ext(input$file$name))
  })
  output$import_options <- renderUI({
    ext <- file_extension()
    if (ext == "rds") {
      return(p(class = "hint", "RDS files must contain a data frame or tibble. Column types are preserved."))
    }
    if (ext %in% c("json", "jsonl", "ndjson")) {
      return(p(class = "hint", "Use an array of records for JSON, or one record per line for JSON Lines. Nested object fields become columns with dotted names."))
    }
    excel <- ext %in% c("xlsx", "xls")
    sheet_control <- if (excel) {
      tryCatch({
        sheets <- hub_excel_sheets(input$file$datapath)
        selectInput("sheet", "Worksheet", choices = sheets, selected = head(sheets, 1L))
      }, error = function(e) div(class = "notification error", role = "alert", conditionMessage(e)))
    }
    tagList(
      sheet_control,
      if (!excel) div(class = "field-grid",
        selectInput("delimiter", "Delimiter", c("Comma (,)" = ",", "Tab" = "\t", "Semicolon (;)" = ";", "Pipe (|)" = "|"), selected = if (ext == "tsv") "\t" else ","),
        selectInput("decimal", "Decimal mark", c("Period (.)" = ".", "Comma (,)" = ","))),
      checkboxInput("header", "First row contains column names", TRUE),
      textInput("na_values", "Missing-value markers", "NA, N/A, NULL"),
      p(class = "hint", "Separate markers with commas. Empty cells are treated as missing."))
  })

  set_dataset <- function(data, name, demo = FALSE) {
    dataset(list(data = data, name = name, demo = demo))
    page(1L)
    updateTextInput(session, "search", value = "")
    numeric_columns <- data_profile(data)$numeric_columns
    updateSelectInput(session, "variable", choices = numeric_columns,
                      selected = if ("score" %in% numeric_columns) "score" else head(numeric_columns, 1))
    defaults <- intersect(c("school", "class"), names(data))
    updateSelectizeInput(session, "groups", choices = names(data), selected = defaults, server = TRUE)
  }
  load_sample <- function() {
    set_dataset(read_hub_data(file.path(project_root, "examples", "classroom-study.csv")), "classroom-study.csv", TRUE)
    status(NULL)
  }
  load_sample()
  observeEvent(input$sample, load_sample())
  observeEvent(input$file, {
    status(list(type = "info", text = "File selected. Check the options, then select Import data."))
  })
  observeEvent(input$import, {
    if (is.null(input$file)) {
      status(list(type = "error", text = "Choose a data file first."))
      return()
    }
    tryCatch({
      ext <- tolower(tools::file_ext(input$file$name))
      if (!ext %in% c("csv", "tsv", "txt", "xlsx", "xls", "json", "jsonl", "ndjson", "rds")) {
        stop("Choose a CSV, TSV, TXT, Excel, JSON, JSON Lines, or RDS file.", call. = FALSE)
      }
      markers <- if (is.null(input$na_values)) c("", "NA", "N/A", "NULL") else unique(c("", trimws(strsplit(input$na_values, ",", fixed = TRUE)[[1]])))
      data <- read_hub_file(input$file$datapath, filename = input$file$name,
                            delimiter = if (is.null(input$delimiter)) "," else input$delimiter,
                            header = if (is.null(input$header)) TRUE else isTRUE(input$header), na_strings = markers,
                            decimal = if (is.null(input$decimal)) "." else input$decimal,
                            sheet = if (is.null(input$sheet) || !nzchar(input$sheet)) 1L else input$sheet)
      set_dataset(data, input$file$name)
      status(list(type = "success", text = sprintf("Imported %s rows successfully.", format(nrow(data), big.mark = ","))))
    }, error = function(e) status(list(type = "error", text = conditionMessage(e))))
  })
  observeEvent(input$nav_data, updateTabsetPanel(session, "view", selected = "data"))
  observeEvent(input$nav_groups, updateTabsetPanel(session, "view", selected = "groups"))
  observeEvent(input$nav_stats, updateTabsetPanel(session, "view", selected = "stats"))

  current_data <- reactive({ req(dataset()); dataset()$data })
  profile <- reactive(data_profile(current_data()))
  output$dataset_title <- renderUI(span(dataset()$name))
  output$dataset_badge <- renderUI(span(class = "dataset-source", if (dataset()$demo) "Sample data" else "Imported"))
  output$import_status <- renderUI({
    message <- status()
    if (is.null(message)) return(NULL)
    div(class = paste("notification", message$type), role = if (message$type == "error") "alert" else "status", message$text)
  })
  output$metrics <- renderUI({
    p <- profile()
    div(class = "metrics-grid", metric("Rows", format(p$rows, big.mark = ",")),
      metric("Columns", p$columns), metric("Missing / non-finite", format(p$missing, big.mark = ",")))
  })
  filtered_data <- reactive({
    data <- current_data()
    query <- trimws(if (is.null(input$search)) "" else input$search)
    if (!nzchar(query)) return(data)
    matches <- Reduce(`|`, lapply(data, function(x) {
      result <- grepl(tolower(query), tolower(as.character(x)), fixed = TRUE)
      result[is.na(x)] <- FALSE
      result
    }))
    data[matches, , drop = FALSE]
  })
  page_size <- reactive({ if (is.null(input$page_size)) 10L else as.integer(input$page_size) })
  total_pages <- reactive(max(1L, ceiling(nrow(filtered_data()) / page_size())))
  observeEvent(list(input$search, input$page_size), page(1L))
  observeEvent(input$previous, page(max(1L, page() - 1L)))
  observeEvent(input$next_page, page(min(total_pages(), page() + 1L)))
  preview_data <- reactive({
    data <- filtered_data()
    start <- (min(page(), total_pages()) - 1L) * page_size() + 1L
    if (!nrow(data)) return(data)
    data[seq.int(start, min(nrow(data), start + page_size() - 1L)), , drop = FALSE]
  })
  output$preview <- renderTable(preview_data(), striped = FALSE, bordered = FALSE, spacing = "m", rownames = FALSE, width = "100%",
                                na = "—", sanitize.text.function = htmltools::htmlEscape)
  output$preview_caption <- renderUI({
    count <- nrow(filtered_data())
    start <- if (count) (min(page(), total_pages()) - 1L) * page_size() + 1L else 0L
    span(class = "preview-caption", if (!count) "No matching rows" else sprintf("Showing %s–%s of %s rows · Page %s of %s", start,
      min(count, start + page_size() - 1L), format(count, big.mark = ","), min(page(), total_pages()), total_pages()))
  })
  grouped <- reactive({
    columns <- if (is.null(input$groups)) character() else intersect(input$groups, names(current_data()))
    tryCatch(hierarchy_summary(current_data(), columns),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$hierarchy_path <- renderUI({
    if (!length(input$groups)) return(NULL)
    div(class = "hierarchy-path", lapply(seq_along(input$groups), function(i) span(class = "hierarchy-chip", paste(i, input$groups[i]), if (i < length(input$groups)) " → ")))
  })
  output$hierarchy <- renderUI({
    if (!length(input$groups)) return(empty_message("Give your data a little structure", "Choose grouping columns above, starting with the outermost level."))
    groups <- grouped()
    shown <- head(groups, 300L)
    # Bound the DOM while keeping the full hierarchy available in the export.
    children <- split(seq_len(nrow(shown)), shown$parent_id)
    render_nodes <- function(parent = "") {
      key <- match(parent, names(children))
      indices <- if (is.na(key)) integer() else children[[key]]
      tagList(lapply(indices, function(i) {
        heading <- tagList(span(class = "hierarchy-label", shown$label[i]), span(class = "hierarchy-count", paste(shown$rows[i], if (shown$rows[i] == 1L) "row" else "rows")))
        if (shown$id[i] %in% shown$parent_id) {
          tags$details(class = "hierarchy-node", open = if (shown$depth[i] == 1L) "open" else NULL,
            tags$summary(class = "hierarchy-summary", heading), div(class = "hierarchy-children", render_nodes(shown$id[i])))
        } else div(class = "hierarchy-node hierarchy-leaf", div(class = "hierarchy-summary", heading))
      }))
    }
    div(class = "hierarchy-tree", p(class = "hint", sprintf("%s groups across %s levels. Counts include every imported row.", nrow(groups), length(input$groups))),
        render_nodes(), if (nrow(groups) > 300L) p(class = "hint", "Showing the first 300 groups. Export group counts for the complete hierarchy."))
  })
  output$statistics <- renderUI({
    if (!length(profile()$numeric_columns)) return(empty_message("No numeric columns yet", "Import a dataset with numeric observations to see a summary and distribution."))
    req(input$variable %in% names(current_data()))
    stats <- numeric_summary(current_data(), input$variable)
    div(class = "statistics-grid", metric("Mean", format_number(stats$mean), paste(stats$count, "finite observations")),
        metric("Median", format_number(stats$median), "Middle observation"),
        metric("Standard deviation", format_number(stats$sd), "Sample standard deviation"),
        metric("Range", paste(format_number(stats$min), "–", format_number(stats$max)), paste(stats$missing, "missing / non-finite")))
  })
  output$distribution <- renderPlot({
    validate(need(length(profile()$numeric_columns), "Import numeric data to see its distribution."))
    req(input$variable %in% names(current_data()))
    values <- current_data()[[input$variable]]
    values <- values[is.finite(values)]
    validate(need(length(values), "This column has no finite observations."))
    par(mar = c(4, 4, 1, 1), fg = "#73726c", col.axis = "#73726c", col.lab = "#73726c", family = "sans", bg = "#faf9f5")
    hist(values, breaks = "Sturges", col = "#d97757", border = "#faf9f5", main = "", xlab = input$variable, ylab = "Observations")
  }, res = 110, alt = "Histogram of the selected numeric variable")
  output$download <- downloadHandler(filename = function() "imported-data.csv", content = function(file) {
    utils::write.csv(current_data(), file, row.names = FALSE, na = "")
  })
  output$groups_download <- downloadHandler(filename = function() "nested-group-counts.csv", content = function(file) {
    utils::write.csv(grouped(), file, row.names = FALSE, na = "")
  })
  output$sample_download <- downloadHandler(filename = function() "classroom-study.csv", content = function(file) {
    file.copy(file.path(project_root, "examples", "classroom-study.csv"), file)
  })
}


