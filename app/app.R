# ------------------------------------------------------------
# 1. LOAD PACKAGES AND SUPPORTING FILES
# ------------------------------------------------------------

# Load Shiny, the R package used to create web applications.
library(shiny)

# Determine where the project is located.
# When running runApp("app"), the working directory is app/.
# When this file is sourced from the project root (for testing),
# the working directory is the root folder.
project_root <- if (file.exists("R/data.R")) "." else ".."

# Load the functions used to import and process datasets.
# These functions are stored separately in R/data.R.
source(file.path(project_root, "R", "data.R"), local = TRUE)

# Set the maximum file upload size to 50 MB.
options(shiny.maxRequestSize = 50 * 1024^2)


# ------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ------------------------------------------------------------

# Creates a message displayed when a section has no content.
# title = the heading of the message.
# message = the explanatory text underneath.
empty_message <- function(title, message) {
  div(class = "empty-state", div(class = "empty-symbol", "↗"),
      h3(title), p(message))
}

# Creates a standardized heading for a dashboard panel.
# title = main heading.
# subtitle = optional smaller description.
# extra = optional additional content, such as a badge.
panel_header <- function(title, subtitle = NULL, extra = NULL) {
  div(class = "panel-header", div(h2(class = "panel-title", title),
                                  if (!is.null(subtitle)) p(class = "panel-subtitle", subtitle)), extra)
}

# Creates a metric card.
# These cards display statistics such as the number of rows,
# columns, means, medians, and standard deviations.
metric <- function(label, value, note = NULL) {
  div(class = "metric-card", div(class = "metric-label", label),
      div(class = "metric-value", value), if (!is.null(note)) div(class = "metric-note", note))
}

# Formats numeric values for display.
# Rounds to two decimal places and adds commas.
# If the value is missing, displays a dash.
format_number <- function(x) {
  if (!length(x) || is.na(x)) "—" else format(round(x, 2), big.mark = ",", trim = TRUE)
}


# ============================================================
# 3. USER INTERFACE (UI)
# ============================================================

# The UI determines what the user sees in the browser.
# fluidPage() creates a responsive page that adjusts to screen size.

ui <- fluidPage(
  
  # Sets the browser page title.
  title = "Statistical Analysis Hub",
  
  # HEAD SECTION:
  # Loads external styling and JavaScript.
  # tags$head() inserts information into the webpage's HTML head.
  tags$head(
    
    # Loads the custom CSS stylesheet.
    # CSS controls colors, fonts, spacing, layout, etc.
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    # Loads the JavaScript file.
    # JavaScript handles additional browser-side interactions.
    tags$script(src = "app.js", defer = NA),
    
    # Makes the dashboard scale appropriately on mobile devices.
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  # MAIN APPLICATION CONTAINER:
  # div() creates an HTML container.
  # class = assigns a CSS class that controls its appearance.
  div(class = "app-shell",
      
      # --------------------------------------------------------
      # SIDEBAR
      # --------------------------------------------------------
      
      # tags$aside() creates the sidebar portion of the webpage.
      tags$aside(class = "sidebar",
                 
                 # Dashboard branding.
                 div(class = "brand", div(class = "brand-name", "Analysis Hub")),
                 
                 # NAVIGATION:
                 # tags$nav() creates a navigation section.
                 # actionLink() creates clickable links that Shiny can detect.
                 tags$nav(class = "nav-list", `aria-label` = "Workspace navigation",
                          
                          actionLink("nav_data", label = "Data", class = "nav-item"),
                          
                          actionLink("nav_groups", label = "Nested groups", class = "nav-item"),
                          
                          actionLink("nav_stats", label = "Summary", class = "nav-item")
                 ),
                 
                 # Bottom section of the sidebar.
                 div(class = "sidebar-footer", "Statistical Analysis Hub", tags$br(),
                     
                     # Link to the project's GitHub repository.
                     tags$a(
                       href = "https://github.com/arkieft735/statistical-analysis-hub",
                       target = "_blank",
                       rel = "noopener noreferrer",
                       "View source ↗"
                     )
                 )
      ),
      
      # --------------------------------------------------------
      # MAIN WORKSPACE
      # --------------------------------------------------------
      
      # tags$main() creates the main content area.
      tags$main(class = "workspace",
                
                # TOP NAVIGATION BAR:
                # Displays the current location within the application.
                div(class = "topbar",
                    div(class = "breadcrumb", "Workspace",
                        span(class = "separator", "/"),
                        strong("Data"))),
                
                # PAGE HEADING:
                # The main title and description displayed above the content.
                div(class = "page-heading",
                    h1(class = "page-title", "Explore your data"),
                    p(class = "page-description",
                      "Import a dataset, inspect its columns, and explore nested groups.")
                ),
                
                # ------------------------------------------------------
                # IMPORT AND DATA WORKSPACE LAYOUT
                # ------------------------------------------------------
                
                # The import-layout container holds the upload panel
                # and the main data workspace side by side.
                div(class = "import-layout",
                    
                    # ----------------------------------------------------
                    # DATA IMPORT PANEL
                    # ----------------------------------------------------
                    
                    div(class = "panel import-card",
                        
                        # Panel heading.
                        panel_header("Import data"),
                        
                        div(class = "import-body",
                            
                            # FILE UPLOAD:
                            # fileInput() allows users to select a file.
                            # The accepted formats are listed in accept.
                            div(class = "upload-zone",
                                
                                fileInput(
                                  "file",
                                  label = "Data file",
                                  accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls",
                                             ".json", ".jsonl", ".ndjson", ".rds"),
                                  buttonLabel = "Choose file",
                                  placeholder = "No file selected"
                                ),
                                
                                p("CSV, TSV, TXT, Excel, JSON, JSON Lines, or RDS")
                            ),
                            
                            # uiOutput() creates a location where the server
                            # can dynamically insert additional UI elements.
                            uiOutput("import_options"),
                            
                            p(class = "hint", "Up to 50 MB, 100,000 rows, and 200 columns."),
                            
                            # Button that tells the server to import the selected file.
                            actionButton("import", "Import data", class = "btn-primary"),
                            
                            # Displays import success or error messages.
                            uiOutput("import_status"),
                            
                            # OPTION TO LOAD SAMPLE DATA:
                            div(class = "sample-callout",
                                
                                actionButton("sample", "Use sample data", class = "btn-secondary"),
                                
                                p(class = "hint", "36 students · 6 classes · 3 schools")
                            ),
                            
                            # Allows users to download the example dataset.
                            downloadButton("sample_download", "Download sample CSV", class = "btn-link")
                        )
                    ),
                    
                    # ----------------------------------------------------
                    # DATA WORKSPACE
                    # ----------------------------------------------------
                    
                    div(class = "data-workspace",
                        
                        # Displays summary metrics about the imported dataset.
                        uiOutput("metrics"),
                        
                        # Main panel containing the dataset preview and other tabs.
                        div(class = "panel preview-panel",
                            
                            # The dataset title and source badge are generated dynamically.
                            panel_header(
                              uiOutput("dataset_title", inline = TRUE),
                              extra = uiOutput("dataset_badge", inline = TRUE)
                            ),
                            
                            # TAB NAVIGATION:
                            # tabsetPanel() creates multiple views within the same panel.
                            # The user can switch between Data preview, Nested groups,
                            # and Summary without leaving the application.
                            div(class = "section-tabs",
                                
                                tabsetPanel(id = "view", type = "tabs",
                                            
                                            # ------------------------------------------------
                                            # TAB 1: DATA PREVIEW
                                            # ------------------------------------------------
                                            
                                            tabPanel("Data preview", value = "data",
                                                     
                                                     # Controls above the dataset table.
                                                     div(class = "preview-toolbar",
                                                         
                                                         # Search field for filtering dataset rows.
                                                         textInput(
                                                           "search",
                                                           "Filter rows",
                                                           placeholder = "Search across all columns…"
                                                         ),
                                                         
                                                         # Allows the user to select how many rows to display.
                                                         selectInput("page_size", "Rows per page", c(10, 25, 50), 10)
                                                     ),
                                                     
                                                     # Displays the dataset table.
                                                     tableOutput("preview"),
                                                     
                                                     # Pagination controls and information.
                                                     div(class = "table-footer",
                                                         
                                                         uiOutput("preview_caption"),
                                                         
                                                         div(class = "pagination-controls",
                                                             
                                                             actionButton("previous", "← Previous", class = "btn-secondary"),
                                                             
                                                             actionButton("next_page", "Next →", class = "btn-secondary")
                                                         )
                                                     ),
                                                     
                                                     # Downloads the imported dataset.
                                                     div(class = "panel-body",
                                                         downloadButton("download", "Export imported data",
                                                                        class = "btn-secondary"))
                                            ),
                                            
                                            # ------------------------------------------------
                                            # TAB 2: NESTED GROUPS
                                            # ------------------------------------------------
                                            
                                            tabPanel("Nested groups", value = "groups",
                                                     
                                                     div(class = "panel-body",
                                                         
                                                         div(class = "hierarchy-controls",
                                                             
                                                             # Allows users to select columns that define
                                                             # a hierarchy within their dataset.
                                                             # For example: School -> Class -> Student.
                                                             selectizeInput(
                                                               "groups",
                                                               "Build your hierarchy",
                                                               choices = NULL,
                                                               multiple = TRUE,
                                                               options = list(
                                                                 plugins = list("remove_button"),
                                                                 maxItems = 8L,
                                                                 placeholder = "Choose outer group, then inner groups…"
                                                               )
                                                             ),
                                                             
                                                             p(class = "hint",
                                                               "Select up to 8 columns from outermost to innermost. Remove and reselect a column to change its order. Repeated class names remain separate within each parent.")
                                                         ),
                                                         
                                                         # Displays the selected hierarchy and its group counts.
                                                         uiOutput("hierarchy_path"),
                                                         uiOutput("hierarchy"),
                                                         
                                                         # Exports the group counts.
                                                         downloadButton("groups_download", "Export group counts",
                                                                        class = "btn-secondary")
                                                     )
                                            ),
                                            
                                            # ------------------------------------------------
                                            # TAB 3: SUMMARY
                                            # ------------------------------------------------
                                            
                                            tabPanel("Summary", value = "stats",
                                                     
                                                     div(class = "panel-body",
                                                         
                                                         # Selects the numeric variable to analyze.
                                                         selectInput("variable", "Numeric variable", choices = NULL),
                                                         
                                                         # Displays descriptive statistics.
                                                         uiOutput("statistics"),
                                                         
                                                         # HISTOGRAM:
                                                         # plotOutput() creates a space for an R-generated plot.
                                                         # renderPlot() in the server creates the actual graph.
                                                         div(class = "chart-panel",
                                                             h3(class = "panel-title", "Distribution"),
                                                             plotOutput("distribution", height = "255px")
                                                         ),
                                                         
                                                         p(class = "hint",
                                                           "Descriptive summaries use finite observations. Missing and non-finite values are excluded. These summaries do not adjust for clustering or imply independent observations.")
                                                     )
                                            )
                                )
                            ),
                            
                            # Footer explaining how imported data is handled.
                            div(class = "footer-note",
                                "Your source file stays unchanged. Data is held in this Shiny session and is cleared when the session ends.")
                        )
                    )
                )
      )
  )
)


# ============================================================
# 4. SERVER
# ============================================================

# The server controls what happens when users interact with the UI.
# It performs calculations, reads data, updates outputs, and responds
# to buttons and other input controls.

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # REACTIVE DATA STORAGE
  # ----------------------------------------------------------
  
  # reactiveVal() stores information that can change during a session.
  # When its value changes, dependent outputs automatically update.
  dataset <- reactiveVal(NULL)
  
  # Stores import success/error messages.
  status <- reactiveVal(NULL)
  
  # Tracks which page of the dataset preview is being displayed.
  page <- reactiveVal(1L)
  
  # Determines the file extension of the uploaded file.
  # If no file has been selected, defaults to CSV.
  file_extension <- reactive({
    if (is.null(input$file)) "csv" else tolower(tools::file_ext(input$file$name))
  })
  
  
  # ----------------------------------------------------------
  # DYNAMIC IMPORT OPTIONS
  # ----------------------------------------------------------
  
  # renderUI() generates UI elements from the server.
  # The available options depend on the uploaded file type.
  output$import_options <- renderUI({
    
    ext <- file_extension()
    
    # RDS files do not require delimiter or decimal settings.
    if (ext == "rds") {
      return(p(class = "hint",
               "RDS files must contain a data frame or tibble. Column types are preserved."))
    }
    
    # JSON files have different import requirements.
    if (ext %in% c("json", "jsonl", "ndjson")) {
      return(p(class = "hint",
               "Use an array of records for JSON, or one record per line for JSON Lines. Nested object fields become columns with dotted names."))
    }
    
    # Determine whether the selected file is an Excel file.
    excel <- ext %in% c("xlsx", "xls")
    
    # If the file is Excel, retrieve the available worksheets.
    # tryCatch() prevents an invalid file from crashing the app.
    sheet_control <- if (excel) {
      tryCatch({
        
        sheets <- hub_excel_sheets(input$file$datapath)
        
        # Creates a dropdown to select an Excel worksheet.
        selectInput("sheet", "Worksheet", choices = sheets,
                    selected = head(sheets, 1L))
        
      }, error = function(e) {
        div(class = "notification error", role = "alert",
            conditionMessage(e))
      })
    }
    
    # Return the appropriate import settings.
    tagList(
      
      sheet_control,
      
      # Non-Excel files receive delimiter and decimal settings.
      if (!excel) div(class = "field-grid",
                      
                      selectInput("delimiter", "Delimiter",
                                  c("Comma (,)" = ",",
                                    "Tab" = "\t",
                                    "Semicolon (;)" = ";",
                                    "Pipe (|)" = "|"),
                                  selected = if (ext == "tsv") "\t" else ","),
                      
                      selectInput("decimal", "Decimal mark",
                                  c("Period (.)" = ".", "Comma (,)" = ","))
      ),
      
      # Determines whether the first row contains column names.
      checkboxInput("header", "First row contains column names", TRUE),
      
      # Allows users to specify missing-value markers.
      textInput("na_values", "Missing-value markers", "NA, N/A, NULL"),
      
      p(class = "hint",
        "Separate markers with commas. Empty cells are treated as missing.")
    )
  })
  
  
  # ----------------------------------------------------------
  # DATASET MANAGEMENT
  # ----------------------------------------------------------
  
  # Function that stores an imported dataset and updates the UI.
  set_dataset <- function(data, name, demo = FALSE) {
    
    # Store the dataset, its filename, and whether it is sample data.
    dataset(list(data = data, name = name, demo = demo))
    
    # Reset the preview to its first page.
    page(1L)
    
    # Clear the search field.
    updateTextInput(session, "search", value = "")
    
    # Identify numeric columns in the dataset.
    numeric_columns <- data_profile(data)$numeric_columns
    
    # Update the numeric variable dropdown.
    # Prefer a column named "score" if one exists.
    updateSelectInput(
      session,
      "variable",
      choices = numeric_columns,
      selected = if ("score" %in% numeric_columns) "score" else head(numeric_columns, 1)
    )
    
    # Automatically select school and class columns if available.
    defaults <- intersect(c("school", "class"), names(data))
    
    # Populate the hierarchy dropdown with all dataset columns.
    updateSelectizeInput(
      session,
      "groups",
      choices = names(data),
      selected = defaults,
      server = TRUE
    )
  }
  
  
  # Function that loads the example dataset.
  load_sample <- function() {
    
    # Read the example CSV from the examples folder.
    set_dataset(
      read_hub_data(file.path(project_root, "examples", "classroom-study.csv")),
      "classroom-study.csv",
      TRUE
    )
    
    # Clear any previous import messages.
    status(NULL)
  }
  
  
  # Load the sample dataset automatically when the app starts.
  load_sample()
  
  # Load the sample dataset again whenever the sample button is clicked.
  observeEvent(input$sample, load_sample())
  
  # Display a message when a user selects a file.
  observeEvent(input$file, {
    status(list(type = "info",
                text = "File selected. Check the options, then select Import data."))
  })
  
  
  # ----------------------------------------------------------
  # IMPORT USER DATA
  # ----------------------------------------------------------
  
  # Runs when the user clicks the Import data button.
  observeEvent(input$import, {
    
    # Make sure a file has been selected.
    if (is.null(input$file)) {
      status(list(type = "error", text = "Choose a data file first."))
      return()
    }
    
    # tryCatch() handles errors during the import process.
    tryCatch({
      
      # Identify the uploaded file extension.
      ext <- tolower(tools::file_ext(input$file$name))
      
      # Only allow supported file types.
      if (!ext %in% c("csv", "tsv", "txt", "xlsx", "xls",
                      "json", "jsonl", "ndjson", "rds")) {
        stop("Choose a CSV, TSV, TXT, Excel, JSON, JSON Lines, or RDS file.",
             call. = FALSE)
      }
      
      # Determine which values should be treated as missing.
      markers <- if (is.null(input$na_values)) {
        c("", "NA", "N/A", "NULL")
      } else {
        unique(c("", trimws(strsplit(input$na_values, ",", fixed = TRUE)[[1]]))
        )
      }
      
      # Read the uploaded file using the appropriate import settings.
      data <- read_hub_file(
        input$file$datapath,
        filename = input$file$name,
        delimiter = if (is.null(input$delimiter)) "," else input$delimiter,
        header = if (is.null(input$header)) TRUE else isTRUE(input$header),
        na_strings = markers,
        decimal = if (is.null(input$decimal)) "." else input$decimal,
        sheet = if (is.null(input$sheet) || !nzchar(input$sheet)) 1L else input$sheet
      )
      
      # Store the imported dataset and update the dashboard.
      set_dataset(data, input$file$name)
      
      # Display a success message.
      status(list(
        type = "success",
        text = sprintf("Imported %s rows successfully.",
                       format(nrow(data), big.mark = ","))
      ))
      
    }, error = function(e) {
      
      # If something goes wrong, display the error to the user.
      status(list(type = "error", text = conditionMessage(e)))
    })
  })
  
  
  # ----------------------------------------------------------
  # NAVIGATION
  # ----------------------------------------------------------
  
  # These observers respond to clicks on the sidebar links.
  # updateTabsetPanel() changes which tab is displayed.
  
  observeEvent(input$nav_data,
               updateTabsetPanel(session, "view", selected = "data"))
  
  observeEvent(input$nav_groups,
               updateTabsetPanel(session, "view", selected = "groups"))
  
  observeEvent(input$nav_stats,
               updateTabsetPanel(session, "view", selected = "stats"))
  
  
  # ----------------------------------------------------------
  # DATASET INFORMATION
  # ----------------------------------------------------------
  
  # current_data() retrieves the currently selected dataset.
  # req() prevents execution until a dataset is available.
  current_data <- reactive({
    req(dataset())
    dataset()$data
  })
  
  # Calculate the dataset profile using the helper function in data.R.
  profile <- reactive(data_profile(current_data()))
  
  # Display the dataset filename.
  output$dataset_title <- renderUI(span(dataset()$name))
  
  # Display whether the data is sample data or user-imported data.
  output$dataset_badge <- renderUI(
    span(class = "dataset-source",
         if (dataset()$demo) "Sample data" else "Imported")
  )
  
  # Display import messages.
  output$import_status <- renderUI({
    
    message <- status()
    
    if (is.null(message)) return(NULL)
    
    div(
      class = paste("notification", message$type),
      role = if (message$type == "error") "alert" else "status",
      message$text
    )
  })
  
  
  # ----------------------------------------------------------
  # SUMMARY METRICS
  # ----------------------------------------------------------
  
  # Generate the metric cards at the top of the workspace.
  output$metrics <- renderUI({
    
    p <- profile()
    
    div(
      class = "metrics-grid",
      
      metric("Rows", format(p$rows, big.mark = ",")),
      
      metric("Columns", p$columns),
      
      metric("Missing / non-finite",
             format(p$missing, big.mark = ","))
    )
  })
  
  
  # ----------------------------------------------------------
  # DATA FILTERING AND PAGINATION
  # ----------------------------------------------------------
  
  # filtered_data() searches across all columns of the dataset.
  filtered_data <- reactive({
    
    data <- current_data()
    
    query <- trimws(if (is.null(input$search)) "" else input$search)
    
    # If the search field is empty, return the entire dataset.
    if (!nzchar(query)) return(data)
    
    # Search each column for the specified text.
    matches <- Reduce(`|`, lapply(data, function(x) {
      
      result <- grepl(tolower(query), tolower(as.character(x)), fixed = TRUE)
      
      # Missing values should not count as matches.
      result[is.na(x)] <- FALSE
      
      result
    }))
    
    # Return only rows that matched.
    data[matches, , drop = FALSE]
  })
  
  
  # Determine the number of rows displayed per page.
  page_size <- reactive({
    if (is.null(input$page_size)) 10L else as.integer(input$page_size)
  })
  
  # Calculate how many pages are required.
  total_pages <- reactive(
    max(1L, ceiling(nrow(filtered_data()) / page_size()))
  )
  
  # Reset to the first page when the search or page size changes.
  observeEvent(list(input$search, input$page_size), page(1L))
  
  # Previous button: move back one page.
  observeEvent(input$previous, page(max(1L, page() - 1L)))
  
  # Next button: move forward one page.
  observeEvent(input$next_page, page(min(total_pages(), page() + 1L)))
  
  
  # Determine which rows should appear on the current page.
  preview_data <- reactive({
    
    data <- filtered_data()
    
    start <- (min(page(), total_pages()) - 1L) * page_size() + 1L
    
    if (!nrow(data)) return(data)
    
    data[
      seq.int(start, min(nrow(data), start + page_size() - 1L)),
      ,
      drop = FALSE
    ]
  })
  
  
  # ----------------------------------------------------------
  # DISPLAY DATA TABLE
  # ----------------------------------------------------------
  
  # renderTable() converts the selected rows into an HTML table.
  output$preview <- renderTable(
    preview_data(),
    striped = FALSE,
    bordered = FALSE,
    spacing = "m",
    rownames = FALSE,
    width = "100%",
    na = "—",
    sanitize.text.function = htmltools::htmlEscape
  )
  
  
  # Displays information about the current page.
  output$preview_caption <- renderUI({
    
    count <- nrow(filtered_data())
    
    start <- if (count) {
      (min(page(), total_pages()) - 1L) * page_size() + 1L
    } else {
      0L
    }
    
    span(
      class = "preview-caption",
      if (!count) {
        "No matching rows"
      } else {
        sprintf(
          "Showing %s–%s of %s rows · Page %s of %s",
          start,
          min(count, start + page_size() - 1L),
          format(count, big.mark = ","),
          min(page(), total_pages()),
          total_pages()
        )
      }
    )
  })
  
  
  # ----------------------------------------------------------
  # NESTED GROUP ANALYSIS
  # ----------------------------------------------------------
  
  # Determine the selected grouping columns.
  grouped <- reactive({
    
    columns <- if (is.null(input$groups)) {
      character()
    } else {
      intersect(input$groups, names(current_data()))
    }
    
    # Calculate the hierarchy summary.
    # If an error occurs, display a validation message.
    tryCatch(
      hierarchy_summary(current_data(), columns),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
  })
  
  
  # Display the selected hierarchy path.
  output$hierarchy_path <- renderUI({
    
    if (!length(input$groups)) return(NULL)
    
    div(
      class = "hierarchy-path",
      
      lapply(seq_along(input$groups), function(i) {
        
        span(
          class = "hierarchy-chip",
          paste(i, input$groups[i]),
          if (i < length(input$groups)) " → "
        )
      })
    )
  })
  
  
  # Display the nested hierarchy as a tree.
  output$hierarchy <- renderUI({
    
    # If no columns have been selected, display an empty-state message.
    if (!length(input$groups)) {
      return(empty_message(
        "Give your data a little structure",
        "Choose grouping columns above, starting with the outermost level."
      ))
    }
    
    groups <- grouped()
    
    # Display at most 300 groups in the browser.
    shown <- head(groups, 300L)
    
    # Organize groups according to their parent IDs.
    children <- split(seq_len(nrow(shown)), shown$parent_id)
    
    # Recursive function that builds the hierarchy.
    # A recursive function calls itself to construct nested levels.
    render_nodes <- function(parent = "") {
      
      key <- match(parent, names(children))
      
      indices <- if (is.na(key)) integer() else children[[key]]
      
      tagList(lapply(indices, function(i) {
        
        # Display the group name and its number of rows.
        heading <- tagList(
          span(class = "hierarchy-label", shown$label[i]),
          span(
            class = "hierarchy-count",
            paste(shown$rows[i],
                  if (shown$rows[i] == 1L) "row" else "rows")
          )
        )
        
        # If the group has children, create an expandable section.
        if (shown$id[i] %in% shown$parent_id) {
          
          tags$details(
            class = "hierarchy-node",
            open = if (shown$depth[i] == 1L) "open" else NULL,
            
            tags$summary(class = "hierarchy-summary", heading),
            
            div(
              class = "hierarchy-children",
              render_nodes(shown$id[i])
            )
          )
          
        } else {
          
          # Otherwise, display it as a leaf node.
          div(
            class = "hierarchy-node hierarchy-leaf",
            div(class = "hierarchy-summary", heading)
          )
        }
      }))
    }
    
    # Create the hierarchy tree.
    div(
      class = "hierarchy-tree",
      
      p(
        class = "hint",
        sprintf(
          "%s groups across %s levels. Counts include every imported row.",
          nrow(groups),
          length(input$groups)
        )
      ),
      
      render_nodes(),
      
      if (nrow(groups) > 300L) {
        p(class = "hint",
          "Showing the first 300 groups. Export group counts for the complete hierarchy.")
      }
    )
  })
  
  
  # ----------------------------------------------------------
  # DESCRIPTIVE STATISTICS
  # ----------------------------------------------------------
  
  # Generate the descriptive statistics for the selected variable.
  output$statistics <- renderUI({
    
    # If there are no numeric columns, display a message.
    if (!length(profile()$numeric_columns)) {
      return(empty_message(
        "No numeric columns yet",
        "Import a dataset with numeric observations to see a summary and distribution."
      ))
    }
    
    # Ensure that the selected variable exists.
    req(input$variable %in% names(current_data()))
    
    # Calculate descriptive statistics using the helper function.
    stats <- numeric_summary(current_data(), input$variable)
    
    # Display the results as metric cards.
    div(
      class = "statistics-grid",
      
      metric(
        "Mean",
        format_number(stats$mean),
        paste(stats$count, "finite observations")
      ),
      
      metric(
        "Median",
        format_number(stats$median),
        "Middle observation"
      ),
      
      metric(
        "Standard deviation",
        format_number(stats$sd),
        "Sample standard deviation"
      ),
      
      metric(
        "Range",
        paste(format_number(stats$min), "–", format_number(stats$max)),
        paste(stats$missing, "missing / non-finite")
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # DISTRIBUTION PLOT
  # ----------------------------------------------------------
  
  # renderPlot() generates an actual R plot inside the Shiny application.
  # This example uses the base R hist() function.
  # The plot updates automatically when the selected variable changes.
  output$distribution <- renderPlot({
    
    # Check that numeric data exists.
    validate(
      need(length(profile()$numeric_columns),
           "Import numeric data to see its distribution.")
    )
    
    # Confirm that the selected variable exists.
    req(input$variable %in% names(current_data()))
    
    # Extract the selected numeric variable.
    values <- current_data()[[input$variable]]
    
    # Remove missing and non-finite observations.
    values <- values[is.finite(values)]
    
    # Ensure that at least one observation remains.
    validate(
      need(length(values),
           "This column has no finite observations.")
    )
    
    # Customize the base R plot appearance.
    par(
      mar = c(4, 4, 1, 1),
      fg = "#73726c",
      col.axis = "#73726c",
      col.lab = "#73726c",
      family = "sans",
      bg = "#faf9f5"
    )
    
    # Generate the histogram.
    hist(
      values,
      breaks = "Sturges",
      col = "#d97757",
      border = "#faf9f5",
      main = "",
      xlab = input$variable,
      ylab = "Observations"
    )
    
  }, res = 110, alt = "Histogram of the selected numeric variable")
  
  
  # ----------------------------------------------------------
  # DOWNLOAD FUNCTIONS
  # ----------------------------------------------------------
  
  # downloadHandler() allows users to download the imported dataset.
  output$download <- downloadHandler(
    
    filename = function() "imported-data.csv",
    
    content = function(file) {
      utils::write.csv(
        current_data(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
  
  
  # Allows users to download the complete nested group summary.
  output$groups_download <- downloadHandler(
    
    filename = function() "nested-group-counts.csv",
    
    content = function(file) {
      utils::write.csv(
        grouped(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
  
  
  # Allows users to download the original sample dataset.
  output$sample_download <- downloadHandler(
    
    filename = function() "classroom-study.csv",
    
    content = function(file) {
      file.copy(
        file.path(project_root, "examples", "classroom-study.csv"),
        file
      )
    }
  )
  
}


# ============================================================
# 5. RUN THE APPLICATION
# ============================================================

# shinyApp() combines the UI and server into a Shiny application.
# This is the final expression Shiny needs when it loads app.R.
shinyApp(ui = ui, server = server)

