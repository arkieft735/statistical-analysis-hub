# Data preparation is kept independent of Shiny so imports and grouping can be
# reused from scripts, notebooks, and tests.

hub_missing <- function(x) {
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

hub_normalize_names <- function(data) {
  column_names <- trimws(sub("^\ufeff", "", names(data)))
  blank <- is.na(column_names) | !nzchar(column_names)
  column_names[blank] <- paste("Column", which(blank))
  names(data) <- make.unique(column_names, sep = "_")
  data
}

hub_validate_file <- function(path, max_bytes = 50 * 1024^2) {
  if (length(path) != 1L || is.na(path) || !file.exists(path) || dir.exists(path)) {
    stop("Choose an existing data file.", call. = FALSE)
  }
  if (length(max_bytes) != 1L || !is.finite(max_bytes) || max_bytes < 1L) {
    stop("Import limits must be positive numbers.", call. = FALSE)
  }
  bytes <- file.info(path)$size
  if (is.na(bytes) || bytes == 0) stop("The selected file is empty.", call. = FALSE)
  if (bytes > max_bytes) {
    stop(sprintf("The file exceeds the %.0f MB import limit.", max_bytes / 1024^2),
         call. = FALSE)
  }
  invisible(bytes)
}

hub_require_reader <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("This import requires '%s'. Run renv::restore() in the project root.", package),
         call. = FALSE)
  }
}

hub_validate_table <- function(data, max_rows, max_columns) {
  if (!length(data) && !is.data.frame(data)) {
    stop("The file has no data rows.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("Use a table of records: JSON must contain an array of objects, or one object per line for JSONL.",
         call. = FALSE)
  }
  if (!nrow(data)) stop("The file has no data rows.", call. = FALSE)
  if (!ncol(data)) stop("The file has no data columns. Each record needs named fields.", call. = FALSE)
  if (nrow(data) > max_rows) {
    stop(sprintf("The file exceeds the %s-row import limit.", format(max_rows, big.mark = ",")),
         call. = FALSE)
  }
  if (ncol(data) > max_columns) {
    stop(sprintf("The file exceeds the %s-column import limit.", max_columns), call. = FALSE)
  }
  complex <- names(data)[vapply(data, function(column) is.list(column) || !is.null(dim(column)), logical(1))]
  if (length(complex)) {
    stop(paste0("Array or list values cannot be displayed as table columns (",
                paste(head(complex, 3L), collapse = ", "),
                "). Expand arrays into separate rows or scalar columns before importing. Nested objects are supported."),
         call. = FALSE)
  }
  hub_normalize_names(as.data.frame(data, stringsAsFactors = FALSE))
}

hub_excel_sheets <- function(path, max_bytes = 50 * 1024^2) {
  hub_validate_file(path, max_bytes)
  hub_require_reader("readxl")
  tryCatch(readxl::excel_sheets(path), error = function(e) {
    stop("The workbook could not be read. Choose a valid XLSX or XLS workbook.", call. = FALSE)
  })
}

read_hub_file <- function(path, filename = basename(path), delimiter = ",", header = TRUE,
                          na_strings = c("", "NA"), decimal = ".", sheet = 1,
                          max_bytes = 50 * 1024^2, max_rows = 100000L, max_columns = 200L) {
  hub_validate_file(path, max_bytes)
  valid_limit <- function(value) is.numeric(value) && length(value) == 1L &&
    is.finite(value) && value >= 1L && value == floor(value) && value < .Machine$integer.max
  if (!valid_limit(max_rows) || !valid_limit(max_columns)) {
    stop("Row and column import limits must be positive whole numbers.", call. = FALSE)
  }
  if (length(filename) != 1L || is.na(filename)) stop("Choose a data file with a supported extension.", call. = FALSE)
  extension <- tolower(tools::file_ext(filename))
  if (extension %in% c("csv", "tsv", "txt")) {
    return(read_hub_data(path, delimiter = delimiter, header = header,
                         na_strings = na_strings, max_bytes = max_bytes,
                         max_rows = max_rows, decimal = decimal, max_columns = max_columns))
  }
  if (extension %in% c("xlsx", "xls")) {
    hub_require_reader("readxl")
    if (!is.logical(header) || length(header) != 1L || is.na(header)) {
      stop("The header option must be TRUE or FALSE.", call. = FALSE)
    }
    sheets <- hub_excel_sheets(path, max_bytes = max_bytes)
    valid_sheet <- (is.character(sheet) && length(sheet) == 1L && !is.na(sheet) && sheet %in% sheets) ||
      (is.numeric(sheet) && length(sheet) == 1L && is.finite(sheet) && sheet >= 1L &&
       sheet == floor(sheet) && sheet <= length(sheets))
    if (!valid_sheet) stop("Choose an existing worksheet from the workbook.", call. = FALSE)
    data <- tryCatch(
      withCallingHandlers(
        readxl::read_excel(path, sheet = sheet, col_names = header,
                           na = unique(c("", as.character(na_strings))),
                           n_max = as.integer(max_rows) + 1L, .name_repair = "minimal"),
        warning = function(w) stop(paste0("Worksheet import warning: ", conditionMessage(w),
                                           ". Convert mixed-type columns to text or use CSV."), call. = FALSE)
      ),
      error = function(e) stop(paste0("The worksheet could not be imported. ", conditionMessage(e)), call. = FALSE)
    )
    return(hub_validate_table(data, max_rows, max_columns))
  }
  if (!extension %in% c("json", "jsonl", "ndjson")) {
    stop("Choose CSV, TSV, TXT, XLSX, XLS, JSON, JSONL, or NDJSON.", call. = FALSE)
  }
  hub_require_reader("jsonlite")
  connection <- file(path, open = "rb")
  bytes <- tryCatch(readBin(connection, what = "raw", n = file.info(path)$size),
                     finally = close(connection))
  if (any(bytes == as.raw(0))) stop("Choose a UTF-8 JSON text file without binary data.", call. = FALSE)
  text <- rawToChar(bytes)
  text <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  if (is.na(text)) stop("JSON files must use UTF-8 text encoding.", call. = FALSE)
  text <- sub("^\ufeff", "", text)
  if (extension == "json") {
    # Check the top-level shape before passing text to fromJSON, which also
    # accepts URLs and file names as its input argument.
    if (!startsWith(trimws(text), "[")) {
      stop("JSON must contain an array of record objects, for example [{\"name\":\"A\",\"score\":12}].",
           call. = FALSE)
    }
    data <- tryCatch(jsonlite::fromJSON(text, flatten = TRUE), error = function(e) {
      stop("The JSON could not be parsed. Check commas, quotes, and brackets.", call. = FALSE)
    })
  } else {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) > max_rows) {
      stop(sprintf("The file exceeds the %s-row import limit.", format(max_rows, big.mark = ",")),
           call. = FALSE)
    }
    if (!length(lines)) stop("The file has no data rows.", call. = FALSE)
    if (!all(startsWith(trimws(lines), "{"))) {
      stop("JSONL and NDJSON require one record object per line.", call. = FALSE)
    }
    connection <- textConnection(lines, open = "r")
    data <- tryCatch(jsonlite::stream_in(connection, verbose = FALSE, flatten = TRUE),
                     error = function(e) stop("The JSONL could not be parsed. Use one complete JSON object per line.", call. = FALSE),
                     finally = close(connection))
  }
  hub_validate_table(data, max_rows, max_columns)
}

read_hub_data <- function(path, delimiter = ",", header = TRUE,
                          na_strings = c("", "NA"),
                          max_bytes = 50 * 1024^2, max_rows = 100000L,
                          decimal = ".", max_columns = 200L) {
  if (length(path) != 1L || is.na(path) || !file.exists(path) || dir.exists(path)) {
    stop("Choose an existing CSV or TSV file.", call. = FALSE)
  }
  if (length(delimiter) != 1L || is.na(delimiter) ||
      !delimiter %in% c(",", ";", "\t", "|")) {
    stop("Choose comma, semicolon, tab, or pipe as the delimiter.", call. = FALSE)
  }
  if (!is.logical(header) || length(header) != 1L || is.na(header)) {
    stop("The header option must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(decimal) != 1L || is.na(decimal) || !decimal %in% c(".", ",")) {
    stop("Choose period or comma as the decimal mark.", call. = FALSE)
  }
  if (length(max_bytes) != 1L || !is.finite(max_bytes) || max_bytes < 1L ||
      length(max_rows) != 1L || !is.finite(max_rows) || max_rows < 1L ||
      length(max_columns) != 1L || !is.finite(max_columns) || max_columns < 1L) {
    stop("Import limits must be positive numbers.", call. = FALSE)
  }
  bytes <- file.info(path)$size
  if (is.na(bytes) || bytes == 0) stop("The selected file is empty.", call. = FALSE)
  if (bytes > max_bytes) {
    stop(sprintf("The file exceeds the %.0f MB import limit.", max_bytes / 1024^2),
         call. = FALSE)
  }
  # A binary file must never be interpreted as a delimited table.
  connection <- file(path, open = "rb")
  raw_data <- tryCatch(readBin(connection, what = "raw", n = bytes),
                       finally = close(connection))
  if (any(raw_data == as.raw(0))) {
    stop("This file contains binary data. Choose a UTF-8 CSV or TSV file.", call. = FALSE)
  }
  rm(raw_data)

  # Check field counts before read.table can silently promote the first column
  # to row names. count.fields understands quoted separators and line breaks.
  field_counts <- tryCatch(
    withCallingHandlers(
      utils::count.fields(path, sep = delimiter, quote = "\"", comment.char = "",
                          blank.lines.skip = TRUE),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    ),
    error = function(e) stop("The file could not be parsed. Check its delimiter and quoted values.",
                             call. = FALSE)
  )
  field_counts <- field_counts[!is.na(field_counts)]
  if (length(field_counts) == 0L) stop("The selected file is empty.", call. = FALSE)
  if (length(unique(field_counts)) > 1L) {
    stop("Rows have different numbers of fields. Check the delimiter and quoted values.",
         call. = FALSE)
  }
  if (field_counts[1] > max_columns) {
    stop(sprintf("The file exceeds the %s-column import limit.", max_columns), call. = FALSE)
  }
  data <- tryCatch(
    withCallingHandlers(
      utils::read.table(
        path, header = header, sep = delimiter, dec = decimal, quote = "\"", comment.char = "",
        stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL,
        na.strings = unique(as.character(na_strings)), strip.white = TRUE,
        fill = FALSE, blank.lines.skip = TRUE, fileEncoding = "UTF-8-BOM",
        nrows = as.integer(max_rows) + 1L
      ),
      warning = function(w) {
        # A final newline is optional in otherwise valid CSV files.
        if (grepl("incomplete final line", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        } else {
          stop(conditionMessage(w), call. = FALSE)
        }
      }
    ),
    error = function(e) stop("The file could not be read. Use a UTF-8 CSV or TSV with consistent columns.",
                             call. = FALSE)
  )
  if (nrow(data) == 0L) stop("The file has no data rows.", call. = FALSE)
  if (nrow(data) > max_rows) {
    stop(sprintf("The file exceeds the %s-row import limit.", format(max_rows, big.mark = ",")),
         call. = FALSE)
  }
  hub_normalize_names(data)
}

data_profile <- function(data) {
  if (!is.data.frame(data)) stop("Expected a data frame.", call. = FALSE)
  list(
    rows = nrow(data), columns = ncol(data),
    missing = sum(vapply(data, function(x) sum(hub_missing(x)), numeric(1))),
    numeric_columns = names(data)[vapply(data, is.numeric, logical(1))]
  )
}

numeric_summary <- function(data, column) {
  if (!is.data.frame(data) || length(column) != 1L || !column %in% names(data) ||
      !is.numeric(data[[column]])) {
    stop("Choose a numeric column to summarize.", call. = FALSE)
  }
  values <- data[[column]]
  finite <- values[is.finite(values)]
  list(
    count = length(finite), missing = length(values) - length(finite),
    mean = if (length(finite)) mean(finite) else NA_real_,
    median = if (length(finite)) stats::median(finite) else NA_real_,
    sd = if (length(finite) > 1L) stats::sd(finite) else NA_real_,
    min = if (length(finite)) min(finite) else NA_real_,
    max = if (length(finite)) max(finite) else NA_real_
  )
}

hierarchy_summary <- function(data, columns, max_depth = 8L, max_nodes = 250000L) {
  if (!is.data.frame(data) || !is.character(columns) || anyNA(columns) ||
      anyDuplicated(columns) || !all(columns %in% names(data))) {
    stop("Choose distinct hierarchy columns from the dataset.", call. = FALSE)
  }
  valid_limit <- function(value) {
    is.numeric(value) && length(value) == 1L && is.finite(value) &&
      value >= 1L && value == floor(value)
  }
  if (!valid_limit(max_depth) || !valid_limit(max_nodes)) {
    stop("Hierarchy limits must be positive whole numbers.", call. = FALSE)
  }
  if (length(columns) > max_depth) {
    stop(sprintf("Choose at most %s hierarchy levels.", max_depth), call. = FALSE)
  }
  too_many_groups <- function() {
    stop(sprintf(paste0("Hierarchy exceeds %s groups. Choose fewer levels or use ",
                         "less detailed grouping columns."),
                 format(max_nodes, big.mark = ",", scientific = FALSE, trim = TRUE)),
         call. = FALSE)
  }
  empty <- data.frame(id = character(), parent_id = character(), depth = integer(),
                      label = character(), path = character(), rows = integer(),
                      stringsAsFactors = FALSE)
  if (!length(columns) || !nrow(data)) return(empty)
  # Store columns directly instead of constructing a data frame for every
  # node. This keeps high-cardinality levels (for example, student IDs) usable.
  capacity <- min(nrow(data) * length(columns), 10000L, max_nodes)
  ids <- parent_ids <- node_labels <- node_paths <- character(capacity)
  depths <- row_counts <- integer(capacity)
  cursor <- 0L
  visit <- function(indices, depth, parent_id, parent_labels) {
    values <- data[[columns[depth]]][indices]
    labels <- as.character(values)
    labels[hub_missing(values)] <- NA_character_
    unique_labels <- unique(labels)
    # Check before allocating the groups or expanding their descendants.
    if (length(unique_labels) > max_nodes - cursor) too_many_groups()
    membership <- match(labels, unique_labels)
    groups <- split(indices, factor(membership, levels = seq_along(unique_labels)))
    for (i in seq_along(groups)) {
      # Earlier siblings can have added descendants since the level check.
      if (cursor >= max_nodes) too_many_groups()
      cursor <<- cursor + 1L
      if (cursor > length(ids)) {
        new_capacity <- min(max_nodes, max(cursor, length(ids) * 2L))
        length(ids) <<- new_capacity
        length(parent_ids) <<- new_capacity
        length(node_labels) <<- new_capacity
        length(node_paths) <<- new_capacity
        length(depths) <<- new_capacity
        length(row_counts) <<- new_capacity
      }
      id <- paste0("group-", cursor)
      label <- if (is.na(unique_labels[i])) "(Missing)" else unique_labels[i]
      path_labels <- c(parent_labels, label)
      ids[cursor] <<- id
      parent_ids[cursor] <<- parent_id
      depths[cursor] <<- depth
      node_labels[cursor] <<- label
      node_paths[cursor] <<- paste(path_labels, collapse = " / ")
      row_counts[cursor] <<- length(groups[[i]])
      if (depth < length(columns)) visit(groups[[i]], depth + 1L, id, path_labels)
    }
  }
  visit(seq_len(nrow(data)), 1L, "", character())
  kept <- seq_len(cursor)
  data.frame(id = ids[kept], parent_id = parent_ids[kept], depth = depths[kept],
             label = node_labels[kept], path = node_paths[kept], rows = row_counts[kept],
             stringsAsFactors = FALSE)
}

hierarchy_tree <- function(data, columns, max_depth = 8L, max_nodes = 250000L) {
  summary <- hierarchy_summary(data, columns, max_depth = max_depth, max_nodes = max_nodes)
  if (!nrow(summary)) return(list())
  parent_keys <- unique(summary$parent_id)
  children <- split(seq_len(nrow(summary)),
                    factor(match(summary$parent_id, parent_keys), levels = seq_along(parent_keys)))
  build <- function(parent_id) {
    parent_index <- match(parent_id, parent_keys)
    if (is.na(parent_index)) return(list())
    indices <- children[[parent_index]]
    lapply(indices, function(i) list(
      id = summary$id[i], label = summary$label[i], rows = summary$rows[i],
      children = build(summary$id[i])
    ))
  }
  build("")
}
