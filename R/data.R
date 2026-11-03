# Data preparation is kept independent of Shiny so imports and grouping can be
# reused from scripts, notebooks, and tests.

hub_missing <- function(x) {
  if (is.numeric(x)) !is.finite(x) else is.na(x)
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
  column_names <- trimws(sub("^\ufeff", "", names(data)))
  blank <- is.na(column_names) | !nzchar(column_names)
  column_names[blank] <- paste("Column", which(blank))
  names(data) <- make.unique(column_names, sep = "_")
  data
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
