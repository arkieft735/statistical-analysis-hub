# Run from the project root: Rscript --vanilla tests/test-data.R
source(file.path("R", "data.R"))

expect_error <- function(expression, pattern) {
  message <- tryCatch({ force(expression); NULL }, error = conditionMessage)
  stopifnot(!is.null(message), grepl(pattern, message, ignore.case = TRUE))
}

with_file <- function(text, test) {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(text, path, useBytes = TRUE)
  test(path)
}

with_file(c('school,class,student,score,note',
            'North,A,1,87,"hello, world"',
            'North,A,2,NA,"a ""quoted"" value"',
            'South,A,3,91,"two', 'lines"'), function(path) {
  data <- read_hub_data(path)
  stopifnot(nrow(data) == 3L, ncol(data) == 5L,
            data$note[1] == "hello, world", data$note[2] == 'a "quoted" value',
            data$note[3] == "two\nlines", is.na(data$score[2]))
})
with_file(c("group;value", "A;1", "B;2"), function(path) {
  stopifnot(identical(read_hub_data(path, delimiter = ";")$value, c(1L, 2L)))
})
with_file(c("group;value", "A;1,25", "B;2,5"), function(path) {
  stopifnot(identical(read_hub_data(path, delimiter = ";", decimal = ",")$value, c(1.25, 2.5)))
})
with_file(c("group\tvalue", "A\t1", "B\t2"), function(path) {
  stopifnot(ncol(read_hub_data(path, delimiter = "\t")) == 2L)
})
with_file(c("A|1", "B|2"), function(path) {
  data <- read_hub_data(path, delimiter = "|", header = FALSE)
  stopifnot(identical(names(data), c("V1", "V2")), data$V2[2] == 2)
})
with_file(c('school,,school," score "', "North,A,West,7"), function(path) {
  stopifnot(identical(names(read_hub_data(path)), c("school", "Column 2", "school_1", "score")))
})
with_file(c("group,value", "A,none", "B,2"), function(path) {
  data <- read_hub_data(path, na_strings = "none")
  stopifnot(is.na(data$value[1]), is.numeric(data$value))
  expect_error(read_hub_data(path, max_rows = 1L), "row import limit")
  expect_error(read_hub_data(path, max_bytes = 1L), "MB import limit")
  expect_error(read_hub_data(path, delimiter = "bad"), "delimiter")
  expect_error(read_hub_data(path, decimal = "bad"), "decimal")
  expect_error(read_hub_data(path, max_columns = 1L), "column import limit")
})
with_file(c("group,value", "A,2,3"), function(path) {
  expect_error(read_hub_data(path), "different numbers of fields")
})
with_file("group,value", function(path) {
  expect_error(read_hub_data(path), "no data rows")
})
with_file(character(), function(path) {
  expect_error(read_hub_data(path), "empty")
})
with_file(c('group,value', '"unfinished,2'), function(path) {
  expect_error(read_hub_data(path), "parsed|read|different numbers")
})
binary_path <- tempfile()
writeBin(as.raw(c(65, 0, 66)), binary_path)
expect_error(read_hub_data(binary_path), "binary")
unlink(binary_path)
# UTF-8 BOMs and optional final newlines are common in exported CSV files.
bom_path <- tempfile()
writeBin(c(as.raw(c(239, 187, 191)), charToRaw("name,value\nCafé,2")), bom_path)
bom_data <- read_hub_data(bom_path)
stopifnot(identical(names(bom_data), c("name", "value")), bom_data$name[1] == "Café")
unlink(bom_path)

groups <- data.frame(school = c("North", "North", "South", "South", "a / b", "a"),
                     class = c("A", "B", "A", NA, "c", "b / c"),
                     student = c(1L, 2L, 3L, 4L, 5L, 6L))
hierarchy <- hierarchy_summary(groups, c("school", "class"))
stopifnot(!anyDuplicated(hierarchy$id), sum(hierarchy$rows[hierarchy$depth == 1L]) == 6L,
          sum(hierarchy$rows[hierarchy$depth == 2L]) == 6L,
          length(unique(hierarchy$parent_id[hierarchy$label == "A"])) == 2L,
          "(Missing)" %in% hierarchy$label)
# These display paths collide, but their IDs and parents remain distinct.
collision <- hierarchy[hierarchy$path == "a / b / c", ]
stopifnot(nrow(collision) == 2L, length(unique(collision$id)) == 2L,
          length(unique(collision$parent_id)) == 2L)
tree <- hierarchy_tree(groups, c("school", "class", "student"))
stopifnot(length(tree) == 4L, tree[[1]]$label == "North", tree[[1]]$rows == 2L,
          tree[[1]]$children[[1]]$children[[1]]$label == "1",
          identical(names(tree[[1]]), c("id", "label", "rows", "children")),
          nrow(hierarchy_summary(groups, character())) == 0L)
expect_error(hierarchy_summary(groups, c("class", "class")), "distinct")
expect_error(hierarchy_summary(groups, "unknown"), "distinct")
missing_labels <- hierarchy_summary(data.frame(group = c(NA, "(Missing)", "")), "group")
stopifnot(nrow(missing_labels) == 3L, !anyDuplicated(missing_labels$id),
          sum(missing_labels$label == "(Missing)") == 2L)
bounded <- data.frame(school = c("North", "North", "South"), class = c("A", "B", "A"))
stopifnot(nrow(hierarchy_summary(bounded, c("school", "class"), max_depth = 2L, max_nodes = 5L)) == 5L)
expect_error(hierarchy_summary(bounded, c("school", "class"), max_depth = 1L), "at most 1 hierarchy levels")
expect_error(hierarchy_summary(bounded, c("school", "class"), max_nodes = 4L), "exceeds 4 groups")
# The direct-child check rejects a wide level before constructing its nodes.
expect_error(hierarchy_summary(data.frame(id = seq_len(10)), "id", max_nodes = 9L), "exceeds 9 groups")
expect_error(hierarchy_tree(bounded, c("school", "class"), max_nodes = 4L), "exceeds 4 groups")
expect_error(hierarchy_summary(bounded, "school", max_depth = 0L), "positive whole numbers")
expect_error(hierarchy_summary(bounded, "school", max_nodes = Inf), "positive whole numbers")
nine_levels <- as.data.frame(setNames(rep(list("A"), 9), paste0("level", seq_len(9))))
expect_error(hierarchy_summary(nine_levels, names(nine_levels)), "at most 8 hierarchy levels")

numeric <- data.frame(score = c(2, 4, NA, NaN, Inf, -Inf),
                       constant = rep(5, 6), empty = rep(NA_real_, 6),
                       note = c("a", "b", "c", "d", "e", NA))
summary <- numeric_summary(numeric, "score")
stopifnot(summary$count == 2L, summary$missing == 4L, summary$mean == 3,
          summary$median == 3, summary$sd == sqrt(2), summary$min == 2, summary$max == 4,
          numeric_summary(numeric, "constant")$sd == 0,
          is.na(numeric_summary(numeric, "empty")$mean),
          data_profile(numeric)$missing == 11L,
          identical(data_profile(numeric)$numeric_columns, c("score", "constant", "empty")))
expect_error(numeric_summary(numeric, "note"), "numeric")

sample <- read_hub_data(file.path("examples", "classroom-study.csv"))
stopifnot(nrow(sample) == 36L, ncol(sample) == 7L)
cat("All data helper tests passed.\n")
