# Run from the project root: Rscript --vanilla tests/test-server.R
# These tests exercise user workflows through a real Shiny server session.
library(testthat)
library(shiny)
source("app/app.R", local = TRUE)

fixture <- function(lines, extension = ".csv") {
  path <- tempfile(fileext = extension)
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that("the example supports browsing, filtering, and bounded pagination", {
  testServer(server, {
    session$setInputs(search = "", page_size = "10", variable = "score",
                      groups = c("school", "class"))
    expect_true(dataset()$demo)
    expect_equal(nrow(current_data()), 36L)
    expect_equal(nrow(preview_data()), 10L)
    expect_equal(total_pages(), 4)
    first_page <- preview_data()
    session$setInputs(next_page = 1L)
    expect_equal(page(), 2L)
    expect_false(identical(first_page, preview_data()))
    session$setInputs(next_page = 2L)
    session$setInputs(next_page = 3L)
    session$setInputs(next_page = 4L)
    expect_equal(page(), 4)
    expect_equal(nrow(preview_data()), 6L)
    expect_match(output$preview_caption$html, "Page 4 of 4", fixed = TRUE)
    session$setInputs(search = current_data()$school[1L])
    expect_equal(page(), 1L)
    expect_equal(nrow(filtered_data()), 12L)
    session$setInputs(search = "THIS WILL NEVER MATCH A STUDENT")
    expect_equal(nrow(preview_data()), 0L)
    expect_match(output$preview_caption$html, "No matching rows", fixed = TRUE)
    session$setInputs(search = "", page_size = "25")
    expect_equal(nrow(preview_data()), 25L)
    session$setInputs(previous = 1L)
    expect_equal(page(), 1L)
  })
})

test_that("import honors delimiter, decimal and missing-value options", {
  path <- fixture(c("school;class;score;note", "North;A;12,5;absent",
                    "North;A;14,5;ok", "South;A;absent;ok"))
  on.exit(unlink(path))
  testServer(server, {
    session$setInputs(file = list(name = "study.csv", datapath = path),
      delimiter = ";", decimal = ",", header = TRUE, na_values = "absent",
      groups = c("school", "class"), variable = "score", import = 1L)
    expect_false(dataset()$demo)
    expect_equal(dataset()$name, "study.csv")
    expect_equal(current_data()$score, c(12.5, 14.5, NA))
    expect_true(is.na(current_data()$note[1]))
    expect_equal(profile()$missing, 2)
    expect_equal(status()$type, "success")
    expect_match(output$statistics$html, "13.5", fixed = TRUE)
    expect_match(output$import_status$html, "Imported 3 rows", fixed = TRUE)
    # Data exports contain all imported rows, independent of the preview filter.
    session$setInputs(search = "South")
    exported <- read.csv(output$download, check.names = FALSE)
    expect_equal(nrow(exported), 3L)
    expect_equal(exported$score, c(12.5, 14.5, NA))
  })
})

test_that("failed imports preserve the current dataset and expose useful errors", {
  malformed <- fixture(c("school,class", "North,A,extra", "South,B"))
  on.exit(unlink(malformed))
  testServer(server, {
    session$setInputs(import = 1L)
    expect_equal(status()$type, "error")
    expect_match(status()$text, "Choose a data file", fixed = TRUE)
    original <- current_data()
    session$setInputs(file = list(name = "broken.csv", datapath = malformed),
      delimiter = ",", decimal = ".", header = TRUE, na_values = "NA", import = 2L)
    expect_equal(status()$type, "error")
    expect_match(status()$text, "different numbers of fields", fixed = TRUE)
    expect_identical(current_data(), original)
    expect_match(output$import_status$html, 'role="alert"', fixed = TRUE)
    session$setInputs(file = list(name = "data.rds", datapath = malformed), import = 3L)
    expect_match(status()$text, "JSON", fixed = TRUE)
    expect_identical(current_data(), original)
  })
})

test_that("hierarchies preserve parent identity and escape imported labels", {
  path <- fixture(c("school,class,score", '"<script>alert(1)</script>",A,1',
                    '"<script>alert(1)</script>",A,2', "South,A,3", "South,B,4"))
  on.exit(unlink(path))
  testServer(server, {
    session$setInputs(file = list(name = "nested.csv", datapath = path),
      delimiter = ",", decimal = ".", header = TRUE, na_values = "NA",
      groups = c("school", "class"), variable = "score", import = 1L)
    groups <- grouped()
    expect_equal(nrow(groups), 5L)
    class_a <- groups[groups$label == "A", , drop = FALSE]
    expect_equal(nrow(class_a), 2L)
    expect_equal(length(unique(class_a$parent_id)), 2L)
    expect_equal(class_a$rows, c(2L, 1L))
    expect_equal(sum(groups$rows[groups$depth == 1L]), 4L)
    html <- as.character(output$hierarchy$html)
    expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
    expect_false(grepl("<script>", html, fixed = TRUE))
    expect_match(html, "2 rows", fixed = TRUE)
    expect_match(html, "South", fixed = TRUE)
    expect_false(grepl("<script>", output$preview, fixed = TRUE))
    exported <- read.csv(output$groups_download, check.names = FALSE)
    expect_equal(nrow(exported), 5L)
    expect_equal(exported$rows, groups$rows)
    session$setInputs(groups = character())
    expect_match(output$hierarchy$html, "Choose grouping columns", fixed = TRUE)
  })
})

test_that("text-only data shows a useful statistics empty state and demo can reset it", {
  path <- fixture(c("school,class", "North,A", "South,B"))
  on.exit(unlink(path))
  testServer(server, {
    session$setInputs(file = list(name = "labels.csv", datapath = path),
      delimiter = ",", decimal = ".", header = TRUE, na_values = "NA",
      groups = c("school", "class"), variable = "", import = 1L)
    expect_length(profile()$numeric_columns, 0L)
    expect_match(output$statistics$html, "No numeric columns yet", fixed = TRUE)
    expect_equal(nrow(current_data()), 2L)
    session$setInputs(sample = 1L)
    expect_true(dataset()$demo)
    expect_equal(nrow(current_data()), 36L)
    expect_null(status())
    expect_equal(nrow(read.csv(output$sample_download)), 36L)
  })
})

test_that("JSON records flow through preview, grouping, summaries and exports", {
  path <- fixture(c('[',
    '{"school":"North","class":"A","score":12.5},',
    '{"school":"North","class":"A","score":null},',
    '{"school":"South","class":"A","score":15.5}', ']'), ".json")
  on.exit(unlink(path))
  testServer(server, {
    session$setInputs(file = list(name = "observations.json", datapath = path),
      groups = c("school", "class"), variable = "score", import = 1L)
    expect_equal(status()$type, "success")
    expect_equal(dataset()$name, "observations.json")
    expect_match(output$import_options$html, "Nested object fields", fixed = TRUE)
    expect_equal(current_data()$score, c(12.5, NA, 15.5))
    expect_equal(profile()$missing, 1)
    expect_match(output$statistics$html, "14", fixed = TRUE)
    expect_match(output$preview, "North", fixed = TRUE)
    groups <- grouped()
    expect_equal(nrow(groups), 4L)
    expect_equal(groups$rows[groups$depth == 1], c(2L, 1L))
    expect_equal(nrow(read.csv(output$download)), 3L)
  })
})

test_that("Excel worksheet selection imports the chosen sheet and recovers from bad names", {
  path <- tempfile(fileext = ".xlsx")
  file.copy(readxl::readxl_example("datasets.xlsx"), path)
  on.exit(unlink(path))
  testServer(server, {
    session$setInputs(file = list(name = "workbook.xlsx", datapath = path),
      delimiter = ",", decimal = ".", header = TRUE, na_values = "NA", sheet = "mtcars",
      groups = character(), variable = "mpg", import = 1L)
    expect_equal(status()$type, "success")
    expect_equal(dataset()$name, "workbook.xlsx")
    expect_match(output$import_options$html, "Worksheet", fixed = TRUE)
    expect_match(output$import_options$html, "chickwts", fixed = TRUE)
    expect_equal(nrow(current_data()), 32L)
    expect_true("mpg" %in% profile()$numeric_columns)
    session$setInputs(sheet = "chickwts", variable = "weight", import = 2L)
    expect_equal(status()$type, "success")
    expect_equal(nrow(current_data()), 71L)
    expect_equal(names(current_data()), c("weight", "feed"))
    expect_equal(nrow(read.csv(output$download)), 71L)
    imported <- current_data()
    session$setInputs(sheet = "nonexistent worksheet", import = 3L)
    expect_equal(status()$type, "error")
    expect_match(tolower(status()$text), "sheet", fixed = TRUE)
    expect_identical(current_data(), imported)
  })
})
