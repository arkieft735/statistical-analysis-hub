# statistical-analysis-hub
Why pay for college if you'll forget hours of content and hundreds of formulas? My goal is to make the most of my biostatistics degree and catalog every new method I learn. Documentation of results, explanations, alternatives, and citations are my paramount objectives. 

## R Shiny workspace

The Shiny app provides a responsive workspace for importing tabular data, inspecting observations, exploring nested groups, and viewing descriptive statistics. It opens with an example study of 36 students in classes and schools.

### Run locally

Open `statistical-analysis-hub.Rproj` in RStudio, or start R from the repository root. The project uses `renv`; the lockfile records R 4.6.2 and Shiny 1.14.0. Use that R version for the closest match to the recorded environment.

Restore the application and test dependencies, then launch the app:

```r
renv::restore()
shiny::runApp("app")
```

The project `.Rprofile` activates `renv` automatically. On a fresh checkout it may bootstrap `renv` first. `renv::restore()` installs the locked packages, including `shiny`, `htmltools`, and `testthat`; an internet connection is required for missing packages.

### Import and explore data

1. Choose a UTF-8 `.csv`, `.tsv`, or delimited `.txt` file. Imports are limited to 50 MB, 100,000 data rows, and 200 columns.
2. Set the delimiter: comma, tab, semicolon, or pipe. TSV files select tab automatically; check the option before importing. Choose a period or comma decimal mark to match the source file.
3. Indicate whether the first row contains column names. Enter missing-value markers separated by commas; the defaults are `NA`, `N/A`, and `NULL`. Empty cells are always treated as missing. Blank column names receive generated names, and duplicate names are made unique.
4. Select **Import dataset**. A failed import displays an error and preserves the current dataset.

**Data preview** includes a search across all columns and pagination with 10, 25, or 50 rows per page. **Export imported data** downloads the complete imported dataset as CSV; the search and current page do not restrict the export. The source file is unchanged.

Use **Load example dataset** to return to the sample, or **Download example CSV** to try the import flow. Equivalent files are available in [`examples/classroom-study.csv`](examples/classroom-study.csv) and [`examples/classroom-study.tsv`](examples/classroom-study.tsv).

### Nested classes and groups

In **Nested groups**, choose up to eight columns from outermost to innermost. For example, select `school`, then `class`, then `student` to build a school → class → student hierarchy. Remove and reselect a column to change its position.

Each group displays its row count. Repeated labels such as “Class A” remain separate under different schools, and missing grouping values appear as `(Missing)`. The view displays the first 300 groups to keep the interface responsive. **Export group counts** includes the complete hierarchy, with group IDs, parent IDs, depth, labels, display paths, and row counts; IDs and parent IDs preserve nesting even when display paths contain the same text.

Hierarchy generation is limited to 250,000 groups. If a selection exceeds that limit, choose fewer levels or less detailed grouping columns before exporting.

### Descriptive statistics

Choose a numeric variable to see its mean, median, sample standard deviation, range, and histogram. Calculations use finite observations; missing values, `NaN`, and infinite values are excluded. These are descriptive summaries of the full imported dataset. They do not adjust for clustering, fit multilevel models, or establish that nested observations are independent.

### Session data

Uploaded files are sent to the R process running Shiny and processed on that server. Imported data is held in the current Shiny session; the app does not create a persistent data store. Running the app locally processes uploads on your own computer. If it is hosted elsewhere, uploads are processed on that host.

## Tests

From the repository root, after `renv::restore()`, run:

```sh
Rscript tests/test-data.R
Rscript tests/test-server.R
```

These commands load the project `.Rprofile` and its `renv` library. The data helpers also run without any external packages:

```sh
Rscript --vanilla tests/test-data.R
```

`--vanilla` skips `.Rprofile`, so explicitly activate the project library when using it for the Shiny server tests:

```sh
Rscript --vanilla -e 'source("renv/activate.R"); source("tests/test-server.R")'
```

The helper tests cover delimited imports, decimal marks, quoted fields, validation limits, missing values, numeric summaries, and nested group identity. The server tests exercise the reactive import and exploration workflows.
