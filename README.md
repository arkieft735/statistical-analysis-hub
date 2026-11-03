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

Choose a data file, review the options, and select **Import data**. Imports are limited to 50 MB, 100,000 data rows, and 200 columns. A failed import displays an error and preserves the current dataset.

| Format | Options and structure |
| --- | --- |
| CSV, TSV, delimited TXT | UTF-8 text; comma, tab, semicolon, or pipe delimiter; period or comma decimal mark. TSV selects tab automatically. |
| Excel (`.xlsx`, `.xls`) | Choose a worksheet before importing. Dates and numeric cells retain their types. |
| JSON (`.json`) | A top-level array of record objects. Nested object fields become dotted column names, such as `student.id`. |
| JSON Lines (`.jsonl`, `.ndjson`) | One record object per nonblank line, with the same nested-field handling as JSON. |

Delimited and Excel imports offer header and missing-value settings. The default missing-value markers are `NA`, `N/A`, and `NULL`; empty cells are treated as missing. JSON `null` becomes a missing value. Blank column names receive generated names, and duplicate names are made unique. Nested arrays and other list-valued columns need to be converted into scalar columns before import.

**Data preview** includes a search across all columns and pagination with 10, 25, or 50 rows per page. **Export imported data** downloads the complete imported dataset as CSV; the search and current page do not restrict the export. The source file is unchanged.

Use **Use sample data** to return to the classroom study, or **Download sample CSV** to try the import flow. Examples are available as [CSV](examples/classroom-study.csv), [TSV](examples/classroom-study.tsv), [JSON](examples/classroom-study.json), and [JSON Lines](examples/classroom-study.jsonl). The JSON examples include a nested student object.

### Nested classes and groups

In **Nested groups**, choose up to eight columns from outermost to innermost. For example, select `school`, then `class`, then `student` to build a school → class → student hierarchy. Remove and reselect a column to change its position.

Each group displays its row count. Repeated labels such as “Class A” remain separate under different schools, and missing grouping values appear as `(Missing)`. The view displays the first 300 groups to keep the interface responsive. **Export group counts** includes the complete hierarchy, with group IDs, parent IDs, depth, labels, display paths, and row counts; IDs and parent IDs preserve nesting even when display paths contain the same text.

Hierarchy generation is limited to 250,000 groups. If a selection exceeds that limit, choose fewer levels or less detailed grouping columns before exporting.

### Descriptive statistics

In **Summary**, choose a numeric variable to see its mean, median, sample standard deviation, range, and histogram. Calculations use finite observations; missing values, `NaN`, and infinite values are excluded. These are descriptive summaries of the full imported dataset. They do not adjust for clustering, fit multilevel models, or establish that nested observations are independent.

### Session data

Uploaded files are sent to the R process running Shiny and processed on that server. Imported data is held in the current Shiny session; the app does not create a persistent data store. Running the app locally processes uploads on your own computer. If it is hosted elsewhere, uploads are processed on that host.

## Tests

From the repository root, after `renv::restore()`, run:

```sh
Rscript tests/test-data.R
Rscript tests/test-server.R
```

These commands load the project `.Rprofile` and its `renv` library. Core delimited-data tests also run without external packages; complete Excel and JSON coverage requires the restored dependencies:

```sh
Rscript --vanilla tests/test-data.R
```

`--vanilla` skips `.Rprofile`, so explicitly activate the project library when using it for the Shiny server tests:

```sh
Rscript --vanilla -e 'source("renv/activate.R"); source("tests/test-server.R")'
```

The helper tests cover delimited, Excel, JSON, and JSON Lines imports, quoted fields, validation limits, missing values, numeric summaries, and nested group identity. The server tests exercise the reactive import and exploration workflows, including Excel worksheet selection and JSON imports.

Design references and the restraint principles used for the interface are recorded in [DESIGN.md](DESIGN.md).
