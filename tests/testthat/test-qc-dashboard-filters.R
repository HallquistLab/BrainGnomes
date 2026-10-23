test_that("shared filter payloads preserve arrays for zero, one, and many fields", {
  skip_if_not_installed("reactable")
  cases <- list(data.frame(value = 1), data.frame(sub_id = "001"),
    data.frame(sub_id = "001", ses_id = NA_character_, stage = "postprocess"))
  for (i in seq_along(cases)) {
    widget <- qc_reactable(cases[[i]], id = paste0("filter-test-", i))
    payload <- jsonlite::fromJSON(htmlwidgets:::toJSON(widget$jsHooks$render[[1L]]$data),
      simplifyVector = FALSE)
    expect_type(payload$fields, "list")
    expect_equal(unlist(payload$fields, use.names = FALSE),
      if (i == 1L) NULL else intersect(names(cases[[i]]), c("sub_id", "ses_id", "stage")))
  }
})

test_that("shared filters initialize subsequent widgets with scalar and array fields", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is needed to execute the browser filter regression")
  asset <- system.file("qc_dashboard", "dashboard.js", package = "BrainGnomes", mustWork = TRUE)
  script <- test_path("fixtures", "qc-dashboard-filters.cjs")
  output <- system2(node, c(shQuote(script), shQuote(asset)), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "Shared filter regression passed", fixed = TRUE)
})
