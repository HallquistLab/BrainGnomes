test_that("mean FD combines tasks and deduplicates shared confounds across spaces", {
  skip_if_not_installed("plotly")
  inventory <- data.frame(record_id = letters[1:6],
    sub_id = c(rep("001", 4), "002", "002"), ses_id = NA_character_,
    run = c(NA, NA, "01", "01", "01", "01"), stage = "fmriprep",
    stream = NA_character_, input_stream = NA_character_, atlas = NA_character_,
    task = c("rest", "rest", "ridl", "ridl", "ridl", "rest"),
    space = c("MNI1", "MNI2", "MNI1", "MNI2", "MNI1", "MNI1"),
    resolution = NA_character_)
  metrics <- data.frame(record_id = letters[1:6], metric = "mean_fd",
    source_file = c("/001/rest.tsv", "/001/rest.tsv", "/001/ridl.tsv",
      "/001/ridl.tsv", "/002/ridl.tsv", "/002/rest.tsv"),
    value = c(0.1, 0.1, 0.2, 0.2, 0.3, 0.4), unit = "mm",
    n_observations = 100L, n_missing = 1L)
  plot <- plotly::plotly_build(qc_metrics_plot(metrics, inventory))
  expect_length(plot$x$data, 1L)
  trace <- plot$x$data[[1L]]
  expect_equal(trace$name, "Mean FD (all tasks)")
  expect_equal(sort(as.numeric(trace$y)), c(0.1, 0.2, 0.3, 0.4))
  expect_length(unique(trace$x), 4L)
  expect_true(any(grepl("task-rest", trace$x, fixed = TRUE)))
  expect_true(any(grepl("task-ridl", trace$x, fixed = TRUE)))
  expect_true(any(grepl("Observed: 100; missing: 1", trace$text, fixed = TRUE)))
  expect_setequal(unlist(plot$x$layout$xaxis$categoryarray), as.character(trace$x))
})

test_that("other metric groups retain task scope and reset their horizontal categories", {
  skip_if_not_installed("plotly")
  inventory <- data.frame(record_id = c("rest", "ridl"), sub_id = "001",
    ses_id = NA_character_, run = c(NA, "01"), stage = "postprocess", stream = "clean",
    input_stream = NA_character_, atlas = NA_character_, task = c("rest", "ridl"),
    space = "MNI", resolution = NA_character_)
  metrics <- data.frame(record_id = c("rest", "ridl", "rest", "ridl"),
    metric = c("censor_retained_percent", "censor_retained_percent", "mean_fd", "mean_fd"),
    source_file = c("/rest-mask.tsv", "/ridl-mask.tsv", "/rest-confounds.tsv", "/ridl-confounds.tsv"),
    value = c(90, 95, 0.1, 0.2), unit = c("%", "%", "mm", "mm"),
    n_observations = 100L, n_missing = 0L)
  plot <- plotly::plotly_build(qc_metrics_plot(metrics, inventory))
  expect_length(plot$x$data, 3L)
  expect_equal(plot$x$data[[1L]]$name, "Mean FD (all tasks)")
  expect_length(plot$x$data[[1L]]$y, 2L)
  expect_true(isTRUE(plot$x$data[[1L]]$visible))
  expect_true(all(vapply(plot$x$data[-1L], function(trace) !trace$visible, logical(1))))
  expect_setequal(unlist(plot$x$layout$xaxis$categoryarray), as.character(plot$x$data[[1L]]$x))
  buttons <- plot$x$layout$updatemenus[[1L]]$buttons
  for (i in seq_along(buttons)) {
    axis <- buttons[[i]]$args[[2L]]$xaxis
    expect_true(axis$autorange)
    expect_equal(axis$categoryorder, "array")
    expect_setequal(unlist(axis$categoryarray), as.character(plot$x$data[[i]]$x))
  }
})
