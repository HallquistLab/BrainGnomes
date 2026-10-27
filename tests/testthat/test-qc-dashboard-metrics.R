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
  rows <- qc_metric_sources(qc_metric_observations(metrics, inventory))
  expect_equal(length(unique(rows$group_id)), 1L)
  expect_equal(unique(rows$group_label), "Mean FD (all tasks) / mm")
  expect_equal(sort(rows$value), c(0.1, 0.2, 0.3, 0.4))
  expect_setequal(rows$task, c("rest", "ridl"))
  expect_true(all(rows$n_observations == 100L & rows$n_missing == 1L))
  plot <- plotly::plotly_build(qc_metrics_plot(metrics, inventory))
  expect_equal(plot$x$data[[1L]]$type, "histogram")
  expect_equal(sort(as.numeric(plot$x$data[[1L]]$x)), sort(rows$value))
})

test_that("other metric groups retain task scope", {
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
  rows <- qc_metric_sources(qc_metric_observations(metrics, inventory))
  expect_length(unique(rows$group_id), 3L)
  fd <- subset(rows, metric == "mean_fd")
  expect_equal(nrow(fd), 2L)
  expect_length(unique(fd$group_id), 1L)
  expect_length(unique(subset(rows, metric != "mean_fd")$group_id), 2L)
  expect_true(all(nzchar(rows$group_label)))
})

test_that("scope selection precedes source deduplication and unavailable values survive", {
  inventory <- data.frame(record_id = letters[1:4], sub_id = "001", ses_id = NA_character_,
    task = "rest", run = "01", stage = "fmriprep", stream = NA_character_,
    space = c("MNI1", "MNI2", "MNI1", "MNI1"))
  metrics <- data.frame(record_id = letters[1:4], metric = "mean_fd", unit = "mm",
    source_file = c("/shared.tsv", "/shared.tsv", NA, NA), value = c(.1, .1, NA, .3))
  rows <- qc_metric_observations(metrics, inventory)
  expect_equal(nrow(rows), 4L)
  expect_equal(nrow(qc_metric_sources(rows)), 3L)
  expect_equal(qc_metric_sources(subset(rows, space == "MNI2"))$value, .1)
  expect_true(anyNA(qc_metric_sources(rows)$value))
  expect_length(unique(rows$group_id), 1L)
  metrics$unit[4L] <- "different unit"
  expect_length(unique(qc_metric_observations(metrics, inventory)$group_id), 2L)
  expect_equal(nrow(qc_metric_observations(metrics[FALSE, ], inventory)), 0L)
})

test_that("comparison keys distinguish missing identities and separator-like labels", {
  inventory <- data.frame(record_id = letters[1:4], sub_id = "001",
    ses_id = c(NA, "", "A", "B"), task = "rest / nback", run = "01",
    stage = "postprocess", stream = "clean / MNI", space = "MNI")
  metrics <- data.frame(record_id = letters[1:4], metric = "censor_retained_percent", unit = "%",
    source_file = paste0("/", letters[1:4]), value = 95)
  expect_length(unique(qc_metric_observations(metrics, inventory)$group_id), 4L)
})

test_that("attention review groups products but keeps acquisition and reason identities", {
  rows <- data.frame(record_id = letters[1:3], sub_id = "001", ses_id = "A", task = "rest",
    run = c("01", "01", "02"), stage = "extract_rois", stream = "network",
    space = c("MNI1", "MNI2", "MNI1"), attention = TRUE,
    attention_reasons = "Saved audit failed; Diagnostics missing; Recorded ROI loss")
  review <- qc_attention_groups(rows)
  expect_equal(nrow(review), 6L)
  expect_true(all(subset(review, run == "01")$products == 2L))
  expect_setequal(review$category, c("Computational/workflow failure", "Missing/invalid evidence", "Descriptive review"))
  expect_equal(nrow(qc_attention_groups(rows[FALSE, ])), 0L)
  rows$echo <- c("1", "2", "1")
  expect_equal(nrow(qc_attention_groups(rows)), 9L)
  rows$attention_reasons <- "Diagnostics invalid; Saved audit ambiguous"
  expect_true(all(qc_attention_groups(rows)$category == "Missing/invalid evidence"))
})

test_that("relative evidence URLs resolve dot segments without requiring files", {
  expect_identical(qc_evidence_href("/unmounted/study/a & b.html", "/unmounted/study/report"),
    "../a%20%26%20b.html")
  expect_identical(qc_evidence_href("/unmounted/study/other/../a.html", "/unmounted/study/report/."),
    "../a.html")
})
