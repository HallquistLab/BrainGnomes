test_that("release metadata and changelog identify the same release", {
  root <- test_path("..", "..")
  description <- file.path(root, "DESCRIPTION")
  news <- file.path(root, "NEWS.md")
  skip_if_not(file.exists(description) && file.exists(news),
    "Source release metadata is not installed with the package")
  metadata <- read.dcf(description)[1L, ]
  expect_identical(readLines(news, n = 1L),
    paste0("# BrainGnomes ", metadata[["Version"]], " (", metadata[["Date"]], ")"))
  expect_false(is.na(as.Date(metadata[["Date"]])))
})

test_that("package help ships a small hex badge and a wide header", {
  path <- test_path("..", "..", "man", "BrainGnomes-package.Rd")
  rd <- if (file.exists(path)) tools::parse_Rd(path) else {
    utils:::.getHelpFile(utils::help("BrainGnomes-package", package = "BrainGnomes"))
  }
  topic <- paste(unlist(rd), collapse = "")
  expect_match(topic, "logo.png", fixed = TRUE)
  expect_match(topic, "width='120'", fixed = TRUE)
  expect_match(topic, "braingnomes-header.png", fixed = TRUE)
  expect_match(topic, "max-width: 100%", fixed = TRUE)

  for (asset in c("logo.png", "braingnomes-header.png")) {
    path <- test_path("..", "..", "man", "figures", asset)
    if (!file.exists(path)) path <- system.file("help", "figures", asset,
      package = "BrainGnomes", mustWork = TRUE)
    connection <- file(path, "rb")
    dimensions <- tryCatch({
      expect_identical(readBin(connection, "raw", n = 8L),
        as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
      seek(connection, 16L)
      readBin(connection, "integer", n = 2L, size = 4L, endian = "big")
    }, finally = close(connection))
    if (asset == "logo.png") expect_equal(dimensions[1L], dimensions[2L])
    else expect_gte(dimensions[1L] / dimensions[2L], 2.5)
  }
})

test_that("website and README reuse the approved package artwork", {
  root <- test_path("..", "..")
  config <- file.path(root, "_pkgdown.yml")
  skip_if_not(file.exists(config), "Website sources are not installed with the package")
  readme <- paste(readLines(file.path(root, "README.md")), collapse = "\n")
  expect_match(readme,
    '<img src="man/figures/logo.png" align="right" width="120" height="120" alt=',
    fixed = TRUE)
  expect_match(readme, '<img src="man/figures/braingnomes-header.png"', fixed = TRUE)
  image <- yaml::read_yaml(config)$template$opengraph$image
  expect_identical(image$src, "man/figures/braingnomes-header.png")
  expect_true(nzchar(image$alt))
  expect_true(file.exists(file.path(root, image$src)))
  icons <- file.path(root, "pkgdown", "favicon")
  expect_true(all(file.exists(file.path(icons,
    c("favicon.ico", "favicon.svg", "favicon-96x96.png", "apple-touch-icon.png")))))
  manifest <- jsonlite::read_json(file.path(icons, "site.webmanifest"))
  expect_identical(manifest$name, "BrainGnomes")
  expect_true(all(vapply(manifest$icons,
    function(icon) file.exists(file.path(icons, icon$src)), logical(1))))
})
