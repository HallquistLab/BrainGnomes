# Derive browser icons from the approved package logo, without network access.
# Input: repository root as the optional first command-line argument.
# Output: PNG, SVG, ICO, and manifest files in pkgdown/favicon/.
# Run from the repository root: Rscript pkgdown/artwork/build-favicons.R

root <- if (length(commandArgs(trailingOnly = TRUE))) {
  commandArgs(trailingOnly = TRUE)[[1]]
} else {
  "."
}
logo <- png::readPNG(file.path(root, "man", "figures", "logo.png"))
icon_dir <- file.path(root, "pkgdown", "favicon")
dir.create(icon_dir, recursive = TRUE, showWarnings = FALSE)

# Render the complete logo at browser sizes, retaining its transparent exterior.
icon_names <- c(
  "favicon-96x96.png" = 96L,
  "apple-touch-icon.png" = 180L,
  "web-app-manifest-192x192.png" = 192L,
  "web-app-manifest-512x512.png" = 512L
)
ico_sizes <- c(16L, 32L, 48L)
ico_files <- character(length(ico_sizes))
for (size in unique(c(unname(icon_names), ico_sizes))) {
  output <- tempfile(fileext = ".png")
  ragg::agg_png(output, width = size, height = size, units = "px",
    background = "transparent")
  grid::grid.raster(logo, width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"), interpolate = TRUE)
  grDevices::dev.off()
  for (name in names(icon_names)[icon_names == size]) {
    file.copy(output, file.path(icon_dir, name), overwrite = TRUE)
  }
  if (size %in% ico_sizes) {
    ico_files[match(size, ico_sizes)] <- output
  } else {
    unlink(output)
  }
}

# ICO supports PNG payloads. Write a directory of three sizes, followed by
# each complete PNG, so browsers can select an icon without rescaling it.
ico <- file(file.path(icon_dir, "favicon.ico"), "wb")
writeBin(as.integer(c(0, 1, length(ico_sizes))), ico, size = 2, endian = "little")
offset <- 6L + 16L * length(ico_sizes)
for (i in seq_along(ico_sizes)) {
  bytes <- as.integer(file.info(ico_files[[i]])$size)
  writeBin(as.raw(c(ico_sizes[[i]], ico_sizes[[i]], 0, 0)), ico)
  writeBin(as.integer(c(1, 32)), ico, size = 2, endian = "little")
  writeBin(as.integer(c(bytes, offset)), ico, size = 4, endian = "little")
  offset <- offset + bytes
}
for (path in ico_files) {
  writeBin(readBin(path, "raw", n = file.info(path)$size), ico)
}
close(ico)
unlink(ico_files)

# pkgdown's standard head template references these SVG and manifest names.
svg_icon <- openssl::base64_encode(readBin(
  file.path(icon_dir, "web-app-manifest-192x192.png"), "raw",
  n = file.info(file.path(icon_dir, "web-app-manifest-192x192.png"))$size
))
writeLines(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 192 192">',
  '<image width="192" height="192" href="data:image/png;base64,',
  svg_icon, '"/></svg>'
), file.path(icon_dir, "favicon.svg"))
writeLines(c(
  '{',
  '  "name": "BrainGnomes",',
  '  "short_name": "BrainGnomes",',
  '  "icons": [',
  '    {"src": "web-app-manifest-192x192.png", "sizes": "192x192", "type": "image/png"},',
  '    {"src": "web-app-manifest-512x512.png", "sizes": "512x512", "type": "image/png"}',
  '  ],',
  '  "display": "standalone"',
  '}'
), file.path(icon_dir, "site.webmanifest"))
message("Browser icons written to ", icon_dir)
