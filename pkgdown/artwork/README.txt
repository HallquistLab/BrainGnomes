BrainGnomes package artwork

The approved images are the assembly-line designs with distinct gnome faces,
continuous conveyors, and no directional arrows (revision v3):

  man/figures/logo.png                1254 x 1254 RGBA PNG
  man/figures/braingnomes-header.png   2172 x 724 RGB PNG

The README and package overview help display both images, with a small hex
badge and a wide rectangular header. pkgdown discovers logo.png automatically
for page headings, copies the README header into the home page, and uses the
header for the Open Graph social-preview image configured in _pkgdown.yml.

Browser icons live in pkgdown/favicon and are copied into the site by
pkgdown::init_site(). They are derived locally from the official logo; site
builds do not need to upload artwork to a favicon service. To regenerate
after changing the approved logo, run from the repository root:

  Rscript pkgdown/artwork/build-favicons.R

This maintainer script requires png, ragg, and openssl. These are development
tools, not package runtime dependencies. The favicon SVG embeds a raster
derivative of the logo; it is not a vector source for the illustration.

The ignored local archive/ preserves earlier designs and generation prompts;
these drafts are not shipped in Git or release packages. The
first pair formerly at the canonical image paths is retained as
logo-original.png and braingnomes-header-original.png. Historical paths in
the archived prompts describe where files were saved during generation;
their corresponding versions now live in archive/. The selected v3 masters
are also archived so the canonical files can be checked against them.

The existing ^pkgdown$ entry in .Rbuildignore excludes this entire directory
from R package builds. Only the approved images remain in man/figures, so
earlier designs do not inflate installed packages or the generated website.
