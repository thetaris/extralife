## utility function to add required assets such as CSS and JS libraries
add_lib_assets <- function(lib, cdn = F, css = NULL) {
  assets = get_assets(get_lib(lib), cdn = cdn)
  if (!is.null(css)) {
    assets$css = c(assets$css, css)
  }
  styles <- lapply(assets$css, function(style) {
    sprintf("<link rel='stylesheet' href=%s>", style)
  })
  
  scripts <- lapply(assets$jshead, function(script) {
    sprintf("<script type='text/javascript' src=%s></script>", script)
  })
  cat(paste(c(styles, scripts), collapse = "\n"))
}

# get assets from online repositories
add_lib_assets("NVD3", cdn = TRUE, css = "http://rawgithub.com/ramnathv/rCharts/master/inst/libraries/nvd3/css/rNVD3.css")
