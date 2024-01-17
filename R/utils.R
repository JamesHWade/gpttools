# borrowed from scales::scientific()
# Wickham H, Pedersen T, Seidel D (2023). _scales: Scale Functions for
# Visualization_. R package version 1.3.0,
# <https://CRAN.R-project.org/package=scales>.
scales_scientific <- function(x,
                              digits = 3,
                              scale = 1,
                              prefix = "",
                              suffix = "",
                              decimal.mark = ".",
                              trim = TRUE,
                              ...) {
  if (length(x) == 0) {
    return(character())
  }
  x <- signif(x * scale, digits)
  ret <- paste0(prefix, format(x,
    decimal.mark = decimal.mark,
    trim = trim, scientific = TRUE, ...
  ), suffix)
  ret[is.na(x)] <- NA
  names(ret) <- names(x)
  ret
}
