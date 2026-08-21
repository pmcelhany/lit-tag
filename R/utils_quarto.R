#' Escape LaTeX special characters
#'
#' @param t A character string or vector
#'
#' @return A character string or vector with LaTeX special characters escaped
#' @noRd
escape_latex <- function(t) {
  if (is.null(t)) {
    return(NULL)
  }
  # Use a very specific placeholder for backslashes to avoid collisions
  placeholder <- "RESERVED_BACKSLASH_PLACEHOLDER_XYZ"
  t <- gsub("\\", placeholder, t, fixed = TRUE)
  t <- gsub("&", "\\&", t, fixed = TRUE)
  t <- gsub("%", "\\%", t, fixed = TRUE)
  t <- gsub("$", "\\$", t, fixed = TRUE)
  t <- gsub("#", "\\#", t, fixed = TRUE)
  t <- gsub("_", "\\_", t, fixed = TRUE)
  t <- gsub("{", "\\{", t, fixed = TRUE)
  t <- gsub("}", "\\}", t, fixed = TRUE)
  t <- gsub("~", "\\textasciitilde{}", t, fixed = TRUE)
  t <- gsub("^", "\\textasciicircum{}", t, fixed = TRUE)
  t <- gsub(placeholder, "\\textbackslash{}", t, fixed = TRUE)
  return(t)
}
