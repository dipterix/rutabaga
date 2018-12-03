# lifecycle
lifecycle <- function(stage) {
  substr(stage, 1, 1) <- toupper(substr(stage, 1, 1))
  sprintf(
    "\\ifelse{html}{%s}{\\strong{%s}}", substr
  )
}
