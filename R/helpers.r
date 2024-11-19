# Scale between 0-1 using min-max scaling to keep distribution
# min_max_scale <- function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
min_max_scale <- function(x) {
  range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (range == 0) {
    return(rep(1, length(x)))
  } else {
    (x - min(x, na.rm = TRUE)) / range
  }
}