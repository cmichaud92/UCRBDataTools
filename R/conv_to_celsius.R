#' Convert temperture captured in Fahrenheit to celsius
#'
#' @param .x the field containing temperatures to convert
#'
#' @return A data frame
#' @export
#'
#' @examples
conv_to_celsius <- function(.x) {
  round((.x - 32) / 1.8, 1)
}
