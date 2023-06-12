#' Convert a temperature from Celsius to Fahrenheit
#'
#' This function converts a temperature from Celsius to Fahrenheit.
#'
#' @param farenTemp Temperature, in Fahrenheit
#' @return Temperature, in Celsius
#' @export
centToFaren <- function(farenTemp) {
  32 + (9/5) * farenTemp
}
