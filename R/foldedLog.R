#' Calculate a folded log  with the Tukey and Haldane-Anscombe corrections
#'
#' @param successes Integer: numerator frequency.
#' @param failures Integer: denominator frequency.
#' @return A 1 x 2 tibble, with columns "Tukey" and "HaldaneAnscombe".
#' @export
foldedLog <- function(successes, failures) {
  sf <- sFrac(successes, failures) %>% as.numeric()
  fl <- 1/2 * log(sf) - 1/2 * log(1 - sf)
  tb <- tibble(Tukey = fl[1], HaldaneAnscombe = fl[2])
  return(tb)
}
