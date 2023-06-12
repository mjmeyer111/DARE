#' Calculate a started fraction with the Tukey and Haldane-Anscombe corrections
#'
#' @param successes Integer: numerator frequency.
#' @param failures Integer: denominator frequency.
#' @return A 1 x 2 tibble, with columns "Tukey" and "HaldaneAnscombe".
#' @export
sFrac <- function(successes, failures, total_count = successes + failures) {
  t <- 1/6
  ha <- 1/2
  sft <- (successes + t)/(total_count + 2 * t)
  sfha <- (successes + ha)/(total_count + 2 * ha)
  tb <- tibble(Tukey = sft, HaldaneAnscombe = sfha)
  return(tb)
}
