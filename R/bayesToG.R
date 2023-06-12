#' Calculate a folded log  with the Tukey and Haldane-Anscombe corrections
#'
#' @param bayesFactor
#' @return The g-prior associated with the inputted Bayes factor
#' @export
bayesToG <- function(bayesFactor) {
  log(bayesFactor)/log(2)
}
