#' Convert a Bayes Factor to a g-prior
#'
#' @param bayesFactor the Bayes factor
#' @return The g-prior associated with the inputted Bayes factor
#' @export
bayesToG <- function(bayesFactor) {
  log(bayesFactor)/log(2)
}
