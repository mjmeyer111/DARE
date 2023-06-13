#' borrowed from vcd::distplot, written by Achim Zeileis
#'
#' @description Computes the data for diagnostic distribution plots:
#' poissonness, binomialness and negative binomialness plots.
#'
#' @param x either a vector of counts, a 1-way table of frequencies of counts
#' or a data frame or matrix with frequencies in the first column and the
#' corresponding counts in the second column.
#' @param type character. String indicating the distribution.
#' @param size the size argument for the binomial and negative binomial
#' distribution. If set to NULL and type is "binomial", then size is taken to
#' be the maximum count. If set to NULL and type is "nbinomial", then size is
#' estimated from the data.
#' @param lambda parameter of the poisson distribution. If type is "poisson"
#' and lambda is specified a leveled poissonness plot is produced.
#' @param conf_int logical. Should confidence intervals be calculated?
#' @param conf_level numeric. Confidence level for confidence intervals.
#' @return Returns a named list containing the parameter estimate(s), the
#' legend text, the plot title, phat, and a tibble with data used to
#' construct the plot.
#' @references D. C. Hoaglin (1980), A poissonness plot, The American
#' Statistican, 34, 146--149.
#'
#' D. C. Hoaglin & J. W. Tukey (1985), Checking the shape of discrete
#' distributions. In D. C. Hoaglin, F. Mosteller, J. W. Tukey (eds.),
#' Exploring Data Tables, Trends and Shapes, chapter 9. John Wiley & Sons,
#' New York.
#'
#' M. Friendly (2000), Visualizing Categorical Data. SAS Institute, Cary, NC.
#' @examples
#' set.seed(1111)
#' rb <- tibble(k = rbinom(10000, 20, 0.8))
#' ddOut <- discDist(rb$k, type = "binomial", size = 20)
#' forPlot <- ddOut$forPlot
#' ggplot(data = forPlot, aes(x = count, y = metameter)) +
#' geom_point(color = "red") +
#' geom_abline(slope = ddOut$estimates$slope, intercept = ddOut$estimates$intercept) +
#' geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
#' @importFrom vcd goodfit
#' @importFrom magrittr pipe
#' @import tibble
#' @export
discDist <- function (
    x,
    type = c("poisson", "binomial", "nbinomial"),
    size = NULL,
    lambda = NULL,
    conf_int = TRUE,
    conf_level = 0.95
)
{
  if (is.vector(x)) {
    x <- table(x)
  }
  if (is.table(x)) {
    if (length(dim(x)) > 1)
      stop("x must be a 1-way table")
    freq <- as.vector(x)
    count <- as.numeric(names(x))
  }
  else {
    if (!(!is.null(ncol(x)) && ncol(x) == 2))
      stop("x must be a 2-column matrix or data.frame")
    freq <- as.vector(x[, 1])
    count <- as.vector(x[, 2])
  }
  myindex <- (1:length(freq))[freq > 0]
  mycount <- count[myindex]
  myfreq <- freq[myindex]
  switch(match.arg(type), poisson = {
    par.ml <- suppressWarnings(goodfit(x, type = type)$par$lambda)
    phi <- function(nk, k, N, size = NULL) ifelse(nk > 0,
                                                  lgamma(k + 1) + log(nk/N), NA)
    y <- phi(myfreq, mycount, sum(freq))
    if (!is.null(lambda)) y <- y + lambda - mycount * log(lambda)
    fm <- lm(y ~ mycount)
    par.estim <- exp(coef(fm)[2])
    names(par.estim) <- "lambda"
    txt <- "exp(slope)"
    if (!is.null(lambda)) {
      par.estim <- par.estim * lambda
      txt <- paste(txt, "x lambda")
    }
  }, binomial = {
    if (is.null(size)) {
      size <- max(count)
      warning("size was not given, taken as maximum count")
    }
    par.ml <- suppressWarnings(goodfit(x,
                                       type = type,
                                       par = list(size = size))$par$prob)
    phi <- function(nk, k, N, size = NULL)
      log(nk) - log(N * choose(size, k))
    y <- phi(myfreq, mycount, sum(freq), size = size)
    fm <- lm(y ~ mycount)
    par.estim <- exp(coef(fm)[2])
    par.estim <- par.estim/(1 + par.estim)
    names(par.estim) <- "prob"
  }, nbinomial = {
    if (is.null(size)) {
      par.ml <- suppressWarnings(goodfit(x, type = type)$par)
      size <- par.ml$size
      par.ml <- par.ml$prob
    } else {
      xbar <- weighted.mean(mycount, myfreq)
      par.ml <- size/(size + xbar)
    }
    phi <- function(nk, k, N, size = NULL)
      log(nk) - log(N * choose(size + k - 1, k))
    y <- phi(myfreq, mycount, sum(freq), size = size)
    fm <- lm(y ~ mycount)
    par.estim <- 1 - exp(coef(fm)[2])
    names(par.estim) <- "prob"
  })
  yhat <- ifelse(myfreq > 1.5, myfreq - 0.67, 1/exp(1))
  yhat <- phi(yhat, mycount, sum(freq), size = size)
  if (!is.null(lambda))
    yhat <- yhat + lambda - mycount * log(lambda)
  phat <- myfreq/sum(myfreq)
  ci.width <-
    qnorm(1 - (1 - conf_level)/2) *
    sqrt(1 - phat)/sqrt(myfreq - (0.25 * phat + 0.47) * sqrt(myfreq))
  RVAL <- cbind(count, freq, NA, NA, NA, NA, NA)
  RVAL[myindex, 3:7] <- cbind(y,
                              yhat,
                              ci.width,
                              yhat - ci.width,
                              yhat + ci.width)
  forPlot <- cbind(phat, RVAL) %>%
    as.data.frame(.)
  colnames(forPlot) <- c("p.hat", "count", "freq", "metameter", "ci.center", "ci.width", "lower", "upper")
  list(forPlot = forPlot,
       par.estim = par.estim,
       slope = coef(fm)[2] %>% as.numeric(),
       intercept = coef(fm)[1] %>% as.numeric()) %>%
    return()
}
