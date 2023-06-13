#' The
#'
#' This function
#'
#' @param n Number of observations in the sample
#' @param R Number of samples. An integer that is at least 1.
#' @param d The minimum and maximum limits for the error
#' @return A dataset
#' @export
unifSampDist <- function(n, R, d) {
  1:R %>%
    map_dbl(~ runif(n = n, min = -1, max = 1) %>% mean()) %>%
    tibble(x = .) %>%
    mutate(close = ifelse((x > 0 - d) & (x < 0 + d), 1, 0)) %>%
    summarize(nc = mean(close)) %>%
    pull(nc)
}
