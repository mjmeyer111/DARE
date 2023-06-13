#' Splitting your data to construct overlays that mimic the four sections of
#' data used to calculate a one-way ANOVA.
#'
#' @param dv Character: What is the name of your dependent variable?
#' @param iv Character: What is the name of your independent variable?
#' @param data tibble or data frame: Your dataset containing your variables.
#' @param common function: which function should represent the common in the
#' splitting algorithm?
#' @return a list of four tibbles, representing the four overlays: the data,
#' the common, the condition, and the residuals.
#' @examples
#'\dontrun{
#' # using tapply to see the data split by condition, assuming a mean common
#' tapply(mtcars$wt,mtcars$am,mean)
#'
#' # using ANOVA_overlay
#' ANOVA_overlay("wt", "am", mtcars)
#' ANOVA_overlay("wt", "am", mtcars, common = median)
#' ANOVA_overlay("wt", "gear", mtcars)
#'}
#' @export
ANOVA_overlay <- function(dv, iv, data, common = mean){

  data <- data[,c(dv, iv)]

  data.w <- unstack(data, formula(paste0(dv, "~", iv)))

  maxN_Group <- max(sapply(data.w, length))

  data.w <- as.data.frame(sapply(data.w, function(x){
    if(length(x) < maxN_Group){
      return(c(x,rep(NA, maxN_Group-length(x))))
    }
    return(x)
  }))

  # Split the mood data into overlays
  data.mn <- sapply(data.w, common, na.rm = TRUE)

  # sweep:
  dataResid <- sweep(data.w, MARGIN = 2, STATS = data.mn)

  #taking a value, and subtracting it from
  #all of the values in an overlay to get another overlay
  # common
  dataCommon <- common(data.mn, na.rm = TRUE)

  # sweep common out condition means to get the effect of condition
  data.eff <- data.mn - dataCommon

  # turn into four overlays
  # (1) data
  data.w

  # (2) common
  dataCommonOverlay <- as.data.frame(matrix(dataCommon, nrow = nrow(data.w),
                                            ncol = ncol(data.w)))
  names(dataCommonOverlay) <- names(data.eff)

  # (3) condition
  dataOverlay <- as.data.frame(matrix(data.mn, nrow = nrow(data.w),
                                      ncol = ncol(data.w),
                                      byrow = TRUE))
  names(dataOverlay) <- names(data.mn)

  # (4) residuals
  names(dataResid) <- names(data.mn)

  return(list("data"=tibble::tibble(data.w),
         "common"=tibble::tibble(dataCommonOverlay),
         "condition"=tibble::tibble(dataOverlay),
         "residuals"=tibble::tibble(dataResid)))
}
