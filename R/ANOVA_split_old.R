#' Splitting data to construct overlays that mimic the four sections of
#' data used to calculate a one-way ANOVA.
#'
#' @param x Either an independent variable (as a character), or a formula
#' in the form dv ~ iv, where dv is a dependent variable, and iv is an
#' independent variable.
#' @param y Character: What is the name of the dependent variable?
#' @param data tibble or data frame: dataset containing the variables.
#' @param common function: which function should represent the common in the
#' splitting algorithm? Note that the function must have an na.rm argument.
#' @param print.statistic Logical: should the test statistic be printed in the
#' source table?
#' @return a list of five tibbles; the first four representing the four overlays --
#' the data, the common, the condition, and the residuals -- and a fifth tibble
#' showing the source table of the analysis.
#' @examples
#'\dontrun{
#' # using dplyr to see the data split by condition, assuming a mean common
#' ToothGrowth %>% group_by(supp) %>% summarize(mn = mean(len))
#'
#' # using ANOVA_split
#' ANOVA_split("supp", "len", ToothGrowth)
#' ANOVA_split("supp", "len", ToothGrowth, common = median)
#' ANOVA_split("dose", "len", ToothGrowth)
#'
#' # using ANOVA_split with formula
#' ANOVA_split(len~supp, ToothGrowth)
#'
#' # Showing that the sums of the overlays add up to the data
#'
#' toothGrowthOverlays <- ANOVA_split("supp", "len", ToothGrowth)
#' SS_common <- toothGrowthOverlays$overlays$common %>% .^2 %>% sum()
#' SS_condition <- toothGrowthOverlays$overlays$condition %>% .^2 %>% sum()
#' SS_residuals <- toothGrowthOverlays$overlays$residuals %>% .^2 %>% sum()
#' SS_data <- toothGrowthOverlays$overlays$data %>% .^2 %>% sum()
#' SS_overlays <- SS_common + SS_condition + SS_residuals
#' all.equal(SS_data, SS_overlays)
#'}
#' @export
ANOVA_split <- function(x, ...){
  UseMethod('ANOVA_split')
}

#' @export
ANOVA_split.default <- function(x, y, data, common = mean, print.statistic = F){

  data <- data[,c(y, x)]

  data.w <- unstack(data, formula(paste0(y, "~", x)))

  maxN_Group <- max(sapply(data.w, length))

  data.w <- as.data.frame(sapply(data.w, function(x){
    if(length(x) < maxN_Group){
      return(c(x,rep(NA, maxN_Group-length(x))))
    }
    return(x)
  }))

  # Split the mood data into overlays
  data.mn <- sapply(data.w, mean, na.rm = TRUE)

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
  dataOverlay <- as.data.frame(matrix(data.eff, nrow = nrow(data.w),
                                      ncol = ncol(data.w),
                                      byrow = TRUE))
  names(dataOverlay) <- names(data.eff)

  # (4) residuals
  names(dataResid) <- names(data.eff)

  # calculate df's
  dfCondition <- ncol(data.w) - 1
  dfResiduals <- nrow(data) - ncol(data.w)
  dfCommon <- 1
  dfData <- nrow(data)

  # calculate sums of squares
  ssCondition <- sum((dataOverlay)^2)
  ssResiduals <- sum((dataResid)^2)
  ssCommon <- sum((dataCommonOverlay)^2)
  ssData <- sum((data.w)^2)

  # calculate mean squares
  msCondition <- ssCondition/dfCondition
  msResiduals <- ssResiduals/dfResiduals

  # calculate test statistic
  fStatistic <- msCondition/msResiduals

  # create source table
  sourceTable <- tibble::tibble("Source" = c("Condition", "Residuals", "Common", "Data"),
                                "Df" = c(dfCondition, dfResiduals, dfCommon, dfData),
                                "SumSq" = c(ssCondition, ssResiduals, ssCommon, ssData),
                                "MeanSq" = c(msCondition, msResiduals, NA, NA))

  # Add F statistic
  if(print.statistic){
    sourceTable <- sourceTable %>%
      tibble::add_column(FStatistic = c(fStatistic, NA, NA, NA))
  }

  return(list("overlays" = list("data"=tibble::tibble(data.w),
              "common"=tibble::tibble(dataCommonOverlay),
              "condition"=tibble::tibble(dataOverlay),
              "residuals"=tibble::tibble(dataResid)),
              "sourceTable" = sourceTable))
}

#' @export
ANOVA_split.formula <- function(x, data, common = mean, print.statistic = F){
  dvFormula <- as.character(x[[2]])
  ivFormula <- as.character(x[[3]])
  ANOVA_split.default(ivFormula, dvFormula, data, common, print.statistic)
}
