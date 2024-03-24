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
ANOVA_split <- function(x, data, print.statistic = F){# contrast = contr.sum,

  IVs <- all.vars(x[-2])
  DV <- as.character(x[2])
  data <- data[do.call("order", data[IVs]), ]

  # if only one contrast is supplied, construct a list of contrasts with length
  # equal to the number of IVs
  # if(length(contrast)==1){
  #   contrast <- lapply(1:length(IVs),function(x){contrast})
  # }

  numLevels <- sapply(IVs, function(x){length(unique(data[[x]]))})
  numRows <- nrow(data)

  # convert all IVs to factors, if they aren't already
  for(i in IVs){
    data[[i]] <- factor(data[[i]])
    # contrasts(data[[i]]) <- contrast[[i]](numLevels[i])
    # contrasts(data[[i]]) <- contrast(numLevels[i])
    tempContrast <- contr.sum(numLevels[i])
    contrasts(data[[i]]) <- tempContrast
  }

  # linear model
  splitLm <- lm(x, data)
  levels(splitLm$model$supp)
  summaryLm <- summary(splitLm)
  anovaLm <- car::Anova(splitLm, type = "II")

  # coefficients and residuals
  coefsLm <- coefficients(summaryLm)
  residLm <- residuals(summaryLm)

  # Split the data into overlays
  # Calculate dimensions for overlay arrays
  numPerCell <- ceiling(numRows/prod(numLevels))
  overlayDim <- numLevels
  print(overlayDim)
  overlayDim <- c(numPerCell, overlayDim)
  permOrder <- c(2,1)
  # To do: do we want the order to be 2, 1, 4, 3 or 2, 1, 3, 4 in the three-way case?
  if(length(overlayDim) > 2){
    permOrder <- c(permOrder, max(length(overlayDim), 3):3)
  }

  # data overlay
  # if only one IV, use unstack; otherwise, use acast
  if(length(IVs)==1){
    data.w <- unstack(data, x)
  } else {
    # if using acast, check if there will be more than one value per cell
    if(numPerCell>1){
      # if so, use ave function to create an id "column", and reshape on that too
      # goal: acast(data, ave()~acastFormula, value.var = DV)
      # TODO: what if partId is already a column in the dataset?
      data$partId <- ave(1:numRows, data[,IVs], FUN = seq_along)
      acastFormula <- formula(paste(c("partId",IVs),collapse="~"))
    } else {
      # if not, don't use ave function, and just run acast(data, acastFormula, value.var = DV)
      acastFormula <- formula(paste(IVs,collapse="~"))
    }
    #acastFormula <- formula(paste(IVs,collapse="~"))
    data.w <- reshape2::acast(data,
                              acastFormula,
                              value.var=DV)
  }

  # Common overlay
  dataCommon <- array(coefsLm[1,1], dim = overlayDim)

  # Residual overlay
  dataResid <- array(residLm, dim = overlayDim)

  # Effects overlays
  # Goal:
  # Supp: aperm(array(coefsX, dim = c(2,10,3)),perm = c(2,1,3))
  # Dose: aperm(array(coefsX, dim = c(3,10,2)),perm = c(2,1,3))
  # Interaction: ???

  termsLm <- dplyr::distinct(coefplot:::matchCoefs.default(splitLm)[-1,1:2])
  data.eff <- lapply(1:length(unique(termsLm[,1])),function(i){
    x <- unique(termsLm[,1])[i]
    coefNamesX <- termsLm$Coefficient[termsLm$Term==x]
    coefsX <- coef(summaryLm)[coefNamesX, 1]
    coefsX <- c(coefsX, -sum(coefsX))
    # TODO: what happens when we have more than two variables?
    # 3 variables:
    # [1] 2 1 3
    # [1] 3 1 2
    # [1] 4 1 2 3
    # 4 variables:
    # [1] 2 1 3
    # [1] 3 1 2
    # [1] 4 1 2 3
    # [1] 5 1 2 3
    overlayOrder <- c(i+1, (1:(length(unique(termsLm[,1]))+(numPerCell > 1)))[-(i+1)])
    # what happens when there are 3+ IVs?
    return(aperm(array(coefsX, dim = overlayDim[overlayOrder]),
                 perm = permOrder))
  })
  names(data.eff) <- unique(termsLm[,1])

  # Return overlays and source/summary tables
  return(list("overlays" = list("data"=data.w,
                                "common"=dataCommon,
                                "condition"=data.eff,
                                "residuals"=dataResid),
              "sourceTable" = anovaLm,
         "summaryTable" = summaryLm))
}
