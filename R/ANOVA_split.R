#' Splitting data to construct overlays that mimic the four sections of
#' data used to calculate a one-way ANOVA.
#'
#' @param x Either an independent variable (as a character), or a formula
#' in the form dv ~ iv, where dv is a dependent variable, and iv is an
#' independent variable.
#' @param y Character: What is the name of the dependent variable?
#' @param data tibble or data frame: Dataset containing the variables.
#' @param splitLm an lm object.
#' @param print.statistic Logical: Should the test statistic be printed in the
#' source table?
#' @param show.warnings Logical: Should warnings be shown when converting independent
#' variables to factors and converting contrasts to sum contrasts?
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
#' # using ANOVA_split with lm
#' ANOVA_split(lm(len~supp, ToothGrowth))
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
ANOVA_split.default <- function(x, y, data, print.statistic = F, show.warnings = F){
  modelFormula <- as.formula(paste0(y, "~", x))
  ANOVA_split.formula(modelFormula, data, print.statistic, show.warnings)
}

# To do:
# Remove ANOVA p-values (keep it, but remove it as the default)
# ANOVA: add MSE, corrected MSE, AIC + BIC
# Replace if-else statement with more generalizable code
# Bring back dimensional arrays - see table 6-9 (pg. 131) in Exploratory
# Analysis of Variance (create top half [the non-residual part] bordered
# table as shiny, where you can click on any piece to have it show the
# residuals)
# Don't do more a three-way (at least for now)
#
# Add method for lm -> if we're pulling from lm, add warnings about switching to contr.sum contrasts
# rather than lm method calling formula method, make the formula method call the lm method
#
#
# Mixed effects version of below function
# lme4 function (lmList) -> run each subject separately and give you a summary
#
#

#' @export
ANOVA_split.formula <- function(x, data, print.statistic = F, show.warnings = F){
  IVs <- all.vars(x[-2])
  numLevels <- sapply(IVs, function(x){length(unique(data[[x]]))})

  # convert all IVs to factors and use sum contrasts, if this isn't the case already
  for(i in IVs){
    if(!is.factor(data[[i]])){
      if(show.warnings){
        warning("The variable ",i," is not a factor. Converting to a factor before proceeding.")
      }
      data[[i]] <- factor(data[[i]])
    }
    tempContrast <- contr.sum(numLevels[i])
    if(!all(contrasts(data[[i]])==tempContrast)){
      if(show.warnings){
        warning("Changing existing contrasts for ",i," to sum contrasts using contr.sum().")
      }
      contrasts(data[[i]]) <- tempContrast
    }
  }

  splitLm <- lm(x, data)
  ANOVA_split.lm(splitLm, print.statistic, show.warnings)
}

#' @export
ANOVA_split.lm <- function(splitLm, print.statistic = F, show.warnings = F){

  splitFormula <- as.formula(splitLm)
  splitData <- splitLm$model

  IVs <- all.vars(splitFormula[-2])
  DV <- as.character(splitFormula[2])
  splitData <- splitData[do.call("order", rev(splitData[IVs])), ]

  numLevels <- sapply(IVs, function(x){length(unique(splitData[[x]]))})
  numRows <- nrow(splitData)

  # convert all IVs to factors and use sum contrasts, if this isn't the case already
  for(i in IVs){
    if(!is.factor(splitData[[i]])){
      if(show.warnings){
        warning("The variable ",i," is not a factor. Converting to a factor before proceeding.")
      }
      splitData[[i]] <- factor(splitData[[i]])
    }
    tempContrast <- contr.sum(numLevels[i])
    if(!all(contrasts(splitData[[i]])==tempContrast)){
      if(show.warnings){
        warning("Changing existing contrasts for ",i," to sum contrasts using contr.sum().")
      }
      contrasts(splitData[[i]]) <- tempContrast
    }
  }

  # Update the linear model based on the above changes
  # update doesn't work, but redoing the lm function does
  # to do: how to get the above to work without
  splitLm <- lm(splitFormula, data = splitData)

  summaryLm <- summary(splitLm)
  # 4th column should always be the p-value column
  anovaLm <- car::Anova(splitLm, type = "II")[,-4]
  coefsLm <- coefficients(summaryLm)
  # Split the data into overlays
  # data overlay
  dataOverlay <- splitData[,DV]

  # Common overlay
  dataCommon <- rep(coefsLm[1,1], times = numRows)

  # Residual overlay
  # To do: use corrected MSE rather than initial MSE
  dataResid <- residuals(summaryLm)

  # Effects overlays
  # Computing means using aggregate function
  listCombIVs <- do.call(c, lapply(seq_along(IVs), combn, x = IVs, simplify = FALSE))
  namesCombIVs <- sapply(listCombIVs, paste, collapse = "_")
  aggregateFormulas <- sapply(paste0(DV, "~",sapply(listCombIVs, paste, collapse="+")), formula)
  names(aggregateFormulas) <- paste0("mean_",namesCombIVs)
  aggregateList <- lapply(seq_along(aggregateFormulas), function(i){
    tempResults <- aggregate(aggregateFormulas[[i]], data = splitData, FUN = mean)
    names(tempResults)[ncol(tempResults)] <- paste0("mean_",namesCombIVs[i])
    return(tempResults)
  })
  names(aggregateList) <- paste0("mean_",namesCombIVs)

  # merging means to data
  meanData <- splitData
  for(i in aggregateList){
    meanData <- merge(meanData, i, sort=FALSE)
  }
  meanData <- meanData[do.call("order", rev(meanData[IVs])), ]

  # computing effects
  # to do: how to do this more programmatically/generally
  effList <- lapply(namesCombIVs, function(i){
    orderTerm <- stringr::str_count(i,"_")+1
    if(orderTerm==1){# Main effects
      return(meanData[,paste0("mean_",i)]-dataCommon)
    } else if (orderTerm==2){# Two-way interaction
      firstOrderTerms <- c(stringr::str_split(i, "_")[[1]])
      return(meanData[,paste0("mean_",i)]-meanData[,paste0("mean_",firstOrderTerms[1])]-
        meanData[,paste0("mean_",firstOrderTerms[2])]+dataCommon)
    } else if (orderTerm>3){# 4+-way interaction (currently throws an error)
      stop("This function can only handle up to a three-way design right now.")
    } else {# Three-way interaction
      lowerOrderTerms <- namesCombIVs[-7]
      return(meanData[,paste0("mean_",i)]+# three-way cell mean
               meanData[,paste0("mean_",lowerOrderTerms[1])]+# main effect/marginal mean
               meanData[,paste0("mean_",lowerOrderTerms[2])]+# main effect/marginal mean
               meanData[,paste0("mean_",lowerOrderTerms[3])]-# main effect/marginal mean
               meanData[,paste0("mean_",lowerOrderTerms[4])]-# two-way cell mean
               meanData[,paste0("mean_",lowerOrderTerms[5])]-# two-way cell mean
               meanData[,paste0("mean_",lowerOrderTerms[6])]-# two-way cell mean
               dataCommon)# Grand mean
    }
  })

  effFrame <- do.call(data.frame,effList)
  names(effFrame) <- paste0("eff_",namesCombIVs)

  finalData <- tibble::as_tibble(cbind(splitData, dataCommon, dataResid, effFrame))

  if(print.statistic){
    cat("Overlay Table:\n")
    print(finalData)
    cat("\n\nANOVA Source Table:\n")
    print(anovaLm)
  }

  finalList <- list("overlayTibble" = finalData,
                    # From https://stackoverflow.com/a/10520832
                    "dataObjName" = deparse(substitute(splitData)),
                    "originalData" = splitData,
                    "vars" = list("indVars"=IVs, "depVars" = DV),
                    "commonOverlay" = dataCommon,
                    "effectOverlayTable" = effFrame,
                    "residualOverlay" = dataResid,
                    "sourceTable" = anovaLm,
                    "summaryTable" = summaryLm)
  class(finalList) <- "dare_overlay"

  # Return overlays and source/summary tables
  return(invisible(finalList))
}
