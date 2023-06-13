#
# library(tidyverse)
# anovaDARE <- function(formulaModel, data){
#
#   # Extract variables and terms
#   variablesModel <- all.vars(formulaModel)
#   termsModel <- attr(terms(formulaModel), "term.labels")
#   dvModel <- setdiff(variablesModel, termsModel)
#
#   # Center continuous variables, and set categorical predictors to contr.sum
#   for(i in variablesModel){
#     if(is.numeric(data[,i])){
#       # Center numeric variables
#       data[,i] <- data[,i]-mean(data[,i])
#     } else if (is.character(data[,i])|is.factor(data[,i])){
#       # for any character or factor variables, coerce variables into a factor,
#       # drop any unused levels, and then set contrasts to contr.sum
#       data[,i] <- as.factor(data[,i])
#       data[,i] <- droplevels(data[,i])
#       contrasts(data[,i]) <- contr.sum(length(levels(data[,i])))
#     }
#   }
#
#   # Perform the linear model
#   modelLm <- lm(formulaModel, data=data)
#
#   # Conduct the ANOVA for full model
#   anovaOutput <- car::Anova(modelLm, type = "II")
#
#   # Assuming there's at least one non-intercept term, find locations of all :'s in each term,
#   # set length of counting output to zero for each term that has zero :'s (gregexpr outputs
#   # -1 instead), and then count number of indices to get number of :'s
#   if(length(termsModel) > 0){
#     orderTerms <- gregexpr(":",termsModel)
#     orderTerms <- lapply(orderTerms,function(i){
#       if(-1%in%i){
#         return(vector())
#       } else {
#         return(i)
#       }
#     })
#     orderTerms <- sapply(orderTerms,length)+1
#
#     # Create hierarchy of models
#     modelHierarchy <- lapply(unique(orderTerms), function(i){
#       ivTempModel <- paste(termsModel[orderTerms<=i], collapse="+")
#       formulaTempModel <- formula(paste0(dvModel, "~", ivTempModel))
#
#       return(lm(formulaTempModel, data))
#     })
#
#     # Adding null model
#     nullModelFormula <- formula(paste0(dvModel, "~ 1"))
#     modelHierarchy <- c(list(lm(nullModelFormula, data)), modelHierarchy)
#
#   } else {
#     # Just modelLm = DV ~ 1
#     modelHierarchy <- list(modelLm)
#   }
#
#   # Get information criteria
#   modelNames <- sapply(modelHierarchy, function(x){
#     capture.output(attr(x$model, "terms"))[1]
#     })
#   modelAIC <- do.call(AIC, modelHierarchy)
#   modelAICc <- sapply(modelHierarchy, gamlr::AICc)
#   modelBIC <- do.call(BIC, modelHierarchy)
#   # correct structure of AIC and BIC if there's only one value
#   if(length(termsModel) == 0){
#     modelAIC <- tibble("df"=2,"AIC"=modelAIC)
#     modelBIC <- tibble("df"=2,"BIC"=modelBIC)
#   }
#   ICtibble <- tibble("model"=modelNames, modelAIC, "AICc"=modelAICc, "BIC"=modelBIC$BIC)
#
#   # Compute likelihood ratio tests on nested model structure
#   if(length(termsModel) == 0){
#     likelihoodModel <- logLik(modelLm)
#   } else {
#     likelihoodModel <- do.call(lmtest::lrtest, modelHierarchy)
#   }
#
#   return(list("model"=modelLm,
#               "anova"=anovaOutput,
#               "IC"=ICtibble,
#               "likelihood" = likelihoodModel
#               ))
# }
#
# anovaDARE(mpg~wt*cyl*hp, data=mtcars)
# anovaDARE(mpg~wt*cyl, data=mtcars)
# anovaDARE(mpg~wt, data=mtcars)
# anovaDARE(mpg~1, data=mtcars)
