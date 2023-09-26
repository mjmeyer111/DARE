# #' Splitting data to construct overlays that mimic the four sections of
# #' data used to calculate a one-way ANOVA.
# #'
# #' @param data tibble or data frame: dataset containing the variables.
# #' @param nominal A vector of column names or indices to be blinded, with their group order shuffled.
# #' Keep in mind that only group names from nominal variables should be shuffled (the order
# #' of ordinal variables should be kept)
# #' @param ordinal A vector of column names or indices to be blinded, with their group order not shuffled.
# blinding <- function(data, nominal = NULL, ordinal = NULL){
#   if(is.null(nominal)&&is.null(ordinal)){
#     stop("Please choose at least one column to blind in the dataset.")
#   }
#   for(i in c(nominal, ordinal)){
#     if(!is.factor(data[,i])){
#       data[,i] <- factor(data[,i])
#       warning("Column ",i," is not a factor. Coercing to a factor.")
#     }
#
#     numLevels <- length(levels(data[,i]))
#     placeValue <- as.character(ceiling(log10(numLevels+1)))
#
#     if(i%in%nominal){# nominal
#       groupNumbers <- sample(1:numLevels, numLevels)
#     } else {# ordinal
#       groupNumbers <- 1:numLevels
#     }
#     data[,i] <- factor(data[,i], labels=paste0("Group_",sprintf(paste0("%0",placeValue,"d"),groupNumbers)))
#   }
#   return(data)
# }
