#' Splitting data to construct overlays that mimic the four sections of
#' data used to calculate a one-way ANOVA.
#'
#' @param data tibble or data frame: dataset containing the variables.
#' @param variables A vector of column names or indices to be blinded, with their group order shuffled.
#' Keep in mind that only groups from nominal variables should be blinded and shuffled.
blinding <- function(data, variables = NULL){
  if(is.null(variables)){
    stop("Please choose at least one column to blind in the dataset.")
  }
  for(i in c(variables)){
    if(!is.factor(data[,i])){
      data[,i] <- factor(data[,i])
      warning("Column ",i," is not a factor. Coercing to a factor.")
    }

    numLevels <- length(levels(data[,i]))
    placeValue <- as.character(ceiling(log10(numLevels+1)))

    groupNumbers <- sample(1:numLevels, numLevels)

    data[,i] <- factor(data[,i], labels=paste0("Group_",sprintf(paste0("%0",placeValue,"d"),groupNumbers)))
  }
  return(data)
}
