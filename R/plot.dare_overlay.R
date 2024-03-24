#' Summarize ANOVA overlays as a plot.
#'
#' @param dareANOVAobj Object with dare_overlay class
#' @return a plot containing scatterplot of overlay effects and boxplot of overlay residuals
#' @examples
#'\dontrun{
#' # One variable
#' ANOVA_split(len~dose, ToothGrowth) |> plot()
#'
#' # Two variables
#' ANOVA_split(len~supp*dose, ToothGrowth) |> plot()
#'}
#' @export
plot.dare_overlay <- function(dareANOVAobj){
  plotData <- tibble::as_tibble(cbind(dareANOVAobj$effectOverlayTable,
                                      "dataResid"=dareANOVAobj$residualOverlay))
  plotDataEffWide <- reshape2::melt(plotData,
                                    measure.vars = names(plotData),
                                    variable.name = "Term",
                                    value.name = "value")

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = plotDataEffWide[plotDataEffWide$Term!="dataResid",],
                        ggplot2::aes(x = Term, y = value)) +
    ggplot2::geom_boxplot(data = plotDataEffWide[plotDataEffWide$Term=="dataResid",],
                          ggplot2::aes(x = Term, y = value)) +
    ggplot2::ylab("Effect of Residual") +
    ggplot2::scale_x_discrete(labels = gsub("_", " x ", unique(plotDataEffWide$Term)))
  print(p)
}
