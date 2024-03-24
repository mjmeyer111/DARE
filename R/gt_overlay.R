#' Summarize ANOVA overlays as a gt_effect_table.
#'
#' @param dareANOVAobj Object with dare_overlay class
#' @return a gt_effect_table containing variables and effects from overlay
#' @examples
#'\dontrun{
#' # One variable
#' ANOVA_split(len~dose, ToothGrowth) |> gt_overlay()
#'
#' # Two variables
#' ANOVA_split(len~supp*dose, ToothGrowth) |> gt_overlay()
#'}
#' @export
gt_overlay <- function(dareANOVAobj){
  IVs <- dareANOVAobj$vars$indVars
  effFrame <- dareANOVAobj$effectOverlayTable
  gt_effNames <- gsub("eff_", "", colnames(effFrame))
  gt_effNames <- gsub("_", " x ", gt_effNames)

  effOnlyData <- tibble::as_tibble(cbind(dareANOVAobj$originalData[IVs],
                                         "dataCommon"=dareANOVAobj$commonOverlay,
                                         effFrame))

  gt_effect_table <- effOnlyData |>
    dplyr::distinct() |>
    gt::gt() |>
    gt::tab_header(
      title = "Effects" # paste("Effects of", dareANOVAobj$dataObjName)
    ) |> gt::tab_spanner(
      label = "Variables",
      columns = IVs
    ) |> gt::tab_spanner(
      label = "Common",
      columns = dataCommon,
    ) |> gt::tab_spanner(
      label = "Effects",
      columns = colnames(effFrame)
    ) |> gt::cols_label(
      dataCommon = "common"
    ) |> gt::cols_label(
      .list=structure(as.list(gt_effNames), names = colnames(effFrame))
    ) |> gt::fmt_number(
      columns = matches("^eff_|common"),
      decimals = 2
    )
  print(gt_effect_table)
}
