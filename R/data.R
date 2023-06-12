#' SearchConjunction dataset
#'
#' Insert description here
#'
#' @format ## `searchConjunction`
#' A tibble with 39,962 rows and 8 columns:
#' \describe{
#'   \item{subject}{The subjects in the study}
#'   \item{trial}{Trial ID}
#'   \item{setSize, setSize.f}{The size of the set. Either 3, 6, 12, or 18.}
#'   \item{target}{Whether the target is absent or present}
#'   \item{error}{Whether there was an error or not}
#'   \item{sdt}{Either CR, FA, H, or M}
#'   \item{rTime}{Reaction time}
#'   ...
#' }
#' @source insert source here (url's can be in < >)
#'
"searchConjunction"

#' Speed Dating dataset
#'
#' Insert description here
#'
#' @format ## `speedDating`
#' A tibble with 6 rows and 3 columns:
#' \describe{
#'   \item{goal.f}{A factor, describing the primary goal in participating in the speed dating study.}
#'   \item{n}{The frequency of participants who chose the given primary goal.}
#'   \item{prop}{The proportion of participants who chose the given primary goal.}
#' }
#' @source insert source here (url's can be in < >)
#'
"speedDating"


#' age dataset
#'
#' A frequency table summarizing the sat.act dataset in the psych package.
#'
#' @format ## `age`
#' A tibble with 48 rows and 3 columns:
#' \describe{
#'   \item{age}{The age categories of students who took either the ACT or SAT in the sat.act dataset.}
#'   \item{freq}{The frequency of participants who are the given age.}
#'   \item{prop}{The proportion of participants who are the given age.}
#' }
#' @source insert source here (url's can be in < >)
#'
"age"
