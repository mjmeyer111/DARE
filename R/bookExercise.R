#' Open up shiny app that includes interactive exercises that the user can use
#' to practice.
#'
#' @param chapter Integer: Which chapter would you like to see exercises on?
#' @return A shiny app that includes interactive exercises that the user can use
#' to practice.
#' @examples
#'\dontrun{
#'bookExercise(chapter = 3)
#'}
#' @export
bookExercise <- function(chapter = 1){
  browseURL(paste0("https://xtong.shinyapps.io/chapter_",chapter,"_exercises"))
}
