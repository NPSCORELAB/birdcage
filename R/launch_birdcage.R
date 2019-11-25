#' Launch birdcage
#' 
#' @param ... Arguments passed to or from other methods.
#' 
#' @importFrom shiny runApp
#' 
#' @export
launch_birdcage <- function(...) {
  runApp(
    appDir = system.file("shiny-birdcage", package = "birdcage") ,
    ...
  )
}