# moadq_App.R
#' @export
moadq_App <- function() {
  appDir <- system.file("moadq_App",
                        package = "moadqproject")

  shiny::runApp(
    appDir,
    launch.browser = TRUE,
    display.mode = "normal"
  )
}
