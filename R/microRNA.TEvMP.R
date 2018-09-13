# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to run the microRNA.TEvMP shiny app
#'
#' This function runs the microRNA.TEvMP app
#' @import shiny
#' @import dplyr
#' @import googlesheets
#' @rawNamespace import(Hmisc, except = c(summarize, src))
#' @importFrom readr  write_csv read_csv
#' @importFrom tidyr gather spread
#' @importFrom stats na.omit
#' @return Plot
#' @param ... Any argument that you can pass to shiny::runApp
#' @examples
#' # microRNA.TEvMP()
#' @return Runs the microRNA.TEvMP shiny app.
#' @export
microRNA.TEvMP <- function(...)
{
  appDir <- system.file("app", package = "microRNA.TEvMP")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing microRNA.TEvMP",
         call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = TRUE, ...)
}
