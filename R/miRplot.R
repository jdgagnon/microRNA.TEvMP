
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' Plot the data
#' @import shiny
#' @import shinythemes
#' @import dplyr
#' @import ggplot2
#' @import viridis
#' @rawNamespace import(Hmisc, except = c(summarize, src))
#' @importFrom tibble as.tibble
#' @importFrom grid convertUnit nullGrob
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom egg set_panel_size
#' @importFrom gtable gtable_add_padding
#' @importFrom readr parse_number read_csv read_tsv
#' @importFrom scales trans_format math_format rescale_none
#' @importFrom stringr str_remove str_split word
#' @importFrom tidyr gather
#' @importFrom stats na.omit start
#' @importFrom colourpicker colourInput updateColourInput
#' @rawNamespace import(Hmisc, except = c(summarize, src))
#' @importFrom readr  write_csv read_csv
#' @importFrom tidyr gather spread
#' @importFrom stats na.omit
#' @param df Dataframe.
#' @param color.groups Color groups
#' @param shape.groups shape.groups
#' @return Located example excel file in package
#' @export
miRplot <- function(df, color.groups, shape.groups) {

  if (unique(df$Type) == "MP") {
    limits <- c(-0.1,30.5)
    breaks <- c(0, 4.5, 9, 30)
    labels <- c("0", "4.5", "9", "30")
  }

  if (unique(df$Type) == "TE") {
    limits <- c(-0.1,9.5)
    breaks <- c(0, 4.5, 9)
    labels <- c("0", "4.5", "9")
  }

  # Assign names to the shape, color arguments
  for (x in c("shape.groups", "color.groups")) {
    assign(x, stats::setNames(object = get(x), levels(df[["miRNA"]])))
  }
  g <- ggplot2::ggplot(data = df, ggplot2::aes(x = Day,
                                               y = value,
                                               color = miRNA,
                                               shape = miRNA)) +
    ggplot2::ggtitle(unique(df$Type)) +
    ggplot2::stat_summary(ggplot2::aes(group = miRNA),
                          fun.data=mean_sdl,
                          fun.args = list(mult=1),
                          geom="errorbar",
                          width=0.125*1.25,
                          size = 0.25) +
    ggplot2::stat_summary(fun.y=mean,geom="point", size = 2.5) +
    ggplot2::stat_summary(fun.y=mean, geom="line") +
    ggplot2::scale_color_manual(name = NULL, values = color.groups) +
    ggplot2::scale_shape_manual(name = NULL, values = shape.groups) +
    ggplot2::ylab("log2(Relative Expression)") +
    ggplot2::xlab("Days Post Infection (LCMV)") +
    ggplot2::scale_x_continuous(limits = limits,
                                breaks = breaks,
                                labels = labels) +
    ggplot2::scale_y_continuous(limits = c(2^-2.2,2^2.2),
                                breaks = c(2^-2,2^-1,2^0,2^1,2^2),
                                labels = c(-2,-1,0,1,2),
                                trans = "log2") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   line = element_line(colour = "black", size = 0.25),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(colour = "black",
                                                      size = 0.25),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black",
                                                     size = 0.25),
                   legend.position ="none",
                   legend.background = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 15,
                                                     colour = "black"),
                   legend.key = ggplot2::element_rect(fill = NA),
                   plot.background = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 15,
                                                colour = "black"))
  gt <- egg::set_panel_size(
    g,
    width = ggplot2::unit(200, "mm"),
    height = ggplot2::unit(100, "mm")
  )
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  return(gt)
}

