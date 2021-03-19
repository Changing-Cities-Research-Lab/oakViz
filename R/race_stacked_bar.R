#' Produce stacked bar charts filled by race category
#'
#' This function takes in data and produces a stacked bar chart, filled by race
#' category. The facet and x-axis labels can be customized.
#'
#' @param dat Data with a "variable" column containing races, a "value" column, a "cat" column for faceting labels, and x-axis grouping variable
#' @param x_group Category for x-axis grouping
#' @param title Plot title
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Panel of stacked bar charts of different groupings
#' @export

race_stacked_bar <- function(
  dat,
  x_group,
  title = NULL,
  save = F,
  savename = "plot.png",
  caption = paste0(frb_acs_caption, ses_caption)
) {

  library(tidyverse)

  theme =
    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 8.5),
      legend.position = "right",
      # Caption
      plot.caption = element_text(size = 9, hjust = .5, face = "italic"),
      # X-axis
      axis.text.x=element_text(size = 8.5, hjust = 1, angle = 45),
      axis.title.x=element_blank(),
      # Y-axis
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 10),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      # Margins
      plot.margin=unit(c(0,0,0,0.5), "cm"))

  dat = as.data.frame(dat)

  # Plot figure
  plot <- ggplot(dat,
                 aes(y = value/100, x = {{x_group}}, fill = variable)) +
    geom_bar(stat="identity", position="stack") +
    facet_grid(~ cat) +
    scale_fill_manual(values = race_colors) +
    scale_y_continuous(limits = c(0, 1.0001),
                       expand = c(0, 0.01),
                       labels = scales::percent_format(accuracy = 5L)) +
    labs(x = NULL,
         y = NULL,
         title = title,
         fill="Race/Ethnicity:",
         caption = caption) +
    theme

if (save) {
  ggsave(savename, plot, height = 5, width = 9)
}
return(plot)
}

