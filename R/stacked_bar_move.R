#' Produce a stacked bar chart for move types
#'
#' This function takes in data and produces a stacked bar chart for
#' different types of moves. Intended to be used with oakViz::aggregate_categories()
#'
#' @param dat Data with a column containing variables of interest and grouping variables.
#' @param facet Name of category to use as facet grouping variable, default: "ses"
#' @param group Category for x-axis grouping, can be any column of labels, default: "ethnoracial"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Stacked bar charts of move types based on different groupings
#' @export

stacked_bar_move <- function(
  dat,
  facet = "ses",
  group = "ethnoracial",
  save = F,
  savename = "plot.png",
  caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.\n"
) {
  library(rgdal)
  library(foreach)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(reshape2)

  theme =
    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 9),
      legend.position = "none",
      legend.direction = "horizontal",
      # Caption
      plot.caption = element_text(size = 10, hjust = .5, face = "italic"),
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
      plot.margin=unit(c(0,0.5,0,0.5), "cm"))

  # X-axis grouping
  if (group == "ethnoracial") {
    dat = dat %>%
      filter(facet %in% c("All", "Ethnoracial"))
  }
  # Add additional x-axis groupings to code

  # Facet grouping
  if (facet == "ses") {
    dat = dat %>%
      filter(!is.na(ses))
    dat$ses <- factor(dat$ses,
                      levels = c("All", "Low", "LMM" ,"Moderate","Middle", "High"))
  }
  # Add additional facet groupings to code

  # ***Following code assumes facet grouping by SES:
  # need to adjust code if using other groupings

  # Compute share of total moves by move type
  dat = dat %>%
    select(ses, cat, outmigration_outba, outmigration_outoak, withinoakmigration) %>%
    group_by(cat, ses) %>%
    summarise_all(sum) %>%
    mutate(denom = outmigration_outba +
             (outmigration_outoak - outmigration_outba) +
             withinoakmigration) %>%
    mutate(moved_outba_pct = outmigration_outba/denom) %>%
    mutate(diff_city_ba_pct = (outmigration_outoak - outmigration_outba)/denom) %>%
    mutate(moved_within_oak_pct = withinoakmigration/denom) %>%
    select(ses, cat, moved_outba_pct, diff_city_ba_pct, moved_within_oak_pct)

  # Melt data for plotting
  dat_melt = dat %>%
    melt(id = c("cat", "ses"))

  # Rename/order/select colors/label move categories
  dat_melt$variable <- plyr::revalue(dat_melt$variable,
                                     c("moved_outba_pct"="Moved out of Bay Area",
                                       "diff_city_ba_pct" = "Different City within Bay Area",
                                       "moved_within_oak_pct" = "Moved within Oakland"))

  dat_melt$variable <- factor(dat_melt$variable,
                              levels = c("Moved out of Bay Area",
                                         "Different City within Bay Area",
                                         "Moved within Oakland"))

  values = c("Moved out of Bay Area" = "#8baf3e",
             "Different City within Bay Area" = "#fdbd3b",
             "Moved within Oakland" = "#2e5e8b")

  fill_labels = c("Moved out of Bay Area",
                  "Different City within Bay Area",
                  "Moved within Oakland")

  # Order/label race categories
  dat_melt$cat <- factor(dat_melt$cat,
                         levels = c("Overall",
                                    "Predominantly Black",
                                    "Black-Other",
                                    "White/White-Mixed",
                                    "Multiethnic/Other"))
  x_labels = c("Overall",
               "Predominantly Black",
               "Black-Other",
               "White/White-Mixed",
               "Multiethnic/Other")

  # Plot stacked bar chart
  plot <-
    ggplot(dat_melt, aes(y = value,
                         x = cat,
                         fill = variable)) +
    geom_bar(stat="identity", position = "stack") +
    facet_grid(cols = vars(ses)) +
    scale_fill_manual(values = values,
                      labels = fill_labels) +
    scale_x_discrete(
      labels = x_labels) +
    scale_y_continuous(expand = c(0, 0.01), labels = scales::percent) +
    labs(x = NULL, y = NULL) +
    theme +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 1, reverse = T))


  if (save) {
    ggsave(savename, plot, height = 4, width = 6)
  }
  return(plot)
}
