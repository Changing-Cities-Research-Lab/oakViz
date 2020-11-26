#' Produce lollipop plot by gentrification, ethnoracial, and income category.
#'
#' This function takes in data and produces a horizontal lollipop plot by
#' gentrification, ethnoracial, and income category. Input data needs columns for
#' variable of interest (titled "var") and tract number (titled "tractid10").
#' Intended to be used after oakViz::aggregate_categories().
#'
#' @param data Data with column for variable of interest.
#' @param var Column name of variable of interest.
#' @param limits Y-axis limits.
#' @param title Figure title
#' @param y_title Y-axis title
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Lollipop plot of variable by gentrification, ethnoracial, and income category.
#' @export
# Lollipop Plot
plot_lollipop <- function(
  data,
  var,
  limits,
  title = "Title",
  y_title = "Y-axis Title",
  save = F,
  savename = "plot.png",
  caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.\n"
) {
  library("ggplot2")
  library("readxl")
  library("dplyr")
  library("foreach")
  library("reshape")
  library("gridExtra")
  library("grid")
  library('tidyverse')

  ### PARAMETERS ###
  gent_cat_colors <-
    c("snow3","#d94801", "#fa7b00", "#fdcc8a", "#a6d894")
  gent_cat <- c("Nongentrifiable", "Intense", "Moderate", "Weak", "People or Price")

  race_short_colors <-
    c("#481567FF", "#33638DDF", "#FDE725FF", "#20A387FF")
  race_short <- c("Predominantly Black", "Black-Other", "White/White-Mixed", "Multiethnic/Other")

  inc_cat_colors <-
    c("#c7cff2","#8897db","#697fe0","#4c66d9","#1437cc")
  inc_cat <- c("Bottom Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Top Quintile")

  labels = c("Overall", gent_cat, race_short, inc_cat)
  colors = c("white", gent_cat_colors, race_short_colors, inc_cat_colors)

  # Relabel names for the graphs

  gent_cat_plot_order <- c("Nongentrifiable", "Gentrifying",
                           "Intense", "Moderate",
                           "Early Gentrification", "Weak", "People or Price")

  race_cat_plot_order <- c("Predominantly White", "Predominantly Black",
                           "Predominantly Other","White-Other","Black-White","Black-Other","Multiethnic",
                           "White/White-Mixed", "Multiethnic/Other")

  inc_cat_plot_order <- c("Bottom Quintile", "Second Quintile", "Middle Quintile",
                          "Fourth Quintile", "Top Quintile")

  plot <-
    ggplot(data, aes(x = cat, y = {{var}}, fill = cat)) +
    geom_segment(aes(x=cat, xend=cat,
                     y=limits[1], yend={{var}}), size=0.25,
                 show.legend = FALSE) +
    geom_point(aes(color = factor(cat)),size = 3.25,shape = 21,
               colour = "black",show.legend = TRUE) +
    facet_grid(rows = vars(facet), scale = "free", space = "free") +
    scale_color_manual(values = colors,
                       labels = labels) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(limits = limits,
                       expand = c(0, 0),
                       labels = scales::percent) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()
    ) +
    theme(axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") +
    labs(title = title, y = y_title, x= "", caption = caption) +
    theme(plot.title = element_text(size = 18, hjust = .5),
          plot.caption = element_text(size = 8, hjust = .5, face = "italic")) +
    coord_flip()

  if (save) {
    ggsave(savename, plot, height = 4.5, width = 4.5)
    return(plot)
  } else {
    return(plot)
  }
}

