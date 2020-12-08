#' Produce panel of stacked bar charts
#'
#' This function takes in data and produces a panel of stacked bar charts,
#' based on different groupings. Intended to be used with oakViz::aggregate_categories()
#'
#' @param dat Data with a column containing variables of interest and grouping variables.
#' @param fill Name of column containing fill variable, default is ses.
#' @param facet Name of category to use as facet grouping variable: "Ethnoracial", "Gentrification", "Income"
#' @param group Category for x-axis grouping, can be any column of labels, period (default)
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Grouped bar chart across four periods.
#' @export

stacked_bar <- function(
  dat,
  fill = "ses",
  facet = c("ethnoracial", "gent", "income"), # "ethnoracial", "gent", "income"
  group = "period", # gent, ethnoracial, income, ses, period
  save = F,
  savename = "plot.png",
  caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.\n"
) {
  library(rgdal)
  library(foreach)
  library(tidyverse)
  library(gridExtra)
  library(grid)

  theme =
    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      legend.direction = "horizontal",
      # Caption
      plot.caption = element_text(size = 10, hjust = .5, face = "italic"),
      # X-axis
      axis.ticks.x=element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      # Y-axis
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 10),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank())

  dat = dat %>%
    filter(year %in% c("boom","bust", "recovery", "post_recovery"))

  # First bar chart

  if (facet[1] == "ethnoracial") {
    dat = dat %>%
      filter(facet == "Ethnoracial")
  } else if (facet == "gent") {
    dat = dat %>%
      filter(facet == "Gentrification")
  } else if (facet == "income") {
    dat = dat %>%
      filter(facet == "Income")
  } else {
    return("Please select valid facet: ethnoracial, gent, or income")
  }

  # Order x-axis grouping
  if (group == "period") {
    dat$year <- factor(dat$year,
                       levels = c("boom", "bust", "recovery", "post_recovery"))
    dat$x_group = dat$year
    x_labels = c("Boom","Bust", "Recovery", "Post-Recovery")
  }
  # Add additional x-axis groupings to code

  # Order fill grouping
  if (fill == "ses") {
    dat$ses <- factor(dat$ses,
                      levels = c("All", "Low", "LMM" ,"Moderate","Middle", "High"))
    dat$fill = dat$ses
    values = c("All" = "#9b9b9b",
               "Low" = "#fcbba1",
               "LMM" = "#faab8c",
               "Moderate" = "#fc9272",
               "Middle" = "#fb6a4a",
               "High" = "#b63b36")
    fill_labels = c("All", "Low", "LMM","Moderate","Middle", "High")
  }
  # Add additional fill groupings to code

  dat = dat %>%
    group_by(cat, x_group) %>%
    mutate(denom = sum(pop)) %>%
    mutate(pop_pct_compute = pop/denom)

  plot1 <-
    ggplot(dat, aes(y = pop_pct_compute,
                    x = x_group,
                    fill = fill)) +
    geom_bar(stat="identity", position = "stack") +
    facet_grid(cols = vars(cat)) +
    scale_fill_manual(values = values,
                      labels = fill_labels) +
    scale_x_discrete(
      labels = x_labels) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    labs(x = "", y = "") +
    theme +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1, reverse = T))

  # Second bar chart

  if (facet[2] == "ethnoracial") {
    dat = dat %>%
      filter(facet == "Ethnoracial")
  } else if (facet == "gent") {
    dat = dat %>%
      filter(facet == "Gentrification")
  } else if (facet == "income") {
    dat = dat %>%
      filter(facet == "Income")
  } else {
    return("Please select valid facet: ethnoracial, gent, or income")
  }

  # Order x-axis grouping
  if (group == "period") {
    dat$year <- factor(dat$year,
                       levels = c("boom", "bust", "recovery", "post_recovery"))
    dat$x_group = dat$year
    x_labels = c("Boom","Bust", "Recovery", "Post-Recovery")
  }
  # Add additional x-axis groupings to code

  # Order fill grouping
  if (fill == "ses") {
    dat$ses <- factor(dat$ses,
                      levels = c("All", "Low", "LMM" ,"Moderate","Middle", "High"))
    dat$fill = dat$ses
    values = c("All" = "#9b9b9b",
               "Low" = "#fcbba1",
               "LMM" = "#faab8c",
               "Moderate" = "#fc9272",
               "Middle" = "#fb6a4a",
               "High" = "#b63b36")
    fill_labels = c("All", "Low", "LMM","Moderate","Middle", "High")
  }
  # Add additional fill groupings to code

  dat = dat %>%
    group_by(cat, x_group) %>%
    mutate(denom = sum(pop)) %>%
    mutate(pop_pct_compute = pop/denom)

  plot2 <-
    ggplot(dat, aes(y = pop_pct_compute,
                    x = x_group,
                    fill = fill)) +
    geom_bar(stat="identity", position = "stack") +
    facet_grid(cols = vars(cat)) +
    scale_fill_manual(values = values,
                      labels = fill_labels) +
    scale_x_discrete(
      labels = x_labels) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    labs(x = "", y = "") +
    theme +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1, reverse = T))

  # Third bar chart

  if (facet[3] == "ethnoracial") {
    dat = dat %>%
      filter(facet == "Ethnoracial")
  } else if (facet == "gent") {
    dat = dat %>%
      filter(facet == "Gentrification")
  } else if (facet == "income") {
    dat = dat %>%
      filter(facet == "Income")
  } else {
    return("Please select valid facet: ethnoracial, gent, or income")
  }

  # Order x-axis grouping
  if (group == "period") {
    dat$year <- factor(dat$year,
                       levels = c("boom", "bust", "recovery", "post_recovery"))
    dat$x_group = dat$year
    x_labels = c("Boom","Bust", "Recovery", "Post-Recovery")
  }
  # Add additional x-axis groupings to code

  # Order fill grouping
  if (fill == "ses") {
    dat$ses <- factor(dat$ses,
                      levels = c("All", "Low", "LMM" ,"Moderate","Middle", "High"))
    dat$fill = dat$ses
    values = c("All" = "#9b9b9b",
               "Low" = "#fcbba1",
               "LMM" = "#faab8c",
               "Moderate" = "#fc9272",
               "Middle" = "#fb6a4a",
               "High" = "#b63b36")
    fill_labels = c("All", "Low", "LMM","Moderate","Middle", "High")
  }
  # Add additional fill groupings to code

  dat = dat %>%
    group_by(cat, x_group) %>%
    mutate(denom = sum(pop)) %>%
    mutate(pop_pct_compute = pop/denom)

  plot3 <-
    ggplot(dat, aes(y = pop_pct_compute,
                    x = x_group,
                    fill = fill)) +
    geom_bar(stat="identity", position = "stack") +
    facet_grid(cols = vars(cat)) +
    scale_fill_manual(values = values,
                      labels = fill_labels) +
    scale_x_discrete(
      labels = x_labels) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    labs(x = "", y = "") +
    theme +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1, reverse = T))

  if (save) {
    ggsave(savename, plot, height = 5, width = 7)
  }
  return(plot)
}
