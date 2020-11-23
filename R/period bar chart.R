#' Produce grouped bar chart across four periods.
#'
#' This function takes in data and produces a bar chart grouped by either
#' gentrification, ethnoracial, or income category across four housing periods.
#'
#' @param dat Data with a column containing census tracts and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param limits Y-axis limits
#' @param group Category for x-axis grouping: "gent" (default), "ethnoracial", or "income"
#' @param compute Method of summarizing values: "mean" (default) or "median"
#' @param title Figure title
#' @param y_title Y-axis title
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Grouped bar chart across four periods.
#' @export
# Plot bar chart by group across periods
plot_bar_periods <- function(
  dat,
  var,
  limits,
  group = "gent", # gent, ethnoracial, income
  compute = "mean",
  title = "Title",
  y_title = "Y-axis title",
  save = F,
  savename = "plot.png",
  caption = "Data was aggregated by taking mean of all tracts in a category"
) {
  ## Read Data
  library("ggplot2")
  library("readxl")
  library("plyr")
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
    c("#b2badf","#a3d5d1","#76c0ba","#48aba3","#19968c")
  inc_cat <- c("Bottom Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Top Quintile")

  # Relabel names for the graphs

  relabel_gent_cat <- c("nongentrifiable" = "Nongentrifiable",
                        "gentrifying" = "Gentrifying",
                        "intense"  = "Intense",
                        "moderate" = "Moderate",
                        "earlygent" = "Early Gentrification",
                        "weak" = "Weak",
                        "peoplepricegent" = "People or Price")

  gent_cat_plot_order <- c("Nongentrifiable", "Gentrifying",
                           "Intense", "Moderate",
                           "Early Gentrification", "Weak", "People or Price")

  relabel_race_cat <- c("PredWhite" = "Predominantly White",
                        "PredBlack" = "Predominantly Black",
                        "PredOther"  = "Predominantly Other",
                        "WhiteOther" = "White-Other",
                        "BlackWhite" = "Black-White",
                        "BlackOther" = "Black-Other",
                        "Multiethnic" = "Multiethnic",
                        "Overall" = "Overall",
                        "WhiteMixed" = "White/White-Mixed",
                        "MixedOther" = "Multiethnic/Other")

  race_cat_plot_order <- c("Predominantly White", "Predominantly Black",
                           "Predominantly Other","White-Other","Black-White","Black-Other","Multiethnic",
                           "White/White-Mixed", "Multiethnic/Other")

  inc_cat_plot_order <- c("Bottom Quintile", "Second Quintile", "Middle Quintile",
                          "Fourth Quintile", "Top Quintile")

  # Create period label
  dat = dat %>%
    mutate(period = ifelse(year %in% c(2002:2006), "Boom",
                           ifelse(year %in% c(2007:2009), "Bust",
                                  ifelse(year %in% c(2010:2014), "Recovery",
                                         ifelse(year %in% c(2015:2017), "Post-Recovery", NA))))) %>%
    mutate(period = factor(period, levels = c("Boom", "Bust", "Recovery", "Post-Recovery")))

  dat$tractid10 = as.double(dat$tractid10)

  # Combine with either gentcat, racecat, or inccat
  if (group == "gent") {
    dat = dat %>% left_join(gentcat, by = "tractid10") %>%
      select(-c(tractid10, year)) %>%
      mutate(cat = factor(cat, levels = c(gent_cat_plot_order)))
    colors = gent_cat_colors
    labels = gent_cat

  } else if (group == "ethnoracial") {
    dat = dat %>% left_join(racecat, by = "tractid10") %>%
      select(-c(tractid10, year)) %>%
      mutate(cat = factor(cat, levels = c(race_cat_plot_order)))
    colors = race_short_colors
    labels = race_short

  } else if (group == "income") {
    dat = dat %>% left_join(inccat, by = "tractid10") %>%
      select(-c(tractid10, year)) %>%
      mutate(cat = factor(cat, levels = c(inc_cat_plot_order)))
    colors = inc_cat_colors
    labels = inc_cat

  } else {
    return("Please select valid group: 'gent', 'ethnoracial', 'income'")
  }

  if (compute == "mean") {
    dat = dat %>%
      drop_na() %>%
      group_by(period, cat) %>%
      # Provide option of mean or median
      dplyr::summarise(value = mean({{var}}, na.rm = T))
  } else if (compute == "median") {
    dat = dat %>%
      drop_na() %>%
      group_by(period, cat) %>%
      # Provide option of mean or median
      dplyr::summarise(value = median({{var}}, na.rm = T))
  } else {
    return("Please select valid computation: 'mean', 'median'")
  }

  plot <-
    ggplot(dat, aes(x = cat, y = value, fill = cat)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(~ period) +
    scale_fill_manual(values = colors,
                      labels = labels) +
    # scale_y_continuous(limits = limits, expand = c(0, 0)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          plot.title = element_text(size = 18, hjust = .5),
          plot.caption = element_text(size = 8, hjust = .5, face = "italic")) +
    labs(title = title, y = y_title, x = "")

  if (save == F) {
    return(plot)
  } else if (save == T){
    return(map)
    ggsave(savename, plot, height = 5, width = 7)
  }

}
