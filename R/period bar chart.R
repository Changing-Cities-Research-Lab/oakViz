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
  caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.\n"
) {
  ## Read Data
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

  ses_cat_colors <-
    c("#fcbba1", "#fc9272", "#fb6a4a", "#b63b36")
  ses_cat = c("Low", "Moderate", "Middle", "High")

  period_cat_colors <-
    c("#46aac8", "#46aac8", "#46aac8", "#46aac8")
  period_cat = c("Boom", "Bust", "Recovery", "Post-Recovery")

  # Create period label
  dat = dat %>%
    mutate(period = ifelse(year %in% c(2002:2006), "Boom",
                           ifelse(year %in% c(2007:2009), "Bust",
                                  ifelse(year %in% c(2010:2014), "Recovery",
                                         ifelse(year %in% c(2015:2017), "Post-Recovery", NA))))) %>%
    mutate(period = factor(period, levels = c("Boom", "Bust", "Recovery", "Post-Recovery")))

  dat$tractid10 = as.double(dat$tractid10)

  # Combine with either gentcat, racecat, inccat, ses, or period
  if (group == "gent") {
    dat = dat %>% left_join(gentcat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(gent_cat)))
    colors = gent_cat_colors
    labels = gent_cat

  } else if (group == "ethnoracial") {
    dat = dat %>% left_join(racecat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(race_short)))
    colors = race_short_colors
    labels = race_short

  } else if (group == "income") {
    dat = dat %>% left_join(inccat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(inc_cat)))
    colors = inc_cat_colors
    labels = inc_cat

  } else if (group == "ses") {
    dat = dat %>%
      mutate(cat = factor(ses, levels = ses_cat))
    colors = ses_cat_colors
    labels = ses_cat

  } else if (group == "period") {
    dat = dat %>%
      mutate(cat = factor(period, levels = period_cat))
    colors = period_cat_colors
    labels = period_cat

  } else {
    return("Please select valid group: 'gent', 'ethnoracial', 'income', 'ses', 'period")
  }

  # Compute mean or median
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

  if (group == "period") {
    plot <-
      ggplot(dat, aes(x = cat, y = value, fill = cat)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(~ period, scales = "free", space = "free") +
      scale_fill_manual(values = colors) +
      scale_y_continuous(limits = limits, expand = c(0, 0)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_blank(),
            legend.position = "none",
            plot.title = element_text(size = 18, hjust = .5),
            plot.caption = element_text(size = 8, hjust = .5, face = "italic")) +
      labs(title = title, y = y_title, x = "", caption = caption)

  } else {
    plot <-
      ggplot(dat, aes(x = cat, y = value, fill = cat)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(~ period) +
      scale_fill_manual(values = colors,
                        labels = labels) +
      scale_y_continuous(limits = limits, expand = c(0, 0)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(size = 18, hjust = .5),
            plot.caption = element_text(size = 8, hjust = .5, face = "italic")) +
      labs(title = title, y = y_title, x = "", caption = caption)
  }

  if (save) {
    ggsave(savename, plot, height = 5, width = 7)
    return(plot)
  } else {
    return(plot)
  }
}
