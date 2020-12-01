#' Produce grouped bar chart across four periods.
#'
#' This function takes in data and produces a bar chart grouped by either
#' gentrification, ethnoracial, income, ses, or period category across four housing periods.
#'
#' @param dat Data with a column containing census tracts and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param limits Y-axis limits
#' @param group Category for x-axis grouping: "gent" (default), "ethnoracial", "income", "ses", or "period"
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
  group = "gent", # gent, ethnoracial, income, ses, period
  save = F,
  savename = "plot.png",
  caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.\n"
) {
  ## Read Data
  library('tidyverse')

  ### PARAMETERS ###
  gent_cat_colors <-
    c("snow3","#d94801", "#fa7b00", "#fdcc8a", "#a6d894")
  gent_cat <- c("Nongentrifiable", "Intense", "Moderate", "Weak", "People or Price")
  names(gent_cat_colors) <- gent_cat
  
  race_short_colors <-
    c("#481567FF", "#33638DDF", "#FDE725FF", "#20A387FF")
  race_short <- c("Predominantly Black", "Black-Other", "White/White-Mixed", "Multiethnic/Other")
  names(race_cat_colors) <- race_cat
  
  inc_cat_colors <-
    c("#c7cff2","#8897db","#697fe0","#4c66d9","#1437cc")
  inc_cat <- c("Bottom Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Top Quintile")
  names(inc_cat_colors) <- inc_cat

  ses_cat_colors <-
    c("#9b9b9b", "#fcbba1", "#fc9272", "#faab8c","#fb6a4a", "#b63b36")
  ses_cat = c("All", "Low", "Moderate", "LMM" ,"Middle", "High")
  names(ses_cat_colors) <- ses_cat

  period_cat_colors <-
    c("#46aac8", "#46aac8", "#46aac8", "#46aac8")
  period_cat = c("Boom", "Bust", "Recovery", "Post-Recovery")
  names(period_cat_colors) <- period_cat

  # Combine with either gentcat, racecat, inccat, ses, or period
  if (group == "gent") {
    dat = dat %>% #left_join(gentcat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(gent_cat))) %>%
      filter(!is.na(cat))

    colors = gent_cat_colors
    labels = gent_cat

  } else if (group == "ethnoracial") {
    dat = dat %>% #left_join(racecat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(race_short))) %>%
      filter(!is.na(cat))

    colors = race_short_colors
    labels = race_short

  } else if (group == "income") {
    dat = dat %>% #left_join(inccat, by = "tractid10") %>%
      mutate(cat = factor(cat, levels = c(inc_cat))) %>%
      filter(!is.na(cat))

    colors = inc_cat_colors
    labels = inc_cat

  } else if (group == "ses") {
    dat = dat %>%
      mutate(cat = factor(ses, levels = ses_cat)) %>%
      filter(!is.na(cat))

    colors = ses_cat_colors
    labels = ses_cat

  } else if (group == "period") {
    dat = dat %>%
      mutate(cat = factor(period, levels = period_cat)) %>%
      filter(!is.na(cat))

    colors = period_cat_colors
    labels = period_cat
  } else {
    return("Please select valid group: 'gent', 'ethnoracial', 'income', 'ses', 'period")
  }

  if (group == "period") {
    plot <-
      ggplot(dat, aes(x = cat, y = {{ var }}, fill = cat)) +
      geom_bar(stat = "identity", position = "stack", width = 0.5) +
      facet_grid(~ period, scales = "free", space = "free") +
      scale_fill_manual(values = colors) +
      scale_y_continuous(limits = limits,
                         expand = c(0, 0),
                         labels = scales::percent) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none",
            plot.title = element_blank(),
            plot.caption = element_text(size = 7, hjust = .5, face = "italic")) +
      labs(caption = caption)

  } else {
    plot <-
      ggplot(dat, aes(x = cat, y = {{ var }}, fill = cat)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(~ period) +
      scale_fill_manual(values = colors,
                        labels = labels) +
      scale_y_continuous(limits = limits,
                         expand = c(0, 0),
                         labels = scales::percent) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none",
            plot.title = element_blank(),
            plot.caption = element_text(size = 7, hjust = .5, face = "italic")) +
      labs(caption = caption)
  }

  height = 5
  width = 7
  if (group == "period") {
    height = 4
    width = 5
  }

  if (save) {
    ggsave(savename, plot, height = height, width = width)
  }
  return(plot)
}
