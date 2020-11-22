#' Aggregation for Oakland variables.
#'
#'Take mean, median, or sum of variables by gentrification, ethnoracial, and income grouping.
#'
#' @param dat Data with a column containing census tracts ("tractid10") and variable of interest ("var").
#' @param compute "mean", "median", or "sum"
#' @return Aggregated data frame.
#' @export
aggregate_categories = function(
  dat,
  compute
) {
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

  dat = dat %>% inner_join(oak_tracts, by = "tractid10") %>%
    select(tractid10, var)

  # Combine gentcat, racecat, & inccat with data
  data <- rbind(
    dat %>% mutate(cat = "Overall", facet = "All"),
    dat %>% left_join(gentcat, by = "tractid10"),
    dat %>% left_join(racecat, by = "tractid10"),
    dat %>% left_join(inccat, by = "tractid10")
  ) %>%
    select(-tractid10) %>%
    mutate(cat = factor(cat, levels = c("Overall", gent_cat_plot_order, race_cat_plot_order, inc_cat_plot_order))) %>%
    mutate(facet = factor(facet, levels = c("All", "Gentrification", "Race/Ethnicity", "Income"))) %>%
    drop_na()

  if (compute == "mean") {
    data = data %>%
      group_by(cat, facet) %>%
      dplyr::summarise_all(funs(mean(., na.rm = T)))
    return(data)
  } else if (compute == "median") {
    data = data %>%
      group_by(cat, facet) %>%
      dplyr::summarise_all(funs(median(., na.rm = T)))
    return(data)
  } else if (compute == "sum") {
    data = data %>%
      group_by(cat, facet) %>%
      dplyr::summarise_all(funs(sum(., na.rm = T)))
    return(data)
  } else {
    return("Please select mean, median, or sum.")
  }
}
