#' Aggregation for Oakland variables.
#'
#'Take mean, median, or sum of variables by gentrification, ethnoracial, and income grouping.
#'
#' @param dat Data with a column containing census tracts ("tractid10") and variable of interest ("var").
#' @param compute "mean", "median", or "sum"
#' @param group_by_vars Optional list of variables to group_by before aggregation
#' @return Aggregated data frame.
#' @export
aggregate_categories = function(
  dat,
  compute = "mean",
  group_vars = c(NULL)) {

  library('tidyverse')

  # Relabel names for the graphs
  gent_cat_plot_order <- c("Nongentrifiable", "Gentrifying",
                           "Intense", "Moderate",
                           "Early Gentrification", "Weak", "People or Price")
  race_cat_plot_order <- c("Predominantly White", "Predominantly Black",
                           "Predominantly Other","White-Other","Black-White","Black-Other","Multiethnic",
                           "White/White-Mixed", "Multiethnic/Other")
  inc_cat_plot_order <- c("Bottom Quintile", "Second Quintile", "Middle Quintile",
                          "Fourth Quintile", "Top Quintile")

  # merge oakland tracts with data
  dat = dat %>% right_join(oak_tracts, by = "tractid10")

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
  
  group_vars <- enquo(group_vars)

  if (compute == "mean") {
    data = data %>%
      group_by_at(vars(cat, facet, !!group_vars)) %>%
      dplyr::summarise_all(~ mean(., na.rm = T))
    return(data)
  } else if (compute == "median") {
    data = data %>%
      group_by_at(vars(cat, facet, !!group_vars)) %>%
      dplyr::summarise_all(~ median(., na.rm = T))
    return(data)
  } else if (compute == "sum") {
    data = data %>%
      group_by_at(vars(cat, facet, !!group_vars)) %>%
      dplyr::summarise_all(~ sum(., na.rm = T))
    return(data)
  } else {
    return("Please select mean, median, or sum.")
  }
}
