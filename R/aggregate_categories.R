#' Aggregation for Oakland variables.
#'
#'Take mean, median, or sum of all variables by gentrification, ethnoracial, and income grouping.
#'
#' @param dat Data with a column containing census tracts ("tractid10") and categorical variables to group by or numeric variables to aggregates.
#' @param compute "mean", "median", or "sum"
#' @param group_vars Optional list of variables to group_by before aggregation. Note: All non-numeric variables should either be included here or de-selected.
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
  dat <- dat %>% right_join(oak_tracts, by = "tractid10")

  # Combine gentcat, racecat, & inccat with data
  data <- rbind(
    dat %>% mutate(cat = "Overall", facet = "All"),
    dat %>% left_join(gentcat, by = "tractid10"),
    dat %>% left_join(racecat, by = "tractid10"),
    dat %>% left_join(inccat, by = "tractid10")
  ) %>%
    select(-tractid10) %>%
    mutate(cat = factor(cat, levels = c("Overall", gent_cat_plot_order, race_cat_plot_order, inc_cat_plot_order))) %>%
<<<<<<< HEAD
    mutate(facet = factor(facet, levels = c("All", "Gentrification", "Ethnoracial", "Income"))) %>%
    drop_na()

  # modify mean, median, and sum so that if there are only NAs, it outputs NA
=======
    mutate(facet = factor(facet, levels = c("All", "Gentrification", "Race/Ethnicity", "Income"))) %>%
    filter(!is.na(facet))
  
  # modify mean, median, and sum so that if there are only NAs then it outputs NA
>>>>>>> 7b2339f5c4357be306a65dbd71bb25b9a3b9ec6e
  compute_fn <- function(x, compute) {
    if (all(is.na(x))) {
      x[NA_integer_]
    } else if (compute == "mean") {
      mean(x, na.rm = TRUE)
    } else if (compute == "median") {
      median(x, na.rm = TRUE)
    } else if (compute == "sum") {
      sum(x, na.rm = TRUE)
    } else {
      return("Please select mean, median, or sum.")
    }
  }

  group_vars <- enquo(group_vars)
<<<<<<< HEAD
  data = data %>%
    group_by_at(vars(cat, facet, !!group_vars)) %>%
    dplyr::summarise_all(~ sum)
  return(data)

  group_vars <- enquo(group_vars)
  data = data %>%
=======
  data <- data %>%
>>>>>>> 7b2339f5c4357be306a65dbd71bb25b9a3b9ec6e
    group_by_at(vars(cat, facet, !!group_vars)) %>%
    dplyr::summarise_all(~ compute_fn(., compute)) %>%
    ungroup()
  return(data)
<<<<<<< HEAD

=======
>>>>>>> 7b2339f5c4357be306a65dbd71bb25b9a3b9ec6e
}
