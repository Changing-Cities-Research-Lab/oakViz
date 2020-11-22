#' Produce lollipop plot by gentrification, ethnoracial, and income category.
#'
#' This function takes in data and produces a horizontal lollipop plot by
#' gentrification, ethnoracial, and income category. Input data needs columns for
#' variable of interest (titled "var") and tract number (titled "tractid10").
#'
#' @param data Data with a column containing census tracts and variable of interest.
#' @param limits Y-axis limits. Default is (min, max) of variable of interest.
#' @param compute "mean", "median", or "sum"
#' @param y_title Y-axis title
#' @param title Figure title
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption
#' @return Lollipop plot of variable by gentrification, ethnoracial, and income category.
#' @export
# Lollipop Plot
plot_lollipop <- function(
  dat,
  limits = c(min(dat$var), max(dat$var)),
  compute = "mean",
  y_title = "Y-axis Title",
  title = "Title",
  save = F,
  savename = "plot.png",
  caption = "Data was aggregated by taking mean of all tracts in a category"
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
    group_by(cat, facet) %>%
      dplyr::summarise_all((mean(., na.rm = T)))
  } else if (compute == "median") {
    group_by(cat, facet) %>%
      dplyr::summarise_all((median(., na.rm = T)))
  } else if (compute == "sum") {
    group_by(cat, facet) %>%
      dplyr::summarise_all((sum(., na.rm = T)))
  } else {
    return("Please select mean, median, or sum.")
  }

  plot <-
    ggplot(data, aes(x = cat, y = var, fill = cat)) +
    geom_segment(aes(x=cat, xend=cat,
                     y=limits[1], yend=var), size=0.25,
                 show.legend = FALSE) +
    geom_point(aes(color = factor(cat)),size = 3.25,shape = 21,
               colour = "black",show.legend = TRUE) +
    facet_grid(rows = vars(facet), scale = "free", space = "free") +
    scale_color_manual(values = colors,
                       labels = labels) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(limits = limits, expand = c(0, 0)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()
    ) +
    theme(axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") +
    labs(title = title, y = y_title, x= "") +
    theme(plot.title = element_text(size = 18, hjust = .5),
          plot.caption = element_text(size = 8, hjust = .5, face = "italic")) +
    coord_flip()
  if (save == F) {
    return(plot)
  } else {
    return(plot)
    ggsave(savename, plot, height = 4.5, width = 4.5)
  }
}

