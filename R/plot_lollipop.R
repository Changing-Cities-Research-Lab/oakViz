#' Produce lollipop plot by ethnoracial, income, and gentrification category.
#'
#' This function takes in data and produces a horizontal lollipop plot by
#' ethnoracial, income, and gentrification category. The order of these categories
#' can be adjusted by changing the factor levels of the facet variable. Input data
#' needs columns for variable of interest (titled "var") and tract number (titled
#' "tractid10"). Intended to be used after oakViz::aggregate_categories().
#'
#' @param data Data with column for variable of interest.
#' @param var Column name of variable of interest.
#' @param limits Y-axis limits.
#' @param ses Displays SES facet columns when T, default is F.
#' @param reverse Reverses direction of x-axis when T, default is F.
#' @param x_title Title to display along x-axis
#' @param scale_type Y-axis scale type: "numeric" or "percent"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Lollipop plot of variable by ethnoracial, income, and gentrification category, with optional columns by SES.
#' @export
# Lollipop Plot
plot_lollipop <- function(
  data,
  var,
  limits,
  ses = F,
  x_title = "",
  scale_type = "numeric",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
  ) {
  library('tidyverse')

  ### PARAMETERS ###
  gent_cat_colors <-
    c("snow3","#d94801", "#fa7b00", "#fdcc8a", "#a6d894")
  gent_cat <- c("Nongentrifiable", "Intense", "Moderate", "Weak", "People or Price")
  names(gent_cat_colors) <- gent_cat

  race_short_colors <-
    c("#481567FF", "#33638DDF", "#FDE725FF", "#20A387FF")
  race_short <- c("Predominantly Black", "Black-Other", "White/White-Mixed", "Multiethnic/Other")
  names(race_short_colors) <- race_short

  inc_cat_colors <-
    c("#c7cff2","#8897db","#697fe0","#4c66d9","#1437cc")
  inc_cat <- c("Bottom Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Top Quintile")
  names(inc_cat_colors) <- inc_cat

  ses_cat_colors <-
    c("#fcbba1", "#fc9272", "#fb6a4a", "#b63b36")
  ses_cat <- c("Low", "Moderate", "Middle", "High")
  names(ses_cat_colors) <- ses_cat

  labels = c("Overall", gent_cat, race_short, inc_cat, ses_cat)
  colors = c("white", gent_cat_colors, race_short_colors, inc_cat_colors, ses_cat_colors)
  names(colors) = labels

  if(ses) {
    data$ses <- factor(data$ses,
                       levels = c("Low",
                                  "Moderate",
                                  "Middle",
                                  "High"))
  }

  if (scale_type == "percent") {
    label_type = scales::percent
  } else if (scale_type == "numeric") {
    label_type = scales::comma
  } else {
    return("Please select percent or numeric")
  }

  # Have line segment start at 0
  ystart = 0
  if (limits[1] > 0) {
    ystart = limits[1]
  }

  plot <-
    ggplot(data, aes(x = cat, y = {{var}}, fill = cat)) +
    geom_segment(aes(x=cat, xend=cat,
                     y=ystart, yend={{var}}), size=0.25,
                 show.legend = FALSE) +
    geom_point(aes(color = factor(cat)), size = 3.25, shape = 21,
               colour = "black", show.legend = TRUE) +
    geom_hline(yintercept=0, linetype="dashed") +
    scale_y_continuous(limits = limits,
                       expand = c(0, 0),
                       labels = label_type) +
    scale_color_manual(values = colors,
                       labels = labels) +
    scale_fill_manual(values = colors) +
    theme_bw() +
    theme(
      # Panel
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.spacing.x = unit(1, "lines"),
      # Axis
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      axis.title.x = element_text(size = 9),
      # Legend
      legend.position = "none",
      # Caption
      plot.caption = element_text(size = 6, hjust = .5, face = "italic")) +
    labs(y = x_title, caption = caption) +
    coord_flip()

  if (ses) {
    plot = plot +
      facet_grid(rows = vars(facet),
                 cols = vars(ses),
                 scale = "free",
                 space = "free")
    width = 6.8
    height = 5.1

  } else {
    plot = plot +
      facet_grid(rows = vars(facet),
                 scale = "free",
                 space = "free")
    width = 4.5
    height = 4.5
  }

  if (save) {
    ggsave(savename, plot, height = height, width = width)
  }
  return(plot)

}

