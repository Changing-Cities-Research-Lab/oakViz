#' Produce stacked bar charts with SES facets and period x-axis grouping.
#'
#' This function takes in data and produces a panel of stacked bar charts,
#' based on different groupings. Intended to be used with oakViz::aggregate_categories()
#'
#' @param dat Data with a column containing variables of interest and grouping variables.
#' @param fill Name of column containing fill variable: "move", "dest" (default).
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Stacked bar charts with SES facets, period x-axis groupings
#' @export

plot_bar_ses <- function(
  dat,
  fill = "dest", # "move"
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
) {
  library(rgdal)
  library(foreach)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(reshape2)

  theme =
    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 8),
      legend.position = "right",
      legend.direction = "vertical",
      # Caption
      plot.caption = element_text(size = 10, hjust = .5, face = "italic"),
      # X-axis
      axis.ticks.x = element_blank(),
      # Y-axis
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 10),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      # Margins
      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))

  dat = dat %>%
    filter(year %in% c("Boom", "Bust", "Recovery", "Post-Recovery"))

  dat$year <- factor(dat$year,
                     levels = c("Boom", "Bust", "Recovery", "Post-Recovery"))

  dat$x_group = dat$year
  x_labels = c("Boom", "Bust", "Recovery", "Post-Recovery")

  # Fill grouping: move, or dest
  if (fill == "move") {
    # Compute share of total moves by move type
    dat = dat %>%
      select(x_group,
             cat,
             facet,
             ses,
             outmigration_outba,
             outmigration_outoak,
             withinoakmigration) %>%
      group_by(x_group,
               cat,
               facet,
               ses) %>%
      summarise_all(sum) %>%
      mutate(denom = outmigration_outba +
               (outmigration_outoak - outmigration_outba) +
               withinoakmigration) %>%
      mutate(moved_outba_pct = outmigration_outba/denom) %>%
      mutate(diff_city_ba_pct = (outmigration_outoak - outmigration_outba)/denom) %>%
      mutate(moved_within_oak_pct = withinoakmigration/denom) %>%
      select(x_group,
             cat,
             facet,
             ses,
             moved_outba_pct,
             diff_city_ba_pct,
             moved_within_oak_pct)

    # Melt data for plotting
    dat = dat %>%
      melt(id = c("cat", "x_group", "facet", "ses"))

    # Rename/order/select colors/label move categories
    dat$variable <- plyr::revalue(dat$variable,
                                  c("moved_outba_pct"="Moved out of Bay Area",
                                    "diff_city_ba_pct" = "Different City within Bay Area",
                                    "moved_within_oak_pct" = "Moved within Oakland"))

    dat$variable <- factor(dat$variable,
                           levels = c("Moved out of Bay Area",
                                      "Different City within Bay Area",
                                      "Moved within Oakland"))
    dat$fill = dat$variable

    values = c("Moved out of Bay Area" = "#8baf3e",
               "Different City within Bay Area" = "#fdbd3b",
               "Moved within Oakland" = "#2e5e8b")

    fill_labels = c("Moved out of Bay Area",
                    "Different City within Bay Area",
                    "Moved within Oakland")

  } else if (fill == "dest") {
    # Compute share of moves by destination
    dat = dat %>%
      select(x_group,
             cat,
             facet,
             ses,
             outmigration_outba,
             withinoakmigration,
             outmigration_alameda,
             outmigration_contracosta,
             outmigration_northbay,
             outmigration_sanfran,
             outmigration_southbay) %>%
      group_by(x_group,
               cat,
               facet,
               ses) %>%
      summarise_all(sum) %>%
      mutate(denom =
               outmigration_outba +
               withinoakmigration +
               outmigration_alameda +
               outmigration_contracosta +
               outmigration_northbay +
               outmigration_sanfran +
               outmigration_southbay) %>%
      mutate(outmigration_outba_pct = outmigration_outba/denom) %>%
      mutate(withinoakmigration_pct = withinoakmigration/denom) %>%
      mutate(outmigration_alameda_pct = outmigration_alameda/denom) %>%
      mutate(outmigration_contracosta_pct = outmigration_contracosta/denom) %>%
      mutate(outmigration_northbay_pct = outmigration_northbay/denom) %>%
      mutate(outmigration_sanfran_pct = outmigration_sanfran/denom) %>%
      mutate(outmigration_southbay_pct = outmigration_southbay/denom) %>%
      select(x_group,
             cat,
             facet,
             ses,
             outmigration_outba_pct,
             withinoakmigration_pct,
             outmigration_alameda_pct,
             outmigration_contracosta_pct,
             outmigration_northbay_pct,
             outmigration_sanfran_pct,
             outmigration_southbay_pct)

    # Melt data for plotting
    dat = dat %>%
      melt(id = c("cat", "x_group", "facet", "ses"))

    # Rename/order/select colors/label move categories
    dat$variable <- plyr::revalue(dat$variable,
                                  c("outmigration_outba_pct" = "Outside Bay Area",
                                    "withinoakmigration_pct" = "Within Oakland",
                                    "outmigration_alameda_pct" = "Alameda",
                                    "outmigration_contracosta_pct" = "Contra Costa",
                                    "outmigration_northbay_pct" = "North Bay",
                                    "outmigration_sanfran_pct" = "San Francisco",
                                    "outmigration_southbay_pct" = "South Bay"))

    dat$variable <- factor(dat$variable,
                           levels = c("Outside Bay Area",
                                      "South Bay",
                                      "San Francisco",
                                      "North Bay",
                                      "Contra Costa",
                                      "Alameda",
                                      "Within Oakland"))

    dat$fill = dat$variable

    values = c("Outside Bay Area" = "#d53e4f",
               "South Bay" = "#fc8d59",
               "San Francisco" = "#fee08b",
               "North Bay" = "#ffffbf",
               "Contra Costa" = "#e6f598",
               "Alameda" = "#99d594",
               "Within Oakland" = "#3288bd")

    fill_labels = c("Outside Bay Area",
                    "South Bay",
                    "San Francisco",
                    "North Bay",
                    "Contra Costa",
                    "Alameda",
                    "Within Oakland")

  } else {
    return("Please select 'move', or 'dest'")
  }

  plot <-
    ggplot(dat, aes(y = value,
                    x = x_group,
                    fill = fill)) +
    geom_bar(stat="identity", position = "stack") +
    facet_grid(cols = vars(ses)) +
    scale_fill_manual(values = values,
                      labels = fill_labels) +
    scale_x_discrete(
      labels = x_labels) +
    scale_y_continuous(expand = c(0, 0.01), labels = scales::percent) +
    labs(x = NULL, y = NULL) +
    theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (save) {
    ggsave(savename, plot, height = 4, width = 6.5)
  }
  return(plot)
}

