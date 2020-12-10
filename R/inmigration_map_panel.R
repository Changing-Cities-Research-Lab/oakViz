#' Produce 4x4 map panel of inmigration count in Oakland
#'
#' This function takes in data and produces a 4x4 panel of inmigration
#' counts using a gradient color scale, across SES and periods.
#' Not intended to be used with aggregated CCP data.
#'
#' @param data Data with a column containing tractid10, year, ses, and inmigration.
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param jenksbreaks Uses Jenks Breaks when T, otherwise uses continuous color scale
#' @param breaks Gradient scale breaks, either numeric vector or scales::extended_breaks(n = 6)
#' @param limits Gradient scale limits, c(min, max)
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return 4x4 panel maps of inmigration count, by SES and period.
#' @export

inmigration_map_panel <- function(
  data,
  shp_tracts,
  jenksbreaks = T,
  breaks = scales::extended_breaks(n = 6),
  limits = NULL,
  save = F,
  savename = "plot.png",
  caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.") {

  library(sf)
  library(rgdal)
  library(foreach)
  library(ggmap)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(BAMMtools)

  MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

  data = data %>%
    filter(year %in% c("boom", "bust", "recovery", "post_recovery")) %>%
    select(tractid10, year, ses, inmigration) %>%
    group_by(tractid10, year, ses) %>%
    summarise_all(sum)

  data$year = plyr::revalue(data$year,
                            c("boom"="Boom",
                              "bust"="Bust",
                              "recovery"="Recovery",
                              "post_recovery"="Post-Recovery"))

  data = data %>%
    filter(ses %in% c("Low", "Moderate", "Middle", "High"))

  data$year <- factor(data$year,
                      levels = c("Boom",
                                 "Bust",
                                 "Recovery",
                                 "Post-Recovery"))

  data$ses <- factor(data$ses,
                     levels = c("Low",
                                "Moderate",
                                "Middle",
                                "High"))

  # Get max and min values for common gradient scale
  max = max(data$inmigration, na.rm = T)

  min = min(data$inmigration, na.rm = T)

  range = c(min, max)

  # Sets Jenks breaks if T
  if (jenksbreaks) {
    breaks = data %>%
      dplyr::pull(inmigration) %>%
      getJenksBreaks(k = 6)
  }

  # Overrides lim value if user inputs limits
  if (!is.null(limits)) {
    range = limits
  }

  # Combine with shapefiles

  # county tract map
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10)

  data = oak_tracts %>%
    right_join(data, by = c("GEOID10S" = "tractid10")) %>%
    st_transform(CRS("+proj=longlat +datum=WGS84"))

  # Read in list of tracts in the Bay Area above the minimum population for display
  tracts_use <-
    oak_ids

  # Read in list of Oakland tract ids
  oak_ids <-
    oak_ids %>%
    subset(trtid10 %in% tracts_use$trtid10)

  # map data
  # Google Street Map for Oakland ----
  gmap_oak <- get_stamenmap(
    bbox = c(-122.3547, 37.6920, -122.1048, 37.890692),
    zoom = 12,
    maptype = "toner-lite",
    color = "bw")

  # produce map
  map <-
    ggmap(gmap_oak) +
    geom_sf(
      data = data,
      aes(fill = inmigration),
      size = 0,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = data,
      size = 0.1,
      alpha = 0,
      inherit.aes = FALSE,
      color = "black"
    ) +
    guides(
      fill =
        guide_colorbar(
          barheight = 0.5,
          barwidth = 15,
          title = NULL,
          frame.colour = "black")
    ) +
    facet_grid(rows = vars(year),
               cols = vars(ses),
               switch = "y") +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 12, hjust = .5, vjust = 3),
      plot.margin = margin(3,-.5,3,-.5, unit = "pt"),
      plot.caption = element_text(size = 8),
      panel.border = element_rect(colour = "black", fill=NA),
      strip.text.y.left = element_text(angle = 0)
    )

  if (jenksbreaks) {
    map = map +
      scale_fill_fermenter(breaks = breaks,
                           type = "seq",
                           palette = "YlOrRd",
                           direction = 1)
  } else {
    map = map +
      scale_fill_gradientn(
        breaks = breaks,
        labels = scales::comma,
        colors = alpha(MAP_COLORS, .8),
        limits = range,
        na.value = "grey60")
  }

  if (save) {
    ggsave(savename, map, height = 4.97, width = 6.33)
    return(map)
  } else {
    return(map)
  }
}
