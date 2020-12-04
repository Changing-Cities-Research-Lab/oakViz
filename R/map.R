#' Produce Oakland map of continuous variable with gradient color scale
#'
#' This function takes in data and produces a census tract map of Oakland
#' representing the variable using a gradient color scale. Census tract
#' column must be named "tractid10".
#'
#' @param data Data with a column containing census tracts and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param palette Color palette: "sequential" (default) or "diverging"
#' @param jenksbreaks Uses Jenks Breaks when T, default is F.
#' @param breaks Gradient scale breaks, either numeric vector or scales::extended_breaks(n = 6)
#' @param labels Gradient scale labels, either character vector or scales::percent or scales::comma
#' @param limits Manual limits for color scale, numeic vector: c(min, max)
#' @param coord T if plotting coordinate values (lat, lon). Default is F.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Map of variable of interest.
#' @export
## Single Map
make_map <- function(data,
                     var,
                     shp_tracts,
                     palette = "sequential",
                     jenksbreaks = F,
                     breaks = scales::extended_breaks(n = 6),
                     labels = scales::percent,
                     limits = NULL,
                     coord = F,
                     save = F,
                     savename = "plot.png",
                     caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.") {

  library(tidyverse)
  library(sf)
  library(rgdal)
  library(ggmap)
  library(gridExtra)
  library(grid)
  library(scales)
  library(BAMMtools)

  # Adjust color palette
  if (palette == "sequential") {
    lim = NULL

    # Sequential palette
    MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

  } else if (palette == "diverging") {

    # Get limits to center diverging palette around 0
    lim <- data %>%
      select({{var}}) %>%
      abs() %>%
      max(na.rm = T) * c(-1, 1)

    # Diverging palette
    MAP_COLORS <- rev(RColorBrewer::brewer.pal(n = 9, name = "RdBu"))

  } else {
    return("Please select sequential or diverging color palette.")
  }

  # Overrides lim value if user inputs limits
  if (!is.null(limits)) {
    lim = limits
  }

  # Sets Jenks breaks if T
  if (jenksbreaks) {
    breaks = data %>%
      dplyr::pull({{var}}) %>%
      getJenksBreaks(k = 5)
  }

  # county tract map
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10)

  data$tractid10 = as.numeric(data$tractid10)

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

  map <-
    ggmap(gmap_oak) +
    geom_sf(
      data = data,
      aes(fill = {{var}}),
      size = 0,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = data,
      size = 0.3,
      alpha = 0,
      inherit.aes = FALSE,
      color = "black"
    ) +
    scale_fill_gradientn(
      breaks = breaks,
      labels = labels,
      colors = alpha(MAP_COLORS, .8),
      limits = lim
    ) +
    guides(
      fill =
        guide_colorbar(
          barheight = 0.5,
          barwidth = 15,
          title = NULL
        )
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(3,0,0,0, unit = "pt"),
      plot.title = element_blank(),
      plot.margin = margin(3,1,3,1, unit = "pt"),
      plot.caption = element_text(size = 6, hjust = .5)
    ) +
    labs(caption = caption)
  if (coord == T) {
    + geom_point(
      data = data,
      aes(x = lon, y = lat),
      color = "navy", size = 2
    )
  }

  if (save) {
    ggsave(savename, map, height = 5, width = 5)
    return(map)
  } else {
    return(map)
  }
}
