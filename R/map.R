#' Produce Oakland map of continuous variable with gradient color scale
#'
#' This function takes in data and produces a census tract map of Oakland
#' representing the variable using a gradient color scale. Census tract
#' column must be named "tractid10".
#'
#' @param data Data with a column containing census tracts and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param title Figure title
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
                     title = "Title",
                     coord = F,
                     save = F,
                     savename = "plot.png",
                     caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.\n") {

  library(devtools)
  library(roxygen2)
  library(tidyverse)
  library(sf)
  library(rgdal)
  library(ggmap)
  library(foreach)
  library(gridExtra)
  library(grid)
  library(scales)
  library(dplyr)

  scale_label = scales::label_comma()

  # county tract map
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10)

  data = oak_tracts %>%
    left_join(data, by = c("GEOID10S" = "tractid10")) %>%
    st_transform(CRS("+proj=longlat +datum=WGS84"))

  # If you want to change colors for any reason
  MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

  # Read in list of tracts in the Bay Area above the minimum population for display
  tracts_use <-
    oak_ids

  # Read in list of Oakland tract ids
  oak_ids <-
    oak_ids %>%
    subset(trtid10 %in% tracts_use$trtid10)

  # map data
  # Google Street Map for Oakland ----
  gmap_oak <-
    get_map(
      location = c(-122.3547, 37.6920, -122.1048, 37.8607),
      maptype = "roadmap",
      source = "google",
      color = "bw"
    )

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
      size = 0.1,
      alpha = 0,
      inherit.aes = FALSE
    ) +
    scale_fill_gradientn(
      breaks = scales::extended_breaks(n = 6),
      labels = scale_label,
      colors = alpha(MAP_COLORS, .8),
      na.value = "grey60"
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
      plot.title = element_text(size = 12, hjust = .5, vjust = 3),
      plot.margin = margin(3,1,3,1, unit = "pt"),
      plot.caption = element_text(size = 8)
    ) +
    labs(title = title, caption = caption)
  if (coord == T) {
    + geom_point(
      data = data,
      aes(x = lon, y = lat),
      color = "navy", size = 2
    )
  }

  if (save) {
    ggsave(savename, map)
    return(map)
  } else {
    return(map)
  }
}
