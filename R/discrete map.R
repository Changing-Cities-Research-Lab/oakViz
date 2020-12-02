#' Produce Oakland map of discrete variable
#'
#' This function takes in category (gentrification, income, or ethnoracial) and
#' produces a census tract map of Oakland representing a discrete variable.
#'
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param discrete_cat Discrete category to plot: gent, income, ethnoracial
#' @param coord T if plotting coordinate values (lat, lon). Default is F.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Map of variable of interest.
#' @export

make_discrete_map <- function(shp_tracts,
                              discrete_cat = "gent",
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

  # Omit 3 tracts****

  gent_cat_colors <-
    c("snow3","#d94801", "#fa7b00", "#fdcc8a", "#a6d894")
  gent_cat <- c("Nongentrifiable", "Intense", "Moderate", "Weak", "People or Price")

  race_short_colors <-
    c("#481567FF", "#33638DDF", "#FDE725FF", "#20A387FF")
  race_short <- c("Predominantly Black", "Black-Other", "White/White-Mixed", "Multiethnic/Other")

  inc_cat_colors <-
    c("#c7cff2","#8897db","#697fe0","#4c66d9","#1437cc")
  inc_cat <- c("Bottom Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Top Quintile")

  if (discrete_cat == "gent") {
    data = gentcat

    breaks = c("1. Intense",
               "2. Moderate",
               "3. Weak",
               "4. People or Price",
               "5. Nongentrifiable")

    values = c("1. Intense"="#e31a1c",
               "2. Moderate"="#fb9a99",
               "3. Weak"="#fdbf6f",
               "4. People or Price"="#b2df8a",
               "5. Nongentrifiable"="#dedede")

    labels = c("Intense",
               "Moderate",
               "Weak",
               "People or Price",
               "Nongentrifiable")

  } else if (discrete_cat == "income") {
    data = inccat

    breaks = c("1. Bottom Quintile",
               "2. Second Quintile",
               "3. Middle Quintile",
               "4. Fourth Quintile",
               "5. Top Quintile")

    values = c("1. Bottom Quintile" = "#c7cff2",
               "2. Second Quintile" = "#8897db",
               "3. Middle Quintile" = "#697fe0",
               "4. Fourth Quintile" = "#4c66d9",
               "5. Top Quintile" = "#1437cc")

    labels = c("Bottom Quintile",
               "Second Quintile",
               "Middle Quintile",
               "Fourth Quintile",
               "Top Quintile")

  } else if (discrete_cat == "ethnoracial") {
    data = racecat

    breaks = c("1. Predominantly Black",
               "2. Black-Other",
               "3. White/White-Mixed",
               "4. Multiethnic/Other")

    values = c("1. Predominantly Black" = "#481567FF",
               "2. Black-Other" = "#33638DDF",
               "3. White/White-Mixed" = "#FDE725FF",
               "4. Multiethnic/Other" = "#20A387FF")

    labels = c("Predominantly Black",
               "Black-Other",
               "White/White-Mixed",
               "Multiethnic/Other")

  } else {
    return("Please select gent, income, or ethnoracial")
  }

  # county tract map
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10)

  data = oak_tracts %>%
    left_join(data, by = c("GEOID10S" = "tractid10")) %>%
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
      aes(fill = cat),
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
    scale_fill_manual(
      breaks = breaks,
      values = values,
      labels = labels,
      na.value = "grey60",
      guide = "legend"
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "right",
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
