#' Produce destination map panel of continuous variable with gradient color scale
#'
#' This function takes in data and produces a panel of destination maps of
#' Oakland movers using a gradient color scale, across four distinct periods.
#' Intended to be used with oakViz::aggregate_categories().
#'
#' @param data Data aggregated with aggregate_categories().
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Destination map panel across four periods.
#' @export

make_dest_map_panel <- function(
  data,
  shp_tracts,
  save = F,
  savename = "plot.png",
  caption = "\nSES Ranges by Equifax Risk Scores: Low = missing or <580, Moderate = 580-649, Middle = 650-749, High = 750+\nHousing Period Ranges: Boom = 2002-2006, Bust = 2007-2009, Recovery = 2010-2014, Post-Recovery = 2015-2017.") {

  library(tidyverse)
  library(sf)
  library(rgdal)
  library(ggmap)

  county_names <-
    c("Alameda County",
      "Contra Costa County",
      "Marin County",
      "Napa County",
      "San Francisco County",
      "San Mateo County",
      "Santa Clara County",
      "Solano County",
      "Sonoma County")

  # Colors
  MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

  ## Geometry and Basemap
  cities <- cities %>%
    filter(county %in% county_names)
  bay_area <- shp_tracts

  city_geometry <-
    bay_area %>%
    filter(GEOID10S %in% cities$trtid10) %>%
    left_join(cities, by = c("GEOID10S" = "trtid10")) %>%
    group_by(city) %>%
    summarize(geometry = st_union(geometry))

  # Only need to use Google API code once
  # register_google("AIzaSyCO-hk4AjUgTdMKDuv18f66py8NIdrf4qU")
  gmap_bay <-
    get_stamenmap(
      bbox = c(-122.533820, 37.636921, -122.081959, 37.9148),
      zoom = 12,
      maptype = "toner-lite",
      color = "bw"
    )

  ## Read and Clean Data

  data = data %>%
    select(year,
           cat,
           withinoakmigration,
           outmigration_alameda,
           outmigration_contracosta,
           outmigration_sanfran) %>%
    filter(year %in% c("boom", "bust", "recovery", "post_recovery")) %>%
    filter (cat == "Overall") %>%
    select(-cat) %>%
    group_by(year) %>%
    summarise_all(sum) %>%
    melt(id = c("year"))

  data$variable <- plyr::revalue(data$variable,
                                 c("withinoakmigration"="Oakland city",
                                   "outmigration_alameda" = "Alameda city",
                                   "outmigration_contracosta" = "Contra Costa city",
                                   "outmigration_sanfran" = "San Francisco city"))

  data$year = plyr::revalue(data$year,
                            c("boom"="Boom",
                              "bust"="Bust",
                              "recovery"="Recovery",
                              "post_recovery"="Post-Recovery"))
  data = data %>%
    mutate(city = variable)

  # Combine with geometry data
  data <-
    city_geometry %>%
    left_join(data, by = c("city")) %>%
    st_transform(CRS("+proj=longlat +datum=WGS84")) %>%
    filter(!is.na(value))

  labels <-
    data %>%
    mutate(geometry = st_centroid(geometry))

  ## Make Maps
  map <-
    ggmap(gmap_bay) +
    geom_sf(
      data = data,
      aes(fill = value),
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
    geom_sf_label(
      data = labels,
      aes(label = variable %>% str_to_title()),
      #nudge_x = 0.1,
      size = 2,
      inherit.aes = FALSE
    ) +
    facet_wrap(as.formula(str_glue("~ ", "year"))) +
    scale_fill_gradientn(
      breaks = scales::breaks_extended(n = 7),
      colors = alpha(MAP_COLORS, .8),
      labels = scales::label_comma(),
      na.value = "grey60"
    ) +
    guides(
      fill =
        guide_colorbar(
          barheight = 0.8,
          barwidth = 27,
          title = NULL,
          frame.colour = "black"
        )
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.margin = margin(.5,0,.5,0, unit = "cm"),
      plot.margin = margin(5,5,5,5, unit = "pt"),
      plot.caption = element_text(size = 8.5, hjust = .5, face = "italic"),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.spacing = unit(1, "lines"),
      strip.text.x =
        element_text(size = 12, margin = margin(3, 3, 3, 3, unit = "pt"))
    ) +
    labs(caption = caption)

  if (save) {
    ggsave(savename, map, height = 7.5, width = 7)
    return(map)
  } else {
    return(map)
  }
}
