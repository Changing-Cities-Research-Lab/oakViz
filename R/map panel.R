#' Produce Oakland map panel of continuous variable with gradient color scale
#'
#' This function takes in data and produces a panel of census tract maps of
#' Oakland representing the variable using a gradient color scale, across
#' four distinct periods. Should have "tractid10" column for census tracts.
#'
#' @param data Data with a column containing census tracts and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param coord T if plotting coordinate values (lat, lon)
#' @param savename File name of map for saving.
#' @param title Title for panel of maps.
#' @param scale_label Adjust scales of gradient key (need to fix).
#' @param periods Name of column containing four distinct time periods for mapping.
#' @return Map panel of variable of interest across four periods.
#' @export

make_map_panel <- function(data, var, coord = F, savename, title,
                           scale_label = scales::label_comma(), periods) {
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

  # Get max and min values for common gradient scale
  max = data[var] %>%
    st_drop_geometry() %>%
    max(na.rm = T)
  min = data[var] %>%
    st_drop_geometry() %>%
    min(na.rm = T)

  maps_all = list()
  period_panels = unique(data$periods)

  # Get common legend
  legend_map <-
    ggmap(gmap_oak) +
    geom_sf(
      data = data,
      aes(fill = !! sym(var)),
      size = 0,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    scale_fill_gradientn(
      breaks = scales::extended_breaks(n = 6),
      labels = scale_label,
      limits = c(min, max),
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
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(3,0,0,0, unit = "pt"),
    )

  # Save legend object
  tmp <- ggplot_gtable(ggplot_build(legend_map))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]

  # Plot maps
  foreach(i = 1:length(period_panels)) %do% {
    data_period = data %>%
      filter(periods == period_panels[i])

    map <-
      ggmap(gmap_oak) +
      geom_sf(
        data = data_period,
        aes(fill = !! sym(var)),
        size = 0,
        alpha = 0.5,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = data_period,
        size = 0.1,
        alpha = 0,
        inherit.aes = FALSE
      ) +
      scale_fill_gradientn(
        breaks = scales::extended_breaks(n = 6),
        labels = scale_label,
        limits = c(min, max),
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
        legend.position = "none",
        plot.title = element_text(size = 12, hjust = .5, vjust = 3),
        plot.margin = margin(3,-.5,3,-.5, unit = "pt"),
        plot.caption = element_text(size = 8)
      ) +
      labs(title = period_panels[i])
    if (coord == T) {
      + geom_point(
        data = data,
        aes(x = lon, y = lat),
        color = "navy", size = 2
      )
    }


    # add map to list of grobs
    maps_all = c(maps_all, list(map))
  }

  # arrange period maps into 4 panels
  layout <- rbind(c(1, 2), c(3, 4), c(5, 5))
  map_panel =
    grid.arrange(maps_all[[1]], maps_all[[2]], maps_all[[3]], maps_all[[4]],
                 legend,
                 nrow = 3, ncol = 2,
                 layout_matrix = layout,
                 heights = c(5, 5, 1),
                 top=textGrob(title,
                              gp=gpar(fontsize=21,font=2)))
  return(map_panel)
}
