% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_map_dest_panel.R
\name{make_dest_map_panel}
\alias{make_dest_map_panel}
\title{Produce destination map panel of continuous variable with gradient color scale}
\usage{
make_dest_map_panel(
  data,
  shp_tracts,
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
)
}
\arguments{
\item{data}{Data aggregated with aggregate_categories().}

\item{shp_tracts}{"US_tract_2010.shp" loaded object}

\item{save}{T if user would like to return plot object and save file, F (default) to just return object.}

\item{savename}{File name of map for saving.}

\item{caption}{Figure caption}
}
\value{
Destination map panel across four periods.
}
\description{
This function takes in data and produces a panel of destination maps of
Oakland movers using a gradient color scale, across four distinct periods.
Filters SES category to Low, Moderate, and Middle.
Intended to be used with oakViz::aggregate_categories().
}
