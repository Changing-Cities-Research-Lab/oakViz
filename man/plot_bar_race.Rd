% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bar_race.R
\name{plot_bar_race}
\alias{plot_bar_race}
\title{Produce grouped bar chart by race across 2 periods.}
\usage{
plot_bar_race(
  dat,
  var,
  limits,
  y_title = NULL,
  scale_type = "percent",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
)
}
\arguments{
\item{dat}{Data with a column containing "race", "periods", and variable of interest}

\item{var}{Name of column containing variable to plot.}

\item{limits}{Y-axis limits}

\item{y_title}{Title to display along y-axis.}

\item{scale_type}{Y-axis scale type: "numeric" or "percent"}

\item{save}{T if user would like to return plot object and save file, F (default) to just return object.}

\item{savename}{File name of map for saving.}

\item{caption}{Caption for figure}
}
\value{
Grouped bar chart by race across 2 periods.
}
\description{
This function takes in data and produces a bar chart grouped by race
across two periods (2006-2010 and 2012-2016).
}
