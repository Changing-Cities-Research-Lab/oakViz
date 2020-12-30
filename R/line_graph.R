#' Produce line graph of variable by month
#'
#' This function takes in data and produces a line graph of variables by month,
#' grouped by neighborhood category or race.
#'
#' @param dat Data with a column containing variable of interest, fips column, and grouping variable.
#' @param var Name of variable to plot, as a string.
#' @param group Category for color grouping: "race" (default) or "neighborhood"
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Line graph of variable by month, grouped by race or neighborhood category
#' @export

line_graph <- function(
  dat,
  var,
  group = "race",
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
) {
  # dat$fips = as.factor(dat$fips)
  #
  # agg = dat %>%
  #   group_by(cat) %>%
  #   dplyr::summarise_all(mean) %>%
  #   select(-fips)
  #
  # agg = as.data.frame(agg)
  #
  # dat_long = reshape::melt(agg, id.vars = c("cat"))
  #
  # dat_long$variable = as.character(dat_long$variable)
  #
  # dat_long = dat_long %>%
  #   mutate(month = stringr::str_sub(variable, start = -2))
  #
  # # Removes leading zeros
  # dat_long$month = as.double(dat_long$month)
  #
  # # Factor for x-axis labels
  # dat_long$month = as.factor(dat_long$month)
  #
  # dat_long$variable = dat_long$variable %>%
  #   str_sub(end = -4)
  #
  # # Filter for variable to plot here
  # data = dat_long %>%
  #   filter(variable == var)

  plot = ggplot(dat, aes(x = month, y = value, group = cat)) +
    geom_line(aes(color = cat), size = 0.8) +
    scale_x_discrete(labels=c("1" = "Jan",
                              "2" = "Feb",
                              "3" = "Mar",
                              "4" = "Apr",
                              "5" = "May",
                              "6" = "Jun",
                              "7" = "Jul",
                              "8" = "Aug",
                              "9" = "Sept",
                              "10" = "Oct"),
                     expand = c(0.03, 0.03)) +
    theme_bw() + theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      # Caption
      plot.caption = element_text(size = 9, hjust = .5,face = "italic"),
      # X-axis
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 9),
      # Y-axis
      axis.ticks.y=element_blank(),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank()) +
    guides(color = guide_legend(nrow = 1))

  if (save) {
    ggsave(savename, plot, height = 5, width = 6)
  }
  return(plot)
}
