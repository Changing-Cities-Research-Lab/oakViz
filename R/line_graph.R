#' Produce line graph of variable by month
#'
#' This function takes in data and produces a line graph of variables by month,
#' grouped by neighborhood category or race.
#'
#' @param dat Data with a column containing variable of interest, fips column, and grouping variable.
#' @param var Name of variable to plot.
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
  dat$fips = as.factor(dat$fips)

  agg = dat %>%
    group_by(cat) %>%
    dplyr::summarise_all(mean) %>%
    select(-fips)

  agg = as.data.frame(agg)

  dat_long = reshape::melt(agg, id.vars = c("cat"))

  dat_long$variable = as.character(dat_long$variable)

  dat_long = dat_long %>%
    mutate(month = stringr::str_sub(variable, start = -2))

  dat_long$month = as.double(dat_long$month)

  dat_long$variable = dat_long$variable %>%
    str_sub(end = -4)

  # Filter for variable to plot here
  data = dat_long %>%
    filter(variable == {{var}})

  plot = ggplot(data, aes(x = month, y = value, group = cat)) +
    geom_line(aes(color = cat), size = 0.8) +
    scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sept", "Oct")) +
    theme_bw() + theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      # Caption
      plot.caption = element_text(size = 9, hjust = .5,face = "italic"),
      # X-axis
      axis.ticks.x=element_blank(),
      # Y-axis
      axis.ticks.y=element_blank(),
      # Background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank())

  if (save) {
    ggsave(savename, plot, height = 6, width = 6)
  }
  return(plot)
}
