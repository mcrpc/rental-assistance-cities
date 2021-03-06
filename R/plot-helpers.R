# Set up a custom theme for all the plots in this analysis
theme_fc <- function(...) {
  
  hrbrthemes::theme_ipsum(
    axis_title_just = "mc",
    subtitle_margin = 10,
    base_size = 11, 
    plot_title_size = 14, 
    subtitle_size = 11, 
    strip_text_size = 11,
    axis_text_size  = 8, 
    axis_title_size = 11
  ) %+replace%
    ggplot2::theme(
      
      plot.title.position = "plot",
      
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(
        size = 9,
        face = "italic",
        color = "grey40",
        hjust = 0,
        margin = ggplot2::margin(10, 0, 0, 0)
      ),
      
      legend.position =      "top",
      legend.justification = "left",
      legend.direction =     "horizontal",
      
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      
      complete = TRUE
    )
}


# Automate the creation of column plots with optional error bars, and various
# y-value formatsting

fc_col_plot <- function(.data, 
                        x, y, 
                        ymin = NULL, ymax = NULL,
                        y_limits = NULL,
                        y_format = c("si", "dollar", "percent"),
                        accuracy = 1) {
  
  y_format <- match.arg(y_format)
  
  if (y_format == "si") {
    format_y_numbers <- scales::label_number_si(accuracy)
  } else if (y_format == "dollar") {
    format_y_numbers <- scales::dollar_format(accuracy)
  } else if (y_format == "percent") {
    format_y_numbers <- scales::percent_format(accuracy)
  }
  
  p <- ggplot2::ggplot(.data) +
    ggplot2::aes(x = {{x}}, y = {{y}}, ymin = {{ymin}}, ymax = {{ymax}}) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_text(
      aes(y = {{ymax}}, label = format_y_numbers({{y}})), 
      vjust = -0.5, size = 3.5
    ) +
    ggplot2::scale_y_continuous(limits = y_limits, labels = format_y_numbers) +
    theme_fc()
  
  if (!missing(ymin) && !missing(ymax)) {
    p <- p + ggplot2::geom_errorbar()
  }
  
  p
}

fc_col_plot_cluster <- function(.data, 
                                x, y, fill,
                                ymin = NULL, ymax = NULL,
                                y_limits = NULL,
                                y_format = c("si", "dollar", "percent"),
                                accuracy = 1) {
  
  y_format <- match.arg(y_format)
  
  if (y_format == "si") {
    format_y_numbers <- scales::label_number_si(accuracy)
  } else if (y_format == "dollar") {
    format_y_numbers <- scales::dollar_format(accuracy)
  } else if (y_format == "percent") {
    format_y_numbers <- scales::percent_format(accuracy)
  }
  
  p <- ggplot2::ggplot(.data) +
    ggplot2::aes(x = {{x}}, y = {{y}}, ymin = {{ymin}}, ymax = {{ymax}}, fill = {{fill}}) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_text( #TODO: this will break if ymin/ymax are supplied - should use either y or ymin
      aes(y = {{ymax}}, label = format_y_numbers({{y}})), 
      vjust = -0.5, size = 3.5,  position = position_dodge(width  = 1)
    ) +
    ggplot2::scale_y_continuous(limits = y_limits, labels = format_y_numbers) +
    theme_fc()
  
  if (!missing(ymin) && !missing(ymax)) {
    p <- p + ggplot2::geom_errorbar(position = position_dodge())
  }
  
  p
}
