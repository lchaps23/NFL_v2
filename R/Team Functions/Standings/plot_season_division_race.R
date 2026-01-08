plot_season_division_race <- function(df) {
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = week,
      y = GB,
      group = team_abbr,
      color = team_color
    )
  ) +
    
    ggplot2::geom_line(
      ggplot2::aes(color = team_color),
      linewidth = 1.5
      ) +
    
    ggplot2::geom_point(
      ggplot2::aes(fill = team_color2),
      shape = 21,
      size  = 4,
      stroke = 1
      ) +
    
    ggplot2::scale_color_identity() +
    
    ggplot2::scale_fill_identity() +
    
    ggplot2::scale_y_reverse(
      breaks = seq(0, 17, by =1)
    ) +
    
    ggplot2::facet_wrap(
      ~ division,
      ncol = 2
    ) +
    
    ggplot2::labs(
      title    = "Divison Races",
      subtitle = "Games Back by Week",
      x        = "Week",
      y        = "Games Back"
    ) +
    
    ggplot2::theme_minimal(base_size = 12) +
    
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold"),
      plot.title       = ggplot2::element_text(face = "bold")
    )
}