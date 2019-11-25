build_value_box <- function(kg_nodes, .node_class, subtitle, icon, color) {
  valueBox(
    value = comma(nrow(kg_nodes[node_class == .node_class, ])),
    subtitle = subtitle,
    icon = icon(icon),
    color = color
  )
}


build_value_box2 <- function(n, subtitle, icon, color) {
  valueBox(
    value = comma(n),
    subtitle = subtitle,
    icon = icon(icon),
    color = color
  )
}



timeline_gg <- function(status_df, earliest, latest, bin_by, merge, sync_y_axes) {
  init <- status_df[between(timestamp_ms, earliest, latest),
                    .(timestamp_ms = lubridate::floor_date(timestamp_ms, bin_by), 
                      status_type)
                    ][, .N, by = c("status_type", "timestamp_ms")
                      ][order(N, decreasing = TRUE)
                        ] %>% 
    ggplot(aes(x = timestamp_ms, y = N)) +
    geom_line(aes(color = status_type), show.legend = merge) +
    labs(x = NULL, y = NULL, "Activity by Hour") +
    scale_color_viridis_d() +
    theme_minimal(base_size = 12L, base_family = "serif") +
    guides(color = guide_legend(title = "Status Type"))
  
  if (merge) {
    return(init)
  }
  
  init +
    theme(legend.position = "none") +  # needed for `ggplotly()`, except plotly always ignores position...
    facet_wrap(~ status_type, ncol = 1L, 
               scales = if (sync_y_axes) "fixed" else "free_y")
}
