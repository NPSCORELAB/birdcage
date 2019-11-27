build_leaflet_popup <- function(sf) {
  leafpop::popupTable(as.data.table(sf), c("screen_name", "text", "description", "location"),
             row.numbers = FALSE, feature.id = FALSE)
}

add_leaflet_layer <- function(leaf, sf, which_geom, label_col = "text", line_weight = 5) {
  color <- switch (which_geom,
    "ist_complex_value"   = "darkblue",
    "bbox_coords"         = "blue",
    "retweet_bbox_coords" = "yellow",
    "quoted_bbox_coords"  = "teal",
    
    stop("unknown sf geometry column.", call. = FALSE)
  )
  leaf %>% 
    addPolylines(
      data = sf,
      popup = build_leaflet_popup(sf),
      label = sf[[label_col]],
      fillOpacity = 0,
      weight = line_weight, 
      color = color,
      highlightOptions = highlightOptions(color = "red"),
      group = which_geom
    )
}
  
build_leaflet <- function(tweet_sf) {
  out <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Default") %>%
    addProviderTiles(providers$Wikimedia, group = "Wikimedia") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Optical Imagery")
  
  split_geoms <- split.data.frame(tweet_sf, f = tweet_sf$which_geom)
  for (i in seq_along(split_geoms)) {
    out <- add_leaflet_layer(out, split_geoms[[i]], 
                             which_geom = names(split_geoms)[[i]])
  }
    
  out %>% 
    addLayersControl(
      baseGroups = c("Default", "Wikimedia", "Optical Imagery"),
      overlayGroups = names(split_geoms),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    addResetMapButton() %>% 
    addSearchFeatures(
      targetGroups = names(split_geoms),
      options = searchFeaturesOptions(zoom = 2, openPopup = TRUE)
    ) %>% 
    addDrawToolbar(
      singleFeature = TRUE,
      polygonOptions = drawPolygonOptions(
        showArea = TRUE, metric = TRUE, 
        shapeOptions = drawShapeOptions(color = "black", fillColor = "red", fillOpacity = 0.25)
      ),
      polylineOptions = FALSE,
      rectangleOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      circleOptions = FALSE,
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
    )
}


