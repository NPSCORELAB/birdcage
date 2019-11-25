build_leaflet <- function(tweet_sf) {
  line_weight <- 5
  # popup_col <- ~ text
  label_col <- ~ text
  
  highlighter <- highlightOptions(color = "red")
  
  tweet_sf <- tweet_sf[order(st_area(tweet_sf$geometry), decreasing = TRUE), ]
  
  # ist_sf <- tweet_sf[tweet_sf$which_geom == "ist_complex_value", ]
  main_sf <- tweet_sf[tweet_sf$which_geom == "bbox_coords", ]
  retweet_sf <- tweet_sf[tweet_sf$which_geom == "retweet_bbox_coords", ]
  quoted_sf <- tweet_sf[tweet_sf$which_geom == "quoted_bbox_coords", ]
  
  build_popup <- function(sf) {
    leafpop::popupTable(as.data.table(sf), c("screen_name", "text", "description", "location"),
                        row.numbers = FALSE, feature.id = FALSE)
  }
  
  leaflet() %>% 
    addTiles(group = "OSM (default)") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "Optical Imagery") %>% 
    # addPolylines(data = ist_sf,
    #              popup = build_popup(ist_sf),
    #              label = label_col,
    #              fillOpacity = 0, 
    #              weight = line_weight,
    #              color = "darkblue",
    #              highlightOptions = highlighter,
    #              group = "IST") %>% 
    addPolylines(data = main_sf,
                popup = build_popup(main_sf),
                label = label_col,
                fillOpacity = 0, 
                weight = line_weight,
                color = "blue",
                highlightOptions = highlighter,
                group = "Main") %>% 
    addPolylines(data = retweet_sf,
                popup = build_popup(retweet_sf),
                label = label_col,
                fillOpacity = 0, 
                weight = line_weight,
                color = "yellow",
                highlightOptions = highlighter,
                group = "Retweet") %>% 
    addPolylines(data = quoted_sf,
                popup = build_popup(quoted_sf),
                label = label_col,
                fillOpacity = 0, 
                weight = line_weight,
                color = "teal",
                highlightOptions = highlighter,
                group = "Quoted") %>% 
    addLayersControl(
      baseGroups = c("OSM", "Optical Imagery"),
      overlayGroups = c("Main", "Retweet", "Quoted", "IST"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    addResetMapButton() %>% 
    addSearchFeatures(
      targetGroups = c("Main", "Retweet", "Quoted", "IST"),
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
    ) %>% 
    hideGroup(c("Retweet", "Quoted", "IST"))
}


