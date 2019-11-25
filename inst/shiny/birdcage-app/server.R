server <- shinyServer(function(input, output) {
  options(shiny.maxRequestSize = 2000 * 1024^2)

  modal_import_data()
  
  observeEvent(input$import_data, {
    modal_import_data()
  })
  
  FILES <- reactiveValues()
  
  observeEvent(input$file1$datapath, {
    FILES$file_to_import <- input$file1$datapath
  })
  
  observeEvent(input$use_example_data, {
    FILES$file_to_import <- system.file("example-data/api-stream.json.gz", 
                                        package = "tweetio")
  })
  
  
  TWEET_DF <- eventReactive(FILES$file_to_import, {
    on.exit({removeModal()})
    req(FILES$file_to_import)
    
    withCallingHandlers({
      shinyjs::html("import_data_message", "")
      
      message("Parsing Tweets...")
      init <- FILES$file_to_import %>%
        tweetio::read_tweets()

      message("Almost Done...")

      future(init)
    },
    message = function(m) shinyjs::html(id = "import_data_message", html = m$message, add = TRUE)
    )}
  )
  
  observeEvent(input$export_data, {
    modal_export_data()
  })
  
  

  # USER_DF <- reactive({
  #   TWEET_DF()  %...>%
  #     tweetio:::build_user_df(.)
  # })

  STATUS_DF <- reactive({
    TWEET_DF()  %...>%
      tweetio:::build_status_df()
  })
  
  DATE_RANGE <- reactive({
    TWEET_DF() %...>% 
      `[[`("created_at") %...>% 
      range()
  })
  
  PROTO_KNOWLEDGE_GRAPH <- reactive({
    TWEET_DF()  %...>%
      build_proto_kg() 
      
  })
  
  KG_NODES <- reactive({
    PROTO_KNOWLEDGE_GRAPH() %...>%
      `[[`("nodes")
    
  })
  
  KNOWLEDGE_GRAPH <- reactive({
    PROTO_KNOWLEDGE_GRAPH() %...>% 
      as_kg_igraph() %...>%
      set_community()
  })
  
  COMMUNITIES <- reactive({
    KNOWLEDGE_GRAPH() %...>% 
      igraph::vertex_attr("community")
  })
  
  TWEET_SF <- reactive({
    TWEET_DF() %...>%
      as_tweet_sf(geom_col = "all")
  })

  
  
# summary ================================================================================ 
  output$n_users <- renderValueBox({
    KG_NODES() %...>%
      build_value_box(.node_class = "user",
                      subtitle = "Users",
                      icon = "user",
                      color = "teal")

  })

  output$n_statuses <- renderValueBox({
    KG_NODES() %...>%
      build_value_box(.node_class = "status",
                      subtitle = "Tweets",
                      icon = "twitter",
                      color = "light-blue")
  })

  output$n_hashtags <- renderValueBox({
    KG_NODES() %...>%
      build_value_box(.node_class = "hashtag",
                      subtitle = "Hashtags",
                      icon = "hashtag",
                      color = "maroon")
  })

  output$n_urls <- renderValueBox({
    KG_NODES() %...>%
      build_value_box(.node_class = "url",
                      subtitle = "URLs",
                      icon = "link",
                      color = "green")
  })

  output$n_media <- renderValueBox({
    KG_NODES() %...>%
      build_value_box(.node_class = "media",
                      subtitle = "Media",
                      icon = "photo",
                      color = "orange")
  })

  output$n_communities <- renderValueBox({
    COMMUNITIES() %...>%
      max() %...>% 
      build_value_box2(subtitle = "Topics/Communities",
                       icon = "users",
                       color = "red")
  })
  
  output$pl_timeline <- renderPlotly({
    promise_all(
      status_df = STATUS_DF(),
      date_range = DATE_RANGE()
    ) %...>% 
      with({
        timeline_gg(status_df, earliest = date_range[[1L]], latest = date_range[[2L]],
                    bin_by = paste(input$timeline_bin_size, input$timeline_bin_unit),
                    merge = input$timeline_merge,
                    sync_y_axes = input$timeline_sync_y_axes)
      })  %...>%
      ggplotly()
  })
  
# map ====================================================================================
  output$map_DT <- DT::renderDT({
    promise_all(
      tweet_sf = TWEET_SF(),
      draw_sf = DRAW_SF()
    ) %...>%
      with({
        st_join(tweet_sf, draw_sf, join = st_within, left = FALSE)
      }) %...>% 
      as.data.table() %...>% 
      build_DT(scroll_y = "600px")
  })
  
  output$leaf <- renderLeaflet({
    TWEET_SF() %...>% 
      build_leaflet()
  })
  
  DRAW_SF <- eventReactive(input$leaf_draw_new_feature, {
    coords <- t(
      matrix(
        unlist(input$leaf_draw_new_feature$geometry$coordinates, use.names = FALSE), 
        nrow = 2
      )
    )
    
    future(
      st_cast(
        st_sf(geometry = st_sfc(st_linestring(coords), crs = 4326L)),
        "POLYGON"
      )
    )
  })
  
  
# knowledge_graph ========================================================================
  
  
# topics_communities =====================================================================

  
# explore ================================================================================ 
  output$tweet_DT <- DT::renderDT({
    on.exit(print("d"))
    
    TWEET_DF()  %...>%
      build_DT()
    
  })
  

})


