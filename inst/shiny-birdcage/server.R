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
    on.exit(message("\nAll set!"))
    
    req(FILES$file_to_import)
    
    withCallingHandlers({
      shinyjs::html("import_data_message", "")
      
      if (grepl("\\.rds$", FILES$file_to_import, ignore.case = TRUE)) {
        init <- FILES$file_to_import %>% readRDS()
        setDT(init)

      } else {
        init <- FILES$file_to_import %>%
          tweetio::read_tweets(verbose = TRUE)
      }

      message("\nAlmost Done...")

      future(init)
    },
    message = function(m) {
      shinyjs::html(id = "import_data_message", html = m$message, add = TRUE)
    },
    error = function(e) {
      shinyjs::html(id = "import_data_message", html = e$message, add = TRUE)
    }
    )}
  )
  
  observeEvent(input$export_data, {
    modal_export_data()
  })
  
  output$export_xlsx <- downloadHandler(
        filename = function() {
          format_file_name(imported_file_name = FILES$file_to_import,
                           file_extension = ".xlsx")
        }
        ,
        content = function(file) {

          withCallingHandlers({
            shinyjs::html("export_data_message", "")

            message("Preprocessing file...")
            Sys.sleep(1)
            message("Writing .xlsx file...")

            TWEET_DF() %...>% 
              write_tweet_xlsx(tweet_df = ., file_path = file, verbose = TRUE)

          },
          message = function(m) {
            shinyjs::html(id = "export_data_message", html = m$message, add = TRUE)
          })

        }
      )

  output$export_csv <- downloadHandler(
    filename = function() {
      format_file_name(imported_file_name = FILES$file_to_import,
                       file_extension = ".csv")
    }
    ,
    content = function(file) {
      
      withCallingHandlers({
        shinyjs::html("export_data_message", "")
        
        message("Preprocessing file...")
        Sys.sleep(1)
        message("Writing .csv file...")
        
        TWEET_DF() %...>% 
          write_tweet_csv(tweet_df = ., file_path = file, verbose = TRUE)
        
      },
      message = function(m) {
        shinyjs::html(id = "export_data_message", html = m$message, add = TRUE)
      })
      
    }
  )

  output$export_graphml <- downloadHandler(
    filename = function() {
      format_file_name(imported_file_name = FILES$file_to_import,
                       file_extension = ".graphml")
    }
    ,
    content = function(file) {
      withCallingHandlers({
        shinyjs::html("export_data_message", "")
        
        message("Building Social Network...")
        
        out <- TWEET_DF() %...>%
          as_igraph(all_user_data = TRUE, all_status_data = TRUE)
        
        message("Writing .graphml file...")
        
        out %...>% 
          tweetio:::write_graphml(file_path = file)
        
      },
      message = function(m) {
        shinyjs::html(id = "export_data_message", html = m$message, add = TRUE)
      })
      
    }
  )
  

  USER_DF <- reactive({
    TWEET_DF() %...>%
      tweetio:::extract_users() %...>% 
      jsonify_list_cols()
  })

  STATUS_DF <- reactive({
    TWEET_DF() %...>%
      tweetio:::extract_statuses() %...>% 
      jsonify_list_cols()
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
      set_community() %...>%
      set_graph_appearance()
  })
  
  ENTITY_DF <- reactive({
    KNOWLEDGE_GRAPH() %...>%
      entity_df_from_kg()
  })
  
  # COMMUNITIES <- reactive({
  #   KNOWLEDGE_GRAPH() %...>% 
  #     igraph::vertex_attr("community")
  # })
  
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
                      subtitle = "Statuses",
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
    KNOWLEDGE_GRAPH() %...>%
      vertex_attr("community") %...>%
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
        timeline_gg(status_df, 
                    earliest = if (input$timeline_expand) -Inf else date_range[[1L]],
                    latest = if (input$timeline_expand) Inf else date_range[[2L]],
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
  # DF_WITH_NAME <- reactive({
  #   if (input$kg_users_found_rows_selected) {
  #     "user"
  #   } else if (input$kg_statuses_found_rows_selected) {
  #     "status"
  #   } else if (input$kg_entities_found_rows_selected) {
  #     "entity"
  #   } else {
  #     NULL
  #   }
  # })
  
  TARGET_EGO_INFO <- reactiveValues(
    # node_df = NULL,
    # row_selected = NULL,
    # name_col = NULL
  )
  
  observeEvent(input$kg_users_found_rows_selected, {
    TARGET_EGO_INFO$name_col <- "user_id"
    TARGET_EGO_INFO$row_selected <- input$kg_users_found_rows_selected
  })
  
  observeEvent(input$kg_statuses_found_rows_selected, {
    TARGET_EGO_INFO$name_col <- "status_id"
    TARGET_EGO_INFO$row_selected <- input$kg_statuses_found_rows_selected
  })
  
  observeEvent(input$kg_entities_found_rows_selected, {
    TARGET_EGO_INFO$name_col <- "name"
    TARGET_EGO_INFO$row_selected <- input$kg_entities_found_rows_selected
  })
  
  
  output$kg_users_found <- DT::renderDT({
    req(input$search_kg)
    
    TARGET_EGO_INFO$node_df <- USER_DF() %...>% 
      search_df_by_string(input$search_kg) 
    
    TARGET_EGO_INFO$node_df %...>% 
      build_DT2()

  })
  
  output$kg_statuses_found <- DT::renderDT({
    req(input$search_kg)

    TARGET_EGO_INFO$node_df <- STATUS_DF() %...>% 
      search_df_by_string(input$search_kg) 
    
    TARGET_EGO_INFO$node_df %...>% 
      build_DT2()

  })
  
  output$kg_entities_found <- DT::renderDT({
    req(input$search_kg)
    
    TARGET_EGO_INFO$node_df <- ENTITY_DF() %...>%
      `[`(stringi::stri_detect_fixed(name, input$search_kg))
    
    TARGET_EGO_INFO$node_df %...>%
      build_DT2()
  })
  
  VIS_NET <- reactive({
    req(TARGET_EGO_INFO$name_col, TARGET_EGO_INFO$node_df, TARGET_EGO_INFO$row_selected  )
    
    promise_all(
      g = KNOWLEDGE_GRAPH(),
      target_node_df = TARGET_EGO_INFO$node_df
    ) %...>% 
      with({
        build_vis_net(
          g = g, 
          df_with_name = target_node_df,
          row_selected = TARGET_EGO_INFO$row_selected,
          name_col = TARGET_EGO_INFO$name_col
        )
      })
  })
  

  
  output$vis_net <- renderVisNetwork({
    VIS_NET()
  })
  
  VIS_NET_NODES <- reactive({
    VIS_NET() %...>%
      `[[`("x") %...>% 
      `[[`("nodes") %...>% 
      as.data.table()
    
    # out %...T>%
    #   print()
    # out
  })
  
  VIS_USER_NODES <- reactive({
    # req("node_class" %chin% names(VIS_NET_NODES()))
    
    # VIS_NET_NODES() %...T>%
    #   print()

    promise_all(
      nodes = VIS_NET_NODES(),
      df = USER_DF()
    ) %...>%
      with({
        target_rows <- df$user_id %chin% nodes$id[nodes$node_class == "user"] 
        df[target_rows]
      })
  })
  
  VIS_STATUS_NODES <- reactive({
    promise_all(
      nodes = VIS_NET_NODES(),
      df = STATUS_DF()
    ) %...>%
      with({
        target_rows <- df$status_id %chin% nodes$id[nodes$node_class == "status"] 
        df[target_rows]
      })
  })
  
  VIS_ENTITY_NODES <- reactive({
    promise_all(
      nodes = VIS_NET_NODES(),
      df = ENTITY_DF()
    ) %...>%
      with({
        target_rows <- df$name %chin% nodes$id[nodes$node_class  %chin% c("hashtag", "url", "media")] 
        df[target_rows]
      })
  })
  
  
  output$vis_user_nodes <- DT::renderDT({
    VIS_USER_NODES() %...>%
      build_DT2()
  })
  
  
  output$vis_status_nodes <- DT::renderDT({
    VIS_STATUS_NODES() %...>%
      build_DT2()
  })
  
  
  output$vis_entity_nodes <- DT::renderDT({
    VIS_ENTITY_NODES() %...>%
      build_DT2()
  })

  
  observeEvent(input$vis_user_nodes_rows_selected, {
    nodes_to_select <- promise_all(
      nodes = VIS_NET_NODES(),
      df = VIS_USER_NODES()
    ) %...>%
      with({
        nodes$id[nodes$id == df[input$vis_user_nodes_rows_selected, user_id]]
      }) %...>%
      vis_select_nodes(visNetworkProxy("vis_net"))
  })
  
  observeEvent(input$vis_status_nodes_rows_selected, {
    nodes_to_select <- promise_all(
      nodes = VIS_NET_NODES(),
      df = VIS_STATUS_NODES()
    ) %...>%
      with({
        nodes$id[nodes$id == df[input$vis_status_nodes_rows_selected, status_id]]
      }) %...>%
      vis_select_nodes(visNetworkProxy("vis_net"))
  })
  
  observeEvent(input$vis_entity_nodes_rows_selected, {
    nodes_to_select <- promise_all(
      nodes = VIS_NET_NODES(),
      df = VIS_ENTITY_NODES()
    ) %...>%
      with({
        nodes$id[nodes$id == df[input$vis_entity_nodes_rows_selected, name]]
      }) %...>%
      vis_select_nodes(visNetworkProxy("vis_net"))
  })


# topics_communities =====================================================================

  
# explore ================================================================================ 
  output$tweet_DT <- DT::renderDT({
    on.exit(print("d"))
    
    TWEET_DF()  %...>%
      build_DT()
    
  })
  

})


