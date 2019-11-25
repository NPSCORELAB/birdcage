# header =================================================================================
header <- dashboardHeader(
  title = HTML("{birdcage}")
)


# sidebar ================================================================================
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    id = "tabs"
    ,
    actionButton("import_data", "Import Data")
    ,
    actionButton("export_data", "Export Data")
    ,
    # menuItem("Home", tabName = "home", icon = icon("home"),
    # selected = TRUE)
    # ,
    menuItem("Summary", tabName = "summary", icon = icon("chart-line"),
             selected = TRUE)
    ,
    menuItem("Map", tabName = "map", icon = icon("globe") )
    ,
    menuItem("Knowledge Graph", tabName = "graph", icon = icon("bezier-curve") )
    ,
    menuItem("Communities and Topics", tabName = "communities", icon = icon("users"))
    ,
    menuItem("Explore Data", tabName = "explore", icon = icon("twitter"))
    ,
    menuItem("About", tabName = "about", icon = icon("info"))
  )
)

# tabs ===================================================================================

#* summary ===============================================================================
tab_summary <-     tabItem(
  tabName = "summary",
  fluidRow(
    box(width = 12, collapsible = TRUE,
        valueBoxOutput("n_users", width = 2) %>% w_spin(),
        valueBoxOutput("n_statuses", width = 2) %>% w_spin(),
        valueBoxOutput("n_hashtags", width = 2) %>% w_spin(),
        valueBoxOutput("n_urls", width = 2) %>% w_spin(),
        valueBoxOutput("n_media", width = 2) %>% w_spin(),
        valueBoxOutput("n_communities", width = 2) %>% w_spin()
    )
  )
  ,
  fluidRow(
    # box(
    box(
      width = 12, collapsible = TRUE,
      column(
        numericInput("timeline_bin_size", label = "Bin Size", value = 1,
                     min = 1)
        ,
        width = 5
      )
      ,
      column(
        selectInput("timeline_bin_unit", label = "Bin Unit",
                    choices = c("second", "minute", "hour", "day", "week", "month",
                                "year"),
                    selected = "minute")
        ,
        width = 5
      )
      ,
      column(
        checkboxInput("timeline_merge", "Merge Plots?", value = FALSE),
        checkboxInput("timeline_sync_y_axes", "Sync Y Axes?", value = FALSE)
       ,
       width = 2
      )
      ,
      box(width = 12,
          plotly::plotlyOutput("pl_timeline", height = "600px") %>% w_spin()
      )
    )
  )
)

#* map ===================================================================================
tab_map <- tabItem(
  tabName = "map",
  fluidRow(
    box(
      width = 12, collapsible = TRUE,
      leafletOutput("leaf", height = "600px") %>% w_spin()
    )
    ,
    box(
      width = 12, collapsed = TRUE,
      DT::dataTableOutput("map_DT", height = "600px") %>% w_spin()
      
    )
  )
)

#* explore ===============================================================================
tab_explore <- tabItem(
  tabName = "explore",
  fluidRow(
    box(
      width = 12,
      DT::dataTableOutput("tweet_DT", height = "800px") %>% w_spin()
    )
  )
)
    
# body ===================================================================================
body <- dashboardBody(
  tabItems(
    tab_summary,
    tab_map,
    tab_explore
  )
)

# dashboardPage ==========================================================================
dashboardPage(
  header,
  sidebar,
  body
)