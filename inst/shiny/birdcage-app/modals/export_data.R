modal_export_data <- function(.failed = FALSE, ...) {
  print("modal_export_data() called")
  
  showModal(
    modalDialog(
      selectInput("export_type", "Export Type", 
                  choices = c("Spreadsheets", "Networks", "Geospatial"))
      
      ,
      shinyjs::useShinyjs()
      ,
      verbatimTextOutput("export_data_message")
      ,
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css")
      ,
      tags$head(
        tags$style(HTML("#text {font-family: 'Fira Code Retina', monospace; color: black;}"))
      )
      ,
      size = "m"
    )
  )
}

