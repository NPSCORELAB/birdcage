modal_import_data <- function(.failed = FALSE, ...) {
  print("modal_import_data() called")
  
  showModal(
    modalDialog(
      box(
        width = 12,
        HTML("<center>"),
        shiny::actionButton("use_example_data", HTML("<strong>Use Example Data</strong>"))
        ,
        br()
        ,
        h3(HTML("<strong><i>or...</i></strong>"))
        ,
        br()
        ,
        fileInput(inputId = "file1",
                  label = "Upload a File",
                  multiple = FALSE,
                  accept = c(".json", ".jsonl", ".gz", ".zip", ".rds"))
      )
      ,
      shinyjs::useShinyjs()
      ,
      verbatimTextOutput("import_data_message")
      ,
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css")
      ,
      tags$head(
        tags$style(HTML("#import_data_message {font-family: 'Fira Code Retina', monospace; color: green;}"))
      )
      ,
      size = "m"
    )
  )
}
