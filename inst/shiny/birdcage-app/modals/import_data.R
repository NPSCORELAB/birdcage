modal_import_data <- function(.failed = FALSE, ...) {
  print("modal_import_data() called")
  
  showModal(
    modalDialog(
      box(
        width = 12,
        column(
          width = 6,
          fileInput(inputId = "file1",
                    label = "Upload File",
                    multiple = FALSE,
                    accept = c(".json", ".jsonl", ".gz", ".zip"))
        )
        ,
        column(
          width = 6,
          shiny::actionButton("use_example_data", "Use Example Data")
        )
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
        tags$style(HTML("#text {font-family: 'Fira Code Retina', monospace; color: black;}"))
      )
      ,
      size = "m"
    )
  )
}