modal_export_data <- function(.failed = FALSE, ...) {
  print("modal_export_data() called")
  
  showModal(
    modalDialog(
      box(
        width = 12,
        HTML("<center>")
        ,
        downloadButton(outputId = "export_xlsx", label = "Export to .xlsx")
        ,
        downloadButton(outputId = "export_csv", label = "Export to .csv")
        ,
        downloadButton(outputId = "export_graphml", label = "Export to  .graphml")
        ,
        HTML("</center>")
      )
      ,
      shinyjs::useShinyjs()
      ,
      verbatimTextOutput("export_data_message")
      # ,
      # tags$link(rel = "stylesheet",
      #           type = "text/css",
      #           href = "https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css")
      ,
      tags$head(
        tags$style(HTML("#export_data_message {font-family: 'Fira Code Retina', monospace; color: red;}"))
      )
      ,
      size = "m"
    )
  )

}


format_file_name <- function(imported_file_name, file_extension) {
  paste0("birdcage-",
         format(Sys.time(), "%Y%m%d-%H%M-"), 
         tools::file_path_sans_ext(basename(imported_file_name)),
         file_extension)
}
