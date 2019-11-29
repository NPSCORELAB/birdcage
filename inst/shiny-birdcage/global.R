suppressPackageStartupMessages({
library(shiny)
library(shinydashboard)
library(tweetio)
library(ggplot2)
library(plotly)
library(purrr)
library(scales)
library(stringi)
library(igraph)
library(openxlsx)
library(data.table)
library(sf)
library(leaflet)
library(geojsonsf)
library(leaflet.extras)
library(shinyWidgets)
library(visNetwork)
  
library(future)
plan(multisession)
# plan(multiprocess)

library(promises)
library(future.apply)

})


source("modals/import_data.R")
source("modals/export_data.R")
source("summary.R")
source("knowledge-graph.R")
source("map.R")

build_DT <- function(tweet_df, list_col_handler = flatten_df_cols,
                     scroll_y = "800px") {
  if (is.data.table(tweet_df)) {
    init <- tweet_df
  } else {
    init <- as.data.table(tweet_df)
  }
  
  vis_cols <- intersect(
    names(tweet_df),
    c("created_at", "text", "lang",
      "screen_name","hashtags")
  )
  
  if (!is.null(list_col_handler)) {
    init <- list_col_handler(init)
  } 
  
  if ("lang" %chin% names(init)) {
    init[, lang := as.factor(lang)]
  }
  
  invis_cols <- setdiff(names(init), vis_cols)
  
  dttm_cols <- which(map_lgl(init, inherits, "POSIXct"))
  
  js_index_invis_cols <- which(!names(init) %in% vis_cols) - 1
  js_index_all_cols <- seq_len(ncol(init)) - 1
  
  user_cols <- tweetio:::user_col_names(tweet_df)$main
  user_col_idx <- which(names(tweet_df) %chin% user_cols) - 1
  non_user_col_idx <- which(!names(tweet_df) %chin% user_cols) - 1
  
  status_cols <- tweetio:::status_col_names(tweet_df)$main
  status_col_idx <- which(names(tweet_df) %chin% status_cols) - 1
  non_status_col_idx <- which(!names(tweet_df) %chin% status_cols) - 1

  init %>% 
    DT::datatable(
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      extensions = c("Buttons", "ColReorder", "FixedHeader", "KeyTable"),
      options = list(
        dom = 'Bfrtip',
        searchDelay = 600,
        # autoWidth = TRUE,
        columnDefs = list(
          list(targets = js_index_invis_cols, visible = FALSE)
        ),
        colReorder = TRUE,
        # fixedColumns = TRUE,
        keys = TRUE,
        scrollX = TRUE,
        scrollY = scroll_y,
        searchHighlight = TRUE,
        buttons = list(
          list(extend = "colvis",
               columns = js_index_all_cols,
               collectionLayout = "four-column"),
          list(extend = "colvisGroup",
               text = "Default Focus",
               show = vis_cols,
               hode = invis_cols),
          list(extend = "colvisGroup",
               text = "User Focus",
               show = user_col_idx,
               hide = non_user_col_idx),
          list(extend = "colvisGroup",
               text = "Status Focus",
               show = status_col_idx,
               hide = non_status_col_idx)
        )
      )
    ) %>%
  DT::formatDate(columns = dttm_cols, method = "toUTCString")
}


w_spin <- function(x) {
  shinycssloaders::withSpinner(x)
}


build_DT2 <- function(df, scroll_y = "600px") {
  dttm_cols <- which(map_lgl(df, inherits, "POSIXct"))
  
  df %>% 
    DT::datatable(
      rownames = FALSE,
      escape = FALSE, 
      selection = "single",
      # selection = list(mode = "single", 1),
      filter = "top",
      extensions = c("ColReorder", "FixedHeader", "KeyTable"),
      options = list(
        dom = 'frtip',
        searchDelay = 600,
        # autoWidth = TRUE,
        colReorder = TRUE,
        # fixedColumns = TRUE,
        keys = TRUE,
        scrollX = TRUE,
        scrollY = scroll_y,
        searchHighlight = TRUE
      )
    ) %>%
    DT::formatDate(columns = dttm_cols, method = "toUTCString")
}



flatten_df_cols <- function(df, copy = TRUE) {
  list_cols <- tweetio:::.match_col_names(df, is.list)
  
  if (!length(list_cols)) {
    return(df)
  }
  
  if (copy) {
    df <- copy(df)
  }
  
  df[, (list_cols) := lapply(.SD, lapply, unlist, recursive = FALSE),
     .SDcols = list_cols]
}


jsonify_list_cols <- function(df, copy = TRUE) {
  list_cols <- tweetio:::.match_col_names(df, is.list)
  
  if (!length(list_cols)) {
    return(df)
  }
  
  if (copy) {
    df <- copy(df)
  }
  
  df[, (list_cols) := lapply(.SD, map_chr, function(.x) {
    .x[is.na(.x)] <- ""
    .x <- jsonify::to_json(.x, unbox = TRUE)
    if (nchar(.x) > 0) .x else NA_character_
  }),
  .SDcols = list_cols
  ]
}


search_df_by_string <- function(df, string) {
  chr_cols <- map_lgl(df, is.character)
  
  df[
    df[, Reduce(`|`, future_lapply(.SD, stri_detect_regex, string)), 
       .SDcols = chr_cols]
    ]
}

