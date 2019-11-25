status_entity_edges <- function(tweet_df, entity, target_class) {
  setDT(
    tweetio:::unnest_entities_impl(
      tracker = tweet_df[["created_at"]],
      source = tweet_df[["status_id"]],
      target = tweet_df[[entity]],
      col_names = c("from", "to", "created_at")
    )
  )[, c("source_class", "target_class", "action") := list("status", target_class, "contains")
    ][, to := stri_trans_tolower(to)]
}

user_status_edges <- function(tweet_df, user_col, action) {
  if (user_col == "user_id") {
    target_cols <- c("user_id", "status_id", "created_at")
  } else if (user_col == "reply_to") {
    target_cols <- c("reply_to_user_id", "reply_to_status_id")
  } else {
    target_cols <- paste0(user_col, c("_user_id", "_status_id", "_created_at"))
  }
  stopifnot(all(target_cols %chin% names(tweet_df)))
  
  out <- na.omit(tweet_df[, ..target_cols], 
                 cols = target_cols[1L:2L]
                 )[, c("source_class", "target_class", "action") := list(
                   "user", "status", "posts")
                   ]
  
  setnames(
    out, old = target_cols, 
    new = c("from", "to", "created_at")[seq_along(target_cols)]
  )[]
}


user_user_edges <- function(tweet_df) {
  relations <- c("mention" ,"retweet","reply_to", "quoted")
  targets <- set_names(
    intersect(paste0(relations, "_user_id"), names(tweet_df))
  )
  
  edge_by_status_type <- lapply(targets, function(x) {
    edge_cols <- c("user_id", x, "created_at")
    
    res <- tweet_df[!is.na(get(x)), edge_cols, with = FALSE]
    
    setnames(res, c("from", "to", "created_at"))
    
    if (is.list(res[["to"]])) {
      res <- setDT(
        unnest_entities_impl(tracker = res[["created_at"]],
                             source = res[["from"]],
                             target = res[["to"]],
                             col_names = c("from", "to", "created_at"))
      )
    }
    
    res[, action := sub("_user_id$", "", x)]
  })
  
  rbindlist(edge_by_status_type, use.names = TRUE
            )[, c("source_class", "target_class") := list("user", "user")
              ]
}


status_status_edges <- function(tweet_df) {
  relations <- c("retweet","reply_to", "quoted")
  targets <- set_names(
    intersect(paste0(relations, "_status_id"), names(tweet_df))
  )
  
  edge_by_status_type <- lapply(targets, function(x) {
    edge_cols <- c("status_id", x, "created_at")
    
    res <- tweet_df[!is.na(get(x)), edge_cols, with = FALSE]
    
    setnames(res, c("from", "to", "created_at"))
    
    if (is.list(res[["to"]])) {
      res <- setDT(
        unnest_entities_impl(tracker = res[["created_at"]],
                             source = res[["from"]],
                             target = res[["to"]],
                             col_names = c("from", "to", "created_at"))
      )
    }
    
    res[, action := sub("_status_id$", "", x)]
  })
  
  rbindlist(edge_by_status_type, use.names = TRUE
            )[, c("source_class", "target_class") := list("status", "status")
              ]
}


build_proto_kg <- function(tweet_df) {
  edges <- rbindlist(
    list(
      # status-to-status: retweet/reply_to/quoted
      status_status_edges(tweet_df),
      # user-to-user: retweet/reply_to/quoted
      user_user_edges(tweet_df),
      # user posts status
      user_status_edges(tweet_df, "user_id"),
      user_status_edges(tweet_df, "retweet"),
      user_status_edges(tweet_df, "reply_to"),
      user_status_edges(tweet_df, "quoted"),
      # status contains entity
      status_entity_edges(tweet_df, entity = "hashtags", "hashtag"),
      status_entity_edges(tweet_df, entity = "mentions_user_id", "user"),
      status_entity_edges(tweet_df, entity = "media_url", "media"),
      status_entity_edges(tweet_df, entity = "urls_expanded_url", "url")
    ),
    fill = TRUE,
    use.names = TRUE
  )
  
  nodes <- unique(
    rbindlist(
      list(edges[, .(name = from, node_class = source_class)], 
           edges[, .(name = to, node_class = target_class)])
    )
  )
  
  edges[, c("from", "to") := lapply(.SD, match, nodes$name),
        .SD = c("from", "to")]

  list(
    nodes = nodes,
    edges = edges
  )
}

as_kg_igraph <- function(proto_kg) {
  el_cols <- c("from", "to")
  el <- t(as.matrix(proto_kg$edges[, ..el_cols]))

  out <- make_empty_graph(n = nrow(proto_kg$nodes), directed = TRUE)
  out <- add_edges(graph = out, edges = el)
  
  edge_attr(out) <- proto_kg$edges[, !..el_cols]
  vertex_attr(out) <- proto_kg$nodes
  out  
}



set_community <- function(g) {
  vertex_attr(g, "community") <- cluster_louvain(as.undirected(g))$membership
  g
}



# set_graph_appearance <- function(tweet_graph, egos = NULL) {
#   vertex_attr(tweet_graph, "shape") <- "icon"
#   
#   fa_icons <- list(status = "f075",
#                    hashtag = "f292",
#                    url    = "f0c1",
#                    user    = "f007",
#                    media   = "f03e")
#   
#   node_classes <- vertex_attr(tweet_graph, "node_class")
#   
#   vertex_attr(tweet_graph, "icon.code") <- dplyr::case_when(
#     node_classes == "user" ~ fa_icons$user,
#     node_classes == "status" ~ fa_icons$status,
#     node_classes == "url" ~ fa_icons$url,
#     node_classes == "media" ~ fa_icons$media
#   )
#   
#   vertex_attr(tweet_graph, "icon.code") <- dplyr::case_when(
#     node_classes == "user" ~ fa_icons$user,
#     node_classes == "status" ~ fa_icons$status,
#     node_classes == "url" ~ fa_icons$url,
#     node_classes == "media" ~ fa_icons$media
#   )
# 
#   tweet_graph
# }
