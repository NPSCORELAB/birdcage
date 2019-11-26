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
  if (is.data.table(proto_kg)) {
    proto_kg <- build_proto_kg(proto_kg)
  }
  el_cols <- c("from", "to")
  el <- t(as.matrix(proto_kg$edges[, ..el_cols]))

  out <- make_empty_graph(n = nrow(proto_kg$nodes), directed = TRUE)
  out <- add_edges(graph = out, edges = el)
  
  edge_attr(out) <- proto_kg$edges[, !..el_cols]
  vertex_attr(out) <- proto_kg$nodes
  out  
}


entity_df_from_kg <- function(kg) {
  target_nodes <- which(
    vertex_attr(kg, "node_class") %chin% c("hashtag", "url", "media")
  ) 
  
  data.table(
    name = vertex_attr(kg, "name", index = target_nodes),
    node_class = vertex_attr(kg, "node_class", index = target_nodes)
  )
}


set_community <- function(g) {
  vertex_attr(g, "community") <- cluster_louvain(as.undirected(g))$membership
  g
}




extract_ego <- function(tweet_graph, node_name, .order = 10L) {
  target_ego_index <- which(vertex_attr(tweet_graph, "name") == node_name)
  # target_community <- vertex_attr(tweet_graph, "community", 
  #                                 index = target_ego_index)
  # target_nodes <- which(
  #   vertex_attr(tweet_graph, "name") == node_name
  # )
  
  # target_nodes <- target_nodes[
  #   vertex_attr(tweet_graph, target_nodes, "community"
  #                ) == vertex_attr(tweet_graph, name = "community",  
  #                                 index = node_name)
  # ]
  
  hood <- unlist(
    neighborhood(graph = tweet_graph,
                 order = .order,
                 nodes = target_ego_index),
    use.names = FALSE
  )
  # hood_indices <- which(
  #   vertex_attr(tweet_graph, "name") %in% hood
  # )
  # hood_communities <- vertex_attr(tweet_graph, "community", index = hood_indices)
  # 
  # hood <- hood[hood_communities == target_community]
  
  
  out <- induced_subgraph(graph = tweet_graph,
                          vids = hood)
  vertex_attr(out, "title") <- vertex_attr(out, "name")
  
  # attr(out, "edge_type") <- attr(tweet_graph, "edge_type")
  
  out
}






set_graph_appearance <- function(tweet_graph, ego = NULL) {
  vertex_attr(tweet_graph, "shape") <- "icon"

  fa_icons <- list(status = "f075",
                   hashtag = "f292",
                   url    = "f0c1",
                   user    = "f007",
                   media   = "f03e")

  node_classes <- vertex_attr(tweet_graph, "node_class")

  vertex_attr(tweet_graph, "icon.code") <- dplyr::case_when(
    node_classes == "user" ~ fa_icons$user,
    node_classes == "status" ~ fa_icons$status,
    node_classes == "url" ~ fa_icons$url,
    node_classes == "media" ~ fa_icons$media
  )

  vertex_attr(tweet_graph, "icon.color") <- dplyr::case_when(
    node_classes == "user"     ~ "orange",
    node_classes == "status"   ~ "blue",
    node_classes == "hashtag"  ~ "gray",
    node_classes == "url"      ~ "purple",
    node_classes == "media"    ~ "green"
  )
  
  if (!is.null(ego)) {
    ego_index <- which(
      vertex_attr(tweet_graph, "name") == ego
    )
    vertex_attr(tweet_graph, "icon.color", index = ego_index) <- "red"
    vertex_attr(tweet_graph, "icon.size", index = ego_index) <- 100
  }
  
  edge_actions <- edge_attr(tweet_graph, "action")
  edge_attr(tweet_graph, "color") <- dplyr::case_when(
    edge_actions == "posts"    ~ "lightblue",
    edge_actions == "contains" ~ "lightgreen",
    edge_actions == "mentions" ~ "lightgray",
    edge_actions == "retweet"  ~ "salmon",
    edge_actions == "reply_to" ~ "purple",
    edge_actions == "quoted"   ~ "red"
  )
  # edge_attr(tweet_graph, "width") <- "50%"

  tweet_graph
}


add_vis_legend <- function(vis_net, ego_g) {
  lnodes <- get.data.frame(
    ego_g, what = "vertices"
  ) %>% 
    as.data.table() %>% 
    .[, .(label = node_class, icon.code, icon.color)] %>% 
    unique() %>% 
    .[, shape := "icon"]
  
  lnodes[, label := fifelse(icon.color == "red", paste("Selected", label), label)]
  
  ledges <- get.data.frame(
    ego_g, what = "edges"
  ) %>% 
    as.data.table() %>% 
    .[, .(label = action, color)] %>% 
    unique() 
  
  visLegend(vis_net, addNodes = lnodes, addEdges = ledges,ncol = 2,
            zoom = FALSE, useGroups = FALSE)
}



build_vis_net <- function(g, df_with_name, row_selected, name_col) {
  target_ego <- df_with_name[row_selected, ..name_col][[1L]]
  
  ego <- extract_ego(g, target_ego) %>% 
    set_graph_appearance(target_ego)
  
  target_comm <- vertex_attr(ego, "community", 
                             index = which(vertex_attr(ego, "name") == target_ego))
  same_com <- which(
    vertex_attr(ego, "community") == target_comm
  )
  
  out <- induced_subgraph(ego, vids = same_com)
  
  set.seed(831)
  # coords <- layout_with_fr(out, niter = 2000, grid = "nogrid")
  n_pivots <- if (vcount(out) < 50) vcount(out) else 50
  coords <- graphlayouts::layout_with_sparse_stress(out, pivots = n_pivots)
  # coords <- graphlayouts::layout_with_focus(
  #   out, v = which(vertex_attr(ego, "name") == target_ego), 
  # )$xy
  
  out %>% 
    visIgraph(idToLabel = FALSE, layout = "layout.norm", layoutMatrix = coords) %>%
    visOptions(nodesIdSelection = TRUE, selectedBy = "node_class", 
               highlightNearest = TRUE) %>%
    visPhysics(enabled = FALSE) %>% 
    visInteraction(
      tooltipDelay = 500,
      tooltipStyle = "width: 350px; background-color: #ffffff; position:fixed; visibility:hidden;"
    ) %>% 
    add_vis_legend(ego) %>% 
    addFontAwesome(name = "font-awesome-visNetwork")
}
