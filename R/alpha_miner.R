require(DiagrammeR)
require(dplyr)
require(tidyr)
require(plyr)
require(bupaR)

alpha_miner <- function(log, freq = NULL){
  names(log)[names(log)==case_id(log)] <- "case"
  log <- set_case_id(log, "case")
  names(log)[names(log)==activity_id(log)] <- "activity"
  log <- set_activity_id(log, "activity")
  ### Places
  if(!is.null(freq)){
    map <- log %>% process_map(type_edges = frequency("absolute"),
                               render = F)
    nodes <- map$nodes_df
    edges <- map$edges_df
    edges$from <- plyr::mapvalues(edges$from,
                                  from = nodes$id,
                                  to = gsub("\n.*","", nodes$label))
    edges$to <- plyr::mapvalues(edges$to,
                                from = nodes$id,
                                to = gsub("\n.*","", nodes$label))
    edges$freq <- as.numeric(edges$label) / sum(as.numeric(edges$label))
    edges <- edges[!edges$from %in% c("Start","End") &
                     !edges$to %in% c("Start","End"),]
    edges <- edges[edges$freq > freq,]
    tr <- edges[,c("from","to")]
  }else{
    freq_table <- log %>% traces()
    tr <- list()
    for(i in 1:nrow(freq_table)){
      x <- unlist(strsplit(freq_table$trace[i], split = ","))
      n <- length(x)
      m <- t(mapply(function(a,b) c(a,b) ,x[-n],x[-1]))
      if(n < 2){next}
      tr[i] <- list(m)
    }
    tr <- do.call(rbind, tr)
    tr <- as.data.frame(tr) %>% group_by(V1,V2) %>% dplyr::summarise(n=n())
    tr$prop <- tr$n / sum(tr$n)
    names(tr)[1:2] <- c("from","to")
  }

    ### Footprint matrix
  footprint <- matrix("#", nrow = length(unique(as.vector(as.matrix(tr[,1:2])))), 
                           ncol = length(unique(as.vector(as.matrix(tr[,1:2])))))
  colnames(footprint) <- sort(unique(as.vector(as.matrix(tr[,1:2]))))
  rownames(footprint) <- sort(unique(as.vector(as.matrix(tr[,1:2]))))
  ## Direct Succession
  for(i in 1:nrow(tr)){
    footprint[colnames(footprint) == as.character(tr[i,1]),
              rownames(footprint) == as.character(tr[i,2])] <- "->"
    footprint[rownames(footprint) == as.character(tr[i,2]),
              colnames(footprint) == as.character(tr[i,1])] <- "<-"
  }
  ## Parallel
  pa <- rbind(as.matrix(tr[duplicated(t(apply(tr[,1:2], 1, sort))),1:2]),
        as.matrix(tr[duplicated(t(apply(tr[,2:1], 1, sort))),2:1]))
  if(nrow(pa) > 0){
    for(i in 1:nrow(pa)){
      footprint[colnames(footprint) == pa[i,1],
                rownames(footprint) == pa[i,2]] <- "||"
      footprint[rownames(footprint) == pa[i,2],
                colnames(footprint) == pa[i,1]] <- "||"
    }
  }
  ### Initial
  init <- rownames(footprint)[apply(apply(footprint,1, function(x) x == "<-" |
                                            x == "||" | x == "#"),
                                    1, function(x) all(x == TRUE))]
  ### Terminal
  # ter <- rownames(footprint)[which.max(apply(footprint,1, function(x) sum(x == "<-")))]
  ter <- rownames(footprint)[apply(apply(footprint,1, function(x) x == "->" |
                                            x == "||" | x == "#"),
                                    1, function(x) all(x == TRUE))]
  ### Archs
  ## Splits
  splits_from <- apply(footprint, 1, function(x) length(which(x == "->")))
  splits_from[splits_from < 2] <- 0
  splits_from <- rep(names(splits_from), splits_from)
  splits_to <- c()
  for(i in seq(unique(splits_from))){
    splits_to <- c(splits_to,names(which(footprint[,unique(splits_from)[i]] == "<-")))
  }
  splits_type <- c()
  splits_df <- cbind.data.frame(splits_from, splits_to)
  for(i in 1:length(unique(splits_from))){
    splits <- splits_df[splits_df$splits_from == unique(splits_from)[i],]
    tos <- unique(splits$splits_to)
    splits_type <- c(splits_type,
                     ifelse(apply(footprint[tos,tos] == "||", 1, function(x) sum(x)) > 0,
                            "parallel","choice")) 

  }
  splits_df <- cbind.data.frame(splits_df,splits_type)
  splits_df$parallel <- NA
  for(i in 1:nrow(splits_df)){
    if(splits_df$splits_type[i] == "parallel"){
      splits_df$parallel[i] <- paste(names(which(footprint[,splits_df$splits_to[i]] == "||")),
                                     collapse = " | ")
    }
  }
  nodes_split <- data.frame() ; idn <- 1
  edges_split <- data.frame()
  for(i in 1:length(unique(splits_from))){
    n_archs <- length(unique(splits_df$splits_type[
      splits_df$splits_from == unique(splits_from)[i]]))
    nodes_s <- data.frame(
                     id = paste0("split_",idn:(idn-1+n_archs)),
                     label = FALSE,
                     type = ifelse(unique(cbind(splits_df[
                       splits_df$splits_from == unique(splits_from)[i],
                       c(1:3)])$splits_type) == "parallel",
                       "split_and", "split_xor"),
                     style = "filled",
                     color = "black",
                     fillcolor = "lightblue",
                     shape = "circle",
                     type2 = NA
                  )
    nodes_s$type2 <- ifelse(nodes_s$type == "split_and",
                                "parallel","choice")
    archs_to_nodes <- ifelse(splits_df$splits_type[splits_df$splits_from == 
                                                     unique(splits_from)[i]] == "parallel",
                             nodes_s$id[nodes_s$type2 == "parallel"],
                             nodes_s$id[nodes_s$type2 == "choice"])
    
    nodes_to_archs <- c(splits_df$splits_to[splits_df$splits_from == 
                                                   unique(splits_from)[i] &
                                                   splits_df$splits_type == "parallel"],
                             splits_df$splits_to[splits_df$splits_from == 
                                                   unique(splits_from)[i] &
                                                   splits_df$splits_type == "choice"])
    # nodes_to_archs <- ifelse(splits_df$splits_type[splits_df$splits_from == 
    #                                                  unique(splits_from)[i]] == "parallel",
    #                          splits_df$splits_to[splits_df$splits_from == 
    #                                                 unique(splits_from)[i] &
    #                                                splits_df$splits_type == "parallel"],
    #                          splits_df$splits_to[splits_df$splits_from == 
    #                                                unique(splits_from)[i] &
    #                                                splits_df$splits_type == "choice"])
    edges_split <- rbind.data.frame(edges_split,
                   data.frame(
                     from = c(rep(unique(splits_from)[i],length(nodes_s$id)),
                              archs_to_nodes), 
                     to = c(nodes_s$id,
                            nodes_to_archs),
                     rel = ""
                   ))
    nodes_split <- rbind.data.frame(nodes_split, nodes_s)
    idn <- idn + n_archs
  }
  nodes_split <- rbind.data.frame(
    nodes_split,
    data.frame(
      id = unique(as.vector(as.matrix(splits_df[,1:2]))),
      label = FALSE,
      type = "activity",
      style = "filled",
      color = "black",
      fillcolor = "white",
      shape = "rectangle",
      type2 = NA
    )
  )

  ## Joins
  joins_to <- apply(footprint, 1, function(x) length(which(x == "<-")))
  joins_to[joins_to < 2] <- 0
  joins_to <- rep(names(joins_to), joins_to)
  joins_from <- c()
  for(i in seq(unique(joins_to))){
    joins_from <- c(joins_from,names(which(footprint[,unique(joins_to)[i]] == "->")))
  }
  joins_type <- c()
  joins_df <- cbind.data.frame(joins_from, joins_to)
  for(i in 1:length(unique(joins_to))){
    joins <- joins_df[joins_df$joins_to == unique(joins_to)[i],]
    tos <- unique(joins$joins_from)
    joins_type <- c(joins_type,
                     ifelse(apply(footprint[tos,tos] == "||", 1, function(x) sum(x))==1,
                            "parallel","choice")) 
    
  }
  joins_df <- cbind.data.frame(joins_df,joins_type)
  joins_df$parallel <- NA
  for(i in 1:nrow(joins_df)){
    if(joins_df$joins_type[i] == "parallel"){
      joins_df$parallel[i] <- paste(names(which(footprint[,joins_df$joins_to[i]] == "||")),
                                     collapse = " | ")
    }
  }
  nodes_joins <- data.frame() ; idn <- 1
  edges_joins <- data.frame()
  for(i in 1:length(unique(joins_to))){
    n_archs <- length(unique(joins_df$joins_type[
      joins_df$joins_to == unique(joins_to)[i]]))
    nodes_j <- data.frame(
      id = paste0("join_",idn:(idn-1+n_archs)),
      label = FALSE,
      type = ifelse(unique(cbind(joins_df[
        joins_df$joins_to == unique(joins_to)[i],
        c(1:3)])$joins_type) == "parallel",
        "join_and", "join_xor"),
      style = "filled",
      color = "black",
      fillcolor = "lightblue",
      shape = "circle",
      type2 = NA
    )
    nodes_j$type2 <- ifelse(nodes_j$type == "join_and",
                            "parallel","choice")
    archs_to_nodes <- ifelse(joins_df$joins_type[joins_df$joins_to == 
                                                     unique(joins_to)[i]] == "parallel",
                             nodes_j$id[nodes_j$type2 == "parallel"],
                             nodes_j$id[nodes_j$type2 == "choice"])
    nodes_to_archs <- c(joins_df$joins_from[joins_df$joins_to == 
                                                   unique(joins_to)[i] &
                                                   joins_df$joins_type == "parallel"],
                             joins_df$joins_from[joins_df$joins_to == 
                                                   unique(joins_to)[i] &
                                                   joins_df$joins_type == "choice"])
    # nodes_to_archs <- ifelse(joins_df$joins_type[joins_df$joins_to == 
    #                                                  unique(joins_to)[i]] == "parallel",
    #                          joins_df$joins_from[joins_df$joins_to == 
    #                                                unique(joins_to)[i] &
    #                                                joins_df$joins_type == "parallel"],
    #                          joins_df$joins_from[joins_df$joins_to == 
    #                                                unique(joins_to)[i] &
    #                                                joins_df$joins_type == "choice"])
    edges_joins <- rbind.data.frame(edges_joins,
                                    data.frame(
                                      from = c(nodes_j$id,
                                             nodes_to_archs),
                                      to = c(rep(unique(joins_to)[i],length(nodes_j$id)),
                                               archs_to_nodes), 
                                      rel = ""
                                    ))
    nodes_joins <- rbind.data.frame(nodes_joins, nodes_j)
    idn <- idn + n_archs
  }
  
  ## Sequences
  splits <- unique(splits_df$splits_from)
  joins <- unique(joins_df$joins_from)
  direct_from <- apply(footprint, 1, 
                       function(x) length(which(x == "->")) == 1)
  direct_from <- names(direct_from[which(!names(direct_from) %in% c(splits,joins,ter))])
  direct_to <- tr$to[tr$from %in% direct_from]
  direct_df <- cbind.data.frame(direct_from, direct_to)
  if(nrow(direct_df)>0){
    direct_archs <- paste0("arch_",1:nrow(direct_df)) 
    nodes_direct <- data.frame(
      id = direct_archs,
      label = FALSE,
      type = "direct",
      style = "filled",
      color = "black",
      fillcolor = "lightblue",
      shape = "circle",
      type2 = NA
    )
  }else{
    nodes_direct <- data.frame(
                     id = c("Início","Fim"),
                     label = FALSE,
                     type = "terminal",
                     style = "filled",
                     color = "black",
                     fillcolor = "lightblue",
                     shape = "circle",
                     type2 = NA
                    )
  }
  nodes_direct <- rbind.data.frame(nodes_direct,
                    data.frame(
                      id = c("Início","Fim"),
                      label = FALSE,
                      type = "terminal",
                      style = "filled",
                      color = "black",
                      fillcolor = "lightblue",
                      shape = "circle",
                      type2 = NA
                    ))
  if(nrow(direct_df)>0){
    edges_direct <- data.frame(
      from = c(direct_df$direct_from, direct_archs),
      to = c(direct_archs,direct_df$direct_to), 
      rel = ""
    )
  }else{
    edges_direct <- data.frame(
      from = NA,
      to = NA, 
      rel = NA
    )
  }
  edges_direct <- rbind.data.frame(edges_direct,
                    data.frame(
                      from = c(rep("Início",length(init)),ter),
                      to = c(init,rep("Fim",length(ter))),
                      rel = ""
                    ))
  edges_direct <- edges_direct[complete.cases(edges_direct),]
  ### All together (Start and End)
  nodes_graph <- rbind.data.frame(nodes_split,nodes_joins,nodes_direct)
  edges_graph <- rbind.data.frame(edges_split,edges_joins,edges_direct)
  nodes_graph <- unique(rbind.data.frame(nodes_graph,
                      data.frame(
                        id = unique(as.vector(as.matrix(tr[,1:2]))),
                        label = FALSE,
                        type = "activity",
                        style = "filled",
                        color = "black",
                        fillcolor = "white",
                        shape = "rectangle",
                        type2 = NA
                      )))
  ### Verificar se já existem joins para um split prévio
  nodes_graph$label <- nodes_graph$id
  nodes_graph$id <- seq_along(nodes_graph$id)
  edges_graph$from <- as.integer(mapvalues(edges_graph$from,
                                           from = nodes_graph$label,
                                           to = nodes_graph$id))
  edges_graph$to <- as.integer(mapvalues(edges_graph$to,
                                         from = nodes_graph$label,
                                         to = nodes_graph$id))
  nodes_graph$label[grepl("xor", nodes_graph$type)] <- "OR"
  nodes_graph$label[grepl("and", nodes_graph$type)] <- "AND"
  nodes_graph$label[grepl("arch", nodes_graph$label)] <- ""
  graph <- create_graph(nodes_df = nodes_graph,
                        edges_df = edges_graph,
                        attr_theme = "lr")
  graph <- graph %>%
    set_node_attrs(
      node_attr = fontname,
      values = "Helvetica"
    ) %>%
    set_node_attrs(
      node_attr = fontcolor,
      values = "black"
    ) %>%
    set_node_attrs(
      node_attr = fixedsize,
      values = "false"
    )
  graph <- graph %>%
    set_edge_attrs(
      edge_attr = penwidth,
      values = 2
    ) %>%
    set_edge_attrs(
      edge_attr = color,
      values = "black"
    )
  return(DiagrammeR::generate_dot(graph)  %>% 
   gsub(pattern = 'neato',replacement = 'tree',x= .) %>%
   gsub(pattern = "graph \\[",'graph \\[rankdir = LR,\n',x = .)%>%
   grViz)
}
