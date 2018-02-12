packages <- c('dplyr', 'tidyr', 'stringr', 'igraph')
sapply(packages, library, character.only = T)

#' Create a standardized tibble of nodes
#' 
#' @param vect a vector of unique names for the nodes
#' @param type the type of nodes. This can be used for filtering
#' @param color what color should this node have in plots?
#' @export
create_nodes <- function(vect, type, color){
  tibble(
    name = vect %>% unique,
    type = type,
    color = color
  )%>%
    filter(!is.na(name))
}

#' Find links attached to a node or multiple nodes of interest
#' 
#' @param nodes a string vector of nodes
#' @param links a master list of links to filter nodes from
#' @param direction c('both', 'in', 'out') filter the links down by direction. 'in' edges only include target nodes and 'out' edges only include source nodes
#' @param type filter links by type
#' @export
find_links <- function(nodes, links, direction = 'both', type = NA){
  if(!is.na(type)){
    links <- links %>%
      filter(type == type)
  }
  
  if(direction == 'both'){
    links <- links %>%
      filter(
        source %in% nodes | target %in% nodes
      )
  } else if(direction == 'in'){
    links <- links %>%
      filter(
        target %in% nodes
      )
  } else if(direction == 'out'){
    links <- links %>%
      filter(
        source %in% nodes
      )
  }
  
  links
}

#' Pull nodes out from a list of sublinks
#' 
#' @param nodes a master list of nodes and their attributes
#' @param links a list of sublinks
#' @param type filter nodes down by type
#' @export
find_nodes <- function(nodes, links, type = NA){
  if(!is.na(type)){
    nodes <- nodes %>%
      filter(type == type)
  }
  
  linked <- c(
    links$source,
    links$target
  ) %>%
    unique()
  
  nodes %>%
    filter(name %in% linked) %>%
    distinct()
}

#' Find links of greater than 1 depth
#' 
#' @param nodes string vector of node or nodes to start search
#' @param links master list of links to search connections from
#' @param direction c('both', 'in', 'out') only find links of a particular direction
#' @param type only find links of a particular type
#' @export
find_links_n <- function(nodes, links, n, direction = 'both', type = NA){
  if(!is.na(type)){
    links <- links %>%
      filter(type == type)
  }
  
  sapply(seq_along(1:n), function(i){
    if(i == 1){
      links_query <<- find_links(nodes = nodes, 
                                links = links,
                                direction = direction, 
                                type = type)
    } else {
      links_query <<- find_links(c(links_query$source, links_query$target) %>% unique(),
                                links = links,
                                direction = direction,
                                type = type)
    }
  })
  
  links_query %>%
    distinct()
}

#'Create igraph subnetwork
#'
#'@param subLinks a list of sublinks forming the network
#'@param nodes a master node list to pull information for the subnetwork
#'@export
create_network <- function(subLinks, nodes){
  subNodes <- find_nodes(nodes, 
                         subLinks)
  
  subNet <- igraph::graph_from_data_frame(subLinks, 
                                          F,
                                          subNodes)
}

#'Find a first layer difference between two nodes
#'
#'@param wantNodes a string vector of nodes of interest
#'@param notNodes a string vector of nodes whose first order connections we should not include
#'@param links a master list of links to pull information from
#'@export
different_first_layer <- function(wantNodes, notNodes, links){
  wantLinks <- find_links(wantNodes, links)
  notLinks <- find_links(notNodes, links)
  query <- find_links_n(wantNodes, links, 2)
  query %>% 
    filter(!source %in% (c(notLinks$target, notLinks$source) %>% unique),
           !target %in% (c(notLinks$target, notLinks$source) %>% unique))
}

#' How similiar are two disconnected nodes
#' 
#' @param query string vector of node or nodes of interest
#' @param links a master list of links to pull information from
#' @export
get_secondary_link_count <- function(query, links){
  query_connections <- find_links(query, links)
  
  secondary_links <- find_links_n(query, links, 2) %>%
    anti_join(query_connections)
  
  secondary_nodes <- c(secondary_links$source, secondary_links$target) %>% unique
  
  
  secondary_link_counts <- tibble(
    node = secondary_nodes,
    connections = sapply(secondary_nodes, function(x){
      secondary_links %>%
        filter(
          (target == x & source %in% secondary_nodes) |
            (target %in% secondary_nodes & source ==x)
        ) %>%
        nrow()
    })) %>%
    arrange(desc(connections))
  
  secondary_link_counts %>%
    filter(!node %in% c(query_connections$source, query_connections$target))
}

#'same as induced_subgraph() but with the first input being a vertex vector
#'
#'@param nodes a vector of nodes or node names
#'@param graph the igraph to filter
#'@export
reverse_induced_subgraph <- function(nodes, graph){
  induced_subgraph(graph = graph, vids = nodes)
}

#' same as subgraph.edges() but with the first input being an edge vector
#'
#'@param edges a vector of edges
#'@param graph the graph to filter
#'@param delete.vertices boolean fo whether or not to delete unconnected vertices
#'@export
reverse_subgraph_edges <- function(edges, graph, delete.vertices = T){
  subgraph.edges(graph = graph, eids = edges, delete.vertices = delete.vertices)
}

#'same as ego() but with the first input being a vertex vector
#'
#'@param nodes vertex of nodes of interest
#'@param graph to find nodes of interest
#'@param order retain nodes within n-ordered distance of nodes of interest
#'@param mode which direction of links to follow
#'@export
reverse_ego <- function(nodes, graph, order = 1, mode = 'all' ){
  ego(graph = graph, order = order, nodes = nodes, mode = mode) 
}

#'same as degree() but with the first input being vertex vector
#'
#'@param v vector of vertices
#'@param graph an igraph to analyze
#'@param mode direction of edges to calculate degree
#'@export
reverse_degree <- function(v, graph, mode = 'all'){
  degree(graph = graph, v = v, mode = mode)
}

#'same as head_of() but with the first input being an edge vector
#'
#'@param es vector of edges
#'@param graph an igraph to analyze
#'@export
reverse_head_of <- function(es, graph){
  head_of(graph, es)
}

#'same as tail_of() but with the first input being an edge vector
#'
#'@param es vector of edges
#'@param graph an igraph to analyze
#'@export
reverse_tail_of <- function(es, graph){
  tail_of(graph, es)
}

#'An imitation of Gremlin's out() and in() functions
#'
#'@parem vertice a vector of vertices or a single vertex
#'@direction "in' or "out" going vertices
#'@graph igraph that the vertices are found in
#'@edge_req a string query to filter the edges by. Normally a boolean for "label" or "name"
#'@export
move <- function(vertices, direction, graph, edge_req = NULL){
  tempGraph <- reverse_ego(names(vertices), graph, mode = direction) %>%
    unlist %>%
    names() %>%
    unique %>%
    reverse_induced_subgraph(graph)
  
  if(direction == "in"){
    tempEdges <- tempGraph %>% 
      E() %>% 
      .[V(tempGraph) %>% .[names(.) %in% names(vertices)] %<-% V(tempGraph)] 
  } else {
    tempEdges <- tempGraph %>% 
      E() %>% 
      .[V(tempGraph) %>% .[names(.) %in% names(vertices)] %->% V(tempGraph)] 
  }
  
  
  if(!is.null(edge_req)){
    tempEdges <- tempEdges %>%
      .[eval(parse(text = edge_req))]
  }
  
  if(direction == "in"){
    tail_of(tempGraph, tempEdges) %>%
      names() %>%
      reverse_induced_subgraph(graph) %>%
      V()
  } else {
    head_of(tempGraph, tempEdges) %>%
      names() %>%
      reverse_induced_subgraph(graph) %>%
      V()
  }
}

#'saves an object to the global environment to be referenced later does not change the input object and the pipe chain can continue
#'
#' @param x an object to save
#' @param name the variable name to save the object o
#' @export
save_as <- function(x, name){
  assign(name, x, envir=globalenv())
  x
}

#'changes the object being piped to a different object
#'
#'@param x the object to transition from
#'@param y the object to transition to 
#'@export
smooth_transition <- function(x, y){
  y
}