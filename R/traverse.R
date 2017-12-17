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