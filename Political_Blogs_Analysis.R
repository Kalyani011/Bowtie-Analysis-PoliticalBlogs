library(igraph)

# Reading graph from gml file
blogs_graph <- read_graph("polblogs.gml", format = "gml")

# Adding names for nodes
V(blogs_graph)$name <- V(blogs_graph)$label

# Getting components from graph
strong_components <- components(blogs_graph, mode = "strong")

# Finding the nodes in largest cluster of connected components
scc <- which(strong_components$membership == which.max(strong_components$csize))
# Finding the nodes not in scc
not_in_scc <- which(strong_components$membership != which.max(strong_components$csize))

non_scc_nodes <- which(V(blogs_graph)$id %in% not_in_scc)
scc_nodes<- which(V(blogs_graph)$id %in% scc)

path_exists <- function(g, start_node, finish_nodes, mode){
  dist <- bfs(g, root=start_node, neimode=mode, unreachable=F, dist=T)$dist
  return (any(!is.nan(dist[finish_nodes])))
}

# in
in_scc <- c()
for (node in non_scc_nodes) {
  if(path_exists(blogs_graph, node, scc_nodes, mode = 'out')) {
    in_scc <- c(in_scc, node)
  }
}

in_component <- c()
for (node in in_scc) {
  if(!path_exists(blogs_graph, node, scc_nodes, mode = 'in')){
    in_component <- c(in_component, node)
  }
}

# out
out_scc <- c()
for (node in non_scc_nodes) {
  if(path_exists(blogs_graph, node, scc_nodes, mode = 'in')) {
    out_scc <- c(out_scc, node)
  }
}

out_component <- c()
for (node in out_scc) {
  if(!path_exists(blogs_graph, node, scc_nodes, mode = 'out')){
    out_component <- c(out_component, node)
  }
}

other_nodes <- non_scc_nodes[! non_scc_nodes %in% in_component]
other_nodes <- other_nodes[!other_nodes %in% out_component]

# these are in tendrils
in_tube <- c()
for (node in other_nodes) {
  if(path_exists(blogs_graph, node, in_component, mode = 'in')) {
    in_tube <- c(in_tube, node)
  }
}
partial_tube <- c()
for (node in in_tube) {
  if(!path_exists(blogs_graph, node, in_component, mode = 'out')) {
    partial_tube <- c(partial_tube, node)
  }
}
# tendrils in - end

# these are out tendrils
tube_out <- c()
for (node in other_nodes) {
  if(path_exists(blogs_graph, node, out_component, mode = 'out')) {
    tube_out <- c(tube_out, node)
  }
}

tube_component <- c()
for (node in tube_out) {
  if(!path_exists(blogs_graph, node, out_component, mode = 'in')) {
    tube_component <- c(tube_component, node)
  }
}
# tendrils out - end

## These are disconnected nodes
other_nodes2 <- other_nodes[! other_nodes %in% partial_tube]
other_nodes2 <- other_nodes2[!other_nodes2 %in% tube_component]

temp <- c()
for (node in other_nodes2) {
  if(path_exists(blogs_graph, node, out_component, mode = 'in')){
    temp <- c(temp, node)    
  }
}
## These are disconnected nodes - end

