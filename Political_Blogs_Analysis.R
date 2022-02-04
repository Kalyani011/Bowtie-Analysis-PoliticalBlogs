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
cat("\nSCC Size:", length(scc_nodes))

# Function to detect paths between nodes
path_exists <- function(g, start_node, finish_nodes, mode){
  dist <- bfs(g, root=start_node, neimode=mode, unreachable=F, dist=T)$dist
  return (any(!is.nan(dist[finish_nodes])))
}

# Finding in component size
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
cat("\nIn Component Size:", length(in_component))

# Finding out component size
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
cat("\nOut Component Size:", length(out_component))

# Finding tube component size

other_nodes <- non_scc_nodes[! non_scc_nodes %in% in_component]
other_nodes <- other_nodes[!other_nodes %in% out_component]

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

tube_out <- c()
for (node in partial_tube) {
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

cat("\nTube Component Size:", length(tube_component))


# Finding liberal and conservative percentage
liberals <- V(blogs_graph)[V(blogs_graph)$value == 0]
conservatives <- V(blogs_graph)[V(blogs_graph)$value == 1]

l_in_scc <- round((length(intersect(liberals, scc)) / length(scc)) * 100, digits = 2)
cat("\nProportion of liberal blogs in SCC:",l_in_scc,"%")

c_in_scc <- round((length(intersect(conservatives, scc)) / length(scc)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in SCC:",c_in_scc,"%")

l_in_component <- round((length(intersect(liberals, in_component)) / length(in_component)) * 100, digits = 2)
cat("\nProportion of liberal blogs in IN component:",l_in_component,"%")

c_in_component <- round((length(intersect(conservatives, in_component)) / length(in_component)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in IN component:",c_in_component,"%")

l_out_component <- round((length(intersect(liberals, out_component)) / length(out_component)) * 100, digits = 2)
cat("\nProportion of liberal blogs in OUT component:",l_out_component,"%")

c_out_component <- round((length(intersect(conservatives, out_component)) / length(out_component)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in OUT component:",c_out_component,"%")
