library(igraph)

# Reading graph from gml file
blogs_graph <- read_graph("polblogs.gml", format = "gml")

# Adding names for nodes
V(blogs_graph)$name <- V(blogs_graph)$label

# Getting components from graph
strong_components <- components(blogs_graph, mode = "strong")

# Finding the nodes in largest cluster of connected components
scc <- which(strong_components$membership == which.max(strong_components$csize))
scc_nodes <- V(blogs_graph)[V(blogs_graph)$id %in% scc]$name
cat("\nSCC Size:", length(scc_nodes))

# Finding the nodes not in scc
not_in_scc <- which(strong_components$membership != which.max(strong_components$csize))
non_scc_nodes <- V(blogs_graph)[V(blogs_graph)$id %in% not_in_scc]$name

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
rem_nodes <- non_scc_nodes[!non_scc_nodes %in% c(in_component, out_component)]

possible_tube <- c()
for (node in non_scc_nodes) {
  if(!path_exists(blogs_graph, node, scc_nodes, mode = 'out') 
     && !path_exists(blogs_graph, node, scc_nodes, mode = 'in')) {
    possible_tube <- c(possible_tube, node)
  }
}

in_tube <- c()
for (node in possible_tube) {
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

l_in_scc <- round((length(intersect(liberals$name, scc_nodes)) / length(scc)) * 100, digits = 2)
cat("\nProportion of liberal blogs in SCC:",l_in_scc,"%")

c_in_scc <- round((length(intersect(conservatives$name, scc_nodes)) / length(scc)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in SCC:",c_in_scc,"%")

l_in_component <- round((length(intersect(liberals$name, in_component)) / length(in_component)) * 100, digits = 2)
cat("\nProportion of liberal blogs in IN component:",l_in_component,"%")

c_in_component <- round((length(intersect(conservatives$name, in_component)) / length(in_component)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in IN component:",c_in_component,"%")

l_out_component <- round((length(intersect(liberals$name, out_component)) / length(out_component)) * 100, digits = 2)
cat("\nProportion of liberal blogs in OUT component:",l_out_component,"%")

c_out_component <- round((length(intersect(conservatives$name, out_component)) / length(out_component)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in OUT component:",c_out_component,"%")

### 3 most conservative influential sites from liberals perspective
# conservative_neighbours <- list()
# for (node in conservatives$name) {
#   alters <- neighbors(blogs_graph, node, mode = c("in"))
#   l_alters <- intersect(V(blogs_graph)[V(blogs_graph) %in% alters]$name, liberals$name)
#   count = 0
#   for(alter in l_alters) {
#     temp <- neighbors(blogs_graph, alter, mode = 'all')
#     temp_c <- intersect(V(blogs_graph)[V(blogs_graph) %in% temp]$name, conservatives$name)
#     temp_l <- intersect(V(blogs_graph)[V(blogs_graph) %in% temp]$name, liberals$name)
#     if(length(temp_c) < length(temp_l)){
#       count = count + 1
#     }
#   }
#   conservative_neighbours[[node]] <- count
# }
# conservative_neighbours <- conservative_neighbours[conservative_neighbours != 0]
# influential_c <- conservative_neighbours[order(unlist(conservative_neighbours), decreasing=TRUE)][1:3]
# cat("\nFrom the perspective of liberal bloggers, 3 most influential conservative bloggers are:", toString(names(influential_c)))

### 3 most liberal influential sites from conservative's perspective
# liberal_neighbours <- list()
# for (node in liberals$name) {
#   alters <- neighbors(blogs_graph, node, mode = 'in')
#   c_alters <- intersect(V(blogs_graph)[V(blogs_graph) %in% alters]$name, conservatives$name)
#   count = 0
#   for(alter in c_alters) {
#     temp <- neighbors(blogs_graph, alter, mode = 'all')
#     temp_c <- intersect(V(blogs_graph)[V(blogs_graph) %in% temp]$name, conservatives$name)
#     temp_l <- intersect(V(blogs_graph)[V(blogs_graph) %in% temp]$name, liberals$name)
#     if(length(temp_c) > length(temp_l)){
#       count = count + 1
#     }
#   }
#   liberals_neighbours[[node]] <- count
# }
# liberals_neighbours <- liberals_neighbours[liberals_neighbours != 0]
# influential_l <- liberals_neighbours[order(unlist(liberals_neighbours), decreasing=TRUE)][1:3]
# cat("\nFrom the perspective of conservative bloggers, 3 most influential liberal bloggers are:",
#     toString(names(influential_l)))

## could show page rank, that they all have page rank greater than the average page rank of the entire graph

neutrals <- list()
for (node in conservatives$name) {
  alters <- neighbors(blogs_graph, node, mode = 'all')
  temp <- V(blogs_graph)[V(blogs_graph) %in% alters]$name
  c_alters <- intersect(temp, conservatives$name)
  l_alters <- intersect(temp, liberals$name)
  if(length(l_alters) > length(c_alters)) {
    neutrals[[node]] <- length(l_alters) - length(c_alters)
  }
}
for (node in liberals$name) {
  alters <- neighbors(blogs_graph, node, mode = 'all')
  temp <- V(blogs_graph)[V(blogs_graph) %in% alters]$name
  c_alters <- intersect(temp, conservatives$name)
  l_alters <- intersect(temp, liberals$name)
  if(length(l_alters) < length(c_alters)) {
    neutrals[[node]] <- length(c_alters) - length(l_alters)
  }
}

conservative_authorities <- list()
for (node in conservatives$name) {
  neighbors <- neighbors(blogs_graph, node, mode="in")
  neighbors <- V(blogs_graph)[intersect(neighbors, conservatives)]
  ego_nodes1.5 <- c(node, neighbors$name)
  sub_net <- induced_subgraph(blogs_graph, c(liberals$name, ego_nodes1.5))
  V(sub_net)$authority <- authority_score(sub_net, scale = TRUE)$vector
  conservative_authorities[[node]] <- V(sub_net)[V(sub_net)$name == node]$authority
}
conservative_authorities <- conservative_authorities[conservative_authorities != 0]
influential_c <- conservative_authorities[order(unlist(conservative_authorities), decreasing=TRUE)][1:3]
cat("\nFrom the perspective of liberal bloggers, 3 most influential conservative bloggers are:"
    , toString(names(influential_c)))

liberal_authorities <- list()
for (node in liberals$name) {
  neighbors <- neighbors(blogs_graph, node, mode="in")
  neighbors <- V(blogs_graph)[intersect(neighbors, liberals)]
  ego_nodes1.5 <- c(node, neighbors$name)
  sub_net <- induced_subgraph(blogs_graph, c(conservatives$name, ego_nodes1.5))
  V(sub_net)$authority <- authority_score(sub_net, scale = TRUE)$vector
  liberal_authorities[[node]] <- V(sub_net)[V(sub_net)$name == node]$authority
}
liberal_authorities <- liberal_authorities[liberal_authorities != 0]
influential_l <- liberal_authorities[order(unlist(liberal_authorities), decreasing=TRUE)][1:3]
cat("\nFrom the perspective of conservative bloggers, 3 most influential liberal bloggers are:",
    toString(names(influential_l)))
