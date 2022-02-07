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
liberals <- V(blogs_graph)[V(blogs_graph)$value == 0]$name

conservatives <- V(blogs_graph)[V(blogs_graph)$value == 1]$name

l_in_scc <- round((length(intersect(liberals, scc_nodes)) / length(scc)) * 100, digits = 2)
cat("\nProportion of liberal blogs in SCC:",l_in_scc,"%")

c_in_scc <- round((length(intersect(conservatives, scc_nodes)) / length(scc)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in SCC:",c_in_scc,"%")

l_in_component <- round((length(intersect(liberals, in_component)) / length(in_component)) * 100, digits = 2)
cat("\nProportion of liberal blogs in IN component:",l_in_component,"%")

c_in_component <- round((length(intersect(conservatives, in_component)) / length(in_component)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in IN component:",c_in_component,"%")

l_out_component <- round((length(intersect(liberals, out_component)) / length(out_component)) * 100, digits = 2)
cat("\nProportion of liberal blogs in OUT component:",l_out_component,"%")

c_out_component <- round((length(intersect(conservatives, out_component)) / length(out_component)) * 100, digits = 2)
cat("\nProportion of conservatives blogs in OUT component:",c_out_component,"%")

# in_degrees <- degree(blogs_graph, mode = 'in')
# V(blogs_graph)[which(in_degrees==max(in_degrees))]$name
# table(in_degrees)
# pr<- page_rank(blogs_graph,  directed = TRUE, damping = 0.85)$vector

# all conservative vertices having in links from liberals and vice-versa
# all_nodes <- V(blogs_graph)$name
# 
# liberals_to_conservatives <- c()
# for (node in conservatives) {
#   dist <- bfs(blogs_graph, root=node, neimode='in', unreachable=F, dist=T)$dist
#   dist[liberals]
#   liberals_to_conservatives <- c(liberals_to_conservatives, c(node, dist[liberals]))
#   # if(path_exists(blogs_graph, node, liberals, mode = 'in')) {
#   #   liberals_to_conservatives <- c(liberals_to_conservatives, node)
#   # }
# }
# 

# V(blogs_graph)[V(blogs_graph) %in% incoming_for_155]
# + 4/1490 vertices, named, from 39e9191:
#   [1] 100monkeystyping.com       12thharmonic.com/wordpress 40ozblog.blogspot.com     
# [4] 4lina.tblog.com           
# > V(blogs_graph)[V(blogs_graph) %in% incoming_for_155]
# + 4/1490 vertices, named, from 39e9191:
#   [1] 100monkeystyping.com       12thharmonic.com/wordpress 40ozblog.blogspot.com     
# [4] 4lina.tblog.com           
# > temp<-V(blogs_graph)[V(blogs_graph) %in% incoming_for_155]
# > temp
# + 4/1490 vertices, named, from 39e9191:
#   [1] 100monkeystyping.com       12thharmonic.com/wordpress 40ozblog.blogspot.com     
# [4] 4lina.tblog.com           
# > temp$value
# [1] 0 0 0 0

# conservatives_to_liberals <- c()
# for (node in liberals) {
#   if(path_exists(blogs_graph, node, conservatives, mode = 'in')) {
#     conservatives_to_liberals <- c(conservatives_to_liberals, node)
#   }
# }
# use ego
### 3 most conservative influential sites from liberals perspective
conservative_neighbours <- list()
for (node in conservatives) {
  alters <- neighbors(blogs_graph, node, mode = c("in"))
  l_alters <- intersect(V(blogs_graph)[V(blogs_graph) %in% alters]$name, liberals)
  conservative_neighbours[[node]] <- length(l_alters)
}
conservative_neighbours <- conservative_neighbours[conservative_neighbours != 0]
conservative_neighbours[order(unlist(conservative_neighbours), decreasing=TRUE)][1:3]

### 3 most conservative influential sites from liberals perspective
liberals_neighbours <- list()
for (node in liberals) {
  alters <- neighbors(blogs_graph, node, mode = c("in"))
  c_alters <- intersect(V(blogs_graph)[V(blogs_graph) %in% alters]$name, liberals)
  liberals_neighbours[[node]] <- length(c_alters)
}
liberals_neighbours <- liberals_neighbours[liberals_neighbours != 0]
liberals_neighbours[order(unlist(liberals_neighbours), decreasing=TRUE)][1:3]










