# Graph analysis using iGraph.
# Modified from book "Web Analysis and Mining With R" by LI MING (ISBN 978-7-111-45971-2).
# Needs package "igraph".
# install.packages("igraph")

library(igraph)

# Constructing a simple graph.
from <- c("a","a","e","b","b","c","d","d","d","f")
to <- c("c","e","c","e","c","d","g","g","f","d")
data <- data.frame(from = from, to = to)

# This function is used to initialize a iGraph.

init.igraph <- function(data, dire = F, remove.multiple = T){
  
  # Get the mapping from vertices to identifiers.
  labels <- union(unique(data[,1]), unique(data[,2]))
  ids <- 1:length(labels)
  names(ids) <- labels

  # From dataset of vertex names to identifiers.
  from <- as.character(data[,1])
  to <- as.character(data[,2])
  edges <- matrix(c(ids[from], ids[to]), nc = 2)

  # Constructing the graph.
  g <- graph.empty(directed = dire)
  g <- add.vertices(g, length(labels))
  V(g)$label = labels
  g <- add.edges(g, t(edges))
  if(remove.multiple){
    E(g)$weight <- count.multiple(g)
    g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "mean")
  }
  g
}

# Initialize the graph use the function defined above.
g.undir <- init.igraph(data)
g.dir <- init.igraph(data, dire = TRUE)

# Plot the graph.
par(mfcol = c(1,2))
plot(g.undir, edge.width = E(g.undir)$weight, main="Undirected", edge.label = E(g.undir)$weight)
plot(g.dir, edge.width = E(g.undir)$weight, main="Directed", edge.label = E(g.undir)$weight)

# Discovering communities (with modularity) and plot out.
# Similarly, one may use "walktrap.community()", "edge.betweenness.community()", "label.propagation.community()".
V(g.undir)$membership <- spinglass.community(g.undir)$membership
modularity.membership <- modularity(g.undir, membership = V(g.undir)$membership)
member.color <- rainbow(length(unique(V(g.undir)$membership)), alpha = 0.3)
V(g.undir)$color <- member.color[V(g.undir)$membership]
plot(g.undir, edge.width = E(g.undir)$weight, vertex.color = V(g.undir)$color)

# Discovering the neighbors of a vertex.
neighbor <- V(g.undir)$label[neighbors(g.undir, v = which(V(g.undir)$label == "d"))]

# Discovering the degree of a vertex.
d <- length(neighbors(g.undir, v = which(V(g.undir)$label == "d")))

# Get all the degrees of the vertices within the graph using iGraph.
# mode = "in" ,"out", "total"
vertices.degree <- degree(g.undir, mode = "total")

# Discovering the shortest paths.
s.path <- shortest.paths(g.undir)

# Discovering closeness, betweenness, and eigenvector centrality.
clo <- closeness(g.undir, mode = "total", normalized = T)
bet <- betweenness(g.undir, normalized = T)
edge.bet <- edge.betweenness(g.undir)
eigen.centrality <- evcent(g.undir, scale = F)$vector