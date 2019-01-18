library(igraph)
nodes <- data.frame(name=paste0("N",1:8),
                    supply=c(10,20,0,-5,0,0,-15,-10))

edges <- data.frame(nodefrom = paste0("N",c( 1, 2, 2, 2, 3, 3, 4, 5, 5, 6, 7)),
                    nodeto   = paste0("N",c( 4, 1, 3, 6, 4, 5, 7, 6, 7, 8, 8)),
                    cost     = c( 2, 1, 0, 6, 1, 4, 5, 2, 7, 8, 9),
                    capacity = c(15,10,10,10, 5,10,10,20,15,10,15),
                    name     = paste0("E",1:11))
G <- graph.data.frame(edges)
V(G)$supply <- nodes$supply[match(V(G)$name,nodes$name)]
# plot the graph
set.seed(3)
par(mar=c(0,0,0,0))
plot(G, vertex.size=30,
     vertex.label=paste0(V(G)$name,' (',V(G)$supply,')'),
     vertex.color='lightblue', edge.arrow.size=0.5,
     edge.label=paste0(E(G)$name,' (',E(G)$cost,',',E(G)$capacity,')')
)



library(Rsymphony)
library(slam)

nVars <- ecount(G)
obj <- E(G)$cost
bounds <- list(upper=list(1:nVars,E(G)$capacity),lower=list(1:nVars,rep(0,nVars)))
types <- rep('C',ecount(G))

mat <- simple_triplet_zero_matrix(nrow=nrow(nodes),ncol=nrow(edges))
colnames(mat) <- E(G)$name
rownames(mat) <- V(G)$name
rhs <- -V(G)$supply
dir <- rep('==',vcount(G))
for(v in V(G)){
  outEdges <- E(G)[from(v)]$name
  inEdges <- E(G)[to(v)]$name
  mat[v,match(inEdges,colnames(mat))] <- 1
  mat[v,match(outEdges,colnames(mat))] <- -1
}

output <- Rsymphony_solve_LP(obj=obj,
                             mat=mat,
                             dir=dir,
                             rhs=rhs,
                             bounds=bounds,
                             types=types,
                             max=FALSE,
                             write_lp = TRUE)

# plot the solution
set.seed(3)
par(mar=c(0,0,0,0))
plot(G, vertex.size=30,
     vertex.label=paste0(V(G)$name,' (',V(G)$supply,')'),
     vertex.color='lightblue', edge.arrow.size=0.5,
     edge.label=paste0(E(G)$name,' flow = ',output$solution))