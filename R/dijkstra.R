#' Dijkstra 
#' 
#' Find the shortest distance from \code{init_node} to all nodes in \code{graph}.
#' 
#' @param graph, a graph table with all neighbors and the distance to them.
#' @param init_node, a starting point of numeric numbers.
#' @return The shortest distance from \code{init_node} to all nodes in \code{graph}.
#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)  
#' dijkstra(wiki_graph, 3)  
#' 

dijkstra<-function(graph,init_node){
  if(!is.data.frame(graph)| !is.numeric(init_node)) stop("Input is not valid!")
  
  unvisi<-levels(factor(graph[,1])) #all univisited nodes, vector of character 
  len<-length(unvisi)               #number of nodes
  curr<-init_node                   #initiate current node
  
  #initiate nodes with element of route and cost, list
  routenr<-NULL
  for(i in 1:len) routenr<-c(routenr,paste("Route",as.character(i)))  #assign names for routes
  nodes<-rep(list(init_node),6)  #list elements for 6 routes
  names(nodes)<-routenr   #assign names for routes
  nodes<-append(nodes,list(cost=rep(Inf,len)))  #add cost element and start value to list
  nodes$cost[init_node]<-0 #set start value o to source point 

  
  while(!length(unvisi)==0){
    #find next current node
    mi=min(nodes$cost[as.numeric(unvisi)])
    if(mi==Inf) stop("All connected nodes explored!")
    else curr=as.numeric(unvisi)[which(nodes$cost[as.numeric(unvisi)]==mi)][1] #pick any one of the minimum
    #exclude it from unvisited set
    unvisi<-unvisi[!(unvisi==as.character(curr))]
    
    tem<-nodes$cost[curr]+graph[graph$v1==curr,"w"] #possible cost for neighbor nodes
    condition<-which(tem<nodes$cost[graph[graph$v1==curr,"v2"]])
    #update cost for all neigbor nodes
    nodes$cost[  graph[graph$v1==curr,"v2"][ condition ]]=tem[condition]
    #update routes for nodes
    index=graph[graph$v1==curr,"v2"][ condition ] #index for those updated nodes
    for(i in index) nodes[[i]]=c(nodes[[curr]], i)
  }

  return (as.vector(nodes$cost))  #as.vector(nodes[[i]]) to get the correponding route
}