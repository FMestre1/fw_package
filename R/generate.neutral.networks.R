#Generate a list of scale free and random networks
generate.neutral.networks <- function(type, nnodes, ninteractions, iter, pow = NULL){
  
  #Arguments
  #type: sf (scale free) and rd (random);
  #nnodes: number of nodes;
  #ninteractions: number of interactions;
  #iter: iterations.
  #pow: to be internally  passed to barabasi.game
  
  result <- list()
  
  #SCALE FREE
  if(type == "sf"){
    
    message(paste0("Computing ", iter," a scale-free networks based upon the Barabasi-Albert algorithm!"))
    
    for(i in 1:iter){
      #this function was also designated by barabasi.game
        #Number of nodes to add at each time step
      #nnodes_add <- rep(round(ninteractions/(nnodes-1)), nnodes)
        #create the graph
      #g <- barabasi.game(n = nnodes, pow, out.seq = nnodes_add, directed = FALSE)
      g <- static.power.law.game(no.of.nodes = nnodes, no.of.edges = ninteractions, exponent.in = -1, exponent.out = pow)
      #plot(g, vertex.label= NA, edge.arrow.size=0.1,vertex.size = 2.5, xlab = "")
      g2 <-as.data.frame(as.matrix(get.adjacency(g)))
      result[[i]] <- g2
      }
  }
  
  #RANDOM
  if(type =="rd"){
    
    message(paste0("Computing ", iter," a random networks based upon the Erdos-Renyi algorithm!"))
    
    for(i in 1:iter){
      
      g <- erdos.renyi.game(n=nnodes, p.or.m=ninteractions, type = "gnm")
      g2 <-as.data.frame(as.matrix(get.adjacency(g)))
      result[[i]] <- g2
      
    }
    
  }
  
  return(result)
}
